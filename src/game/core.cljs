(ns game.core
    (:require
      [reagent.core :as r]
      [reagent.dom :as d]))


(defn clamp-low [v low]
  (if (< v low) low v))

(defn clamp-high [v high]
  (if (> v high) high v))

(defn clamp [v low high]
  (clamp-high (clamp-low v low) high))

(defn decrease-oxygen [state modifier-fn]
  (println (str "Decrease oxygen by " (modifier-fn 10)))
  (swap! state
         update-in
         [:game :resources :oxygen]
         #(clamp-low (- % (modifier-fn 10)) 0))
  )

(defn increase-hunger [state modifier-fn]
  (println (str "Increase hunger by " (modifier-fn 10)))
  (swap! state update-in [:game :hunger :current] #(clamp (+ % (modifier-fn 10))
                                                          0
                                                          (get-in @state [:game :hunger :max]))))
(defn apply-buildings-effects [state]
  (doseq [[_ b] (filter #(:built (second %)) (get-in @state [:game :buildings]))]
    ((:effect b) state)))

(defn next-hour [state]
  (swap! state update-in [:game :time] inc)
  (let [current-activity (get-in @state
                                 [:game :activities (get-in @state [:game :selected-activity])])
        modifiers ((:effect current-activity) state)
        ;_ (println modifiers)
        ]
    (apply-buildings-effects state)
    (decrease-oxygen state (or (:oxygen modifiers) identity))
    (increase-hunger state (or (:hunger modifiers) identity)))
  )

(defn dead? [state]
  (or
    (not (pos? (get-in @state [:game :resources :oxygen])))
    (>= (get-in @state [:game :hunger :current])
        (get-in @state [:game :hunger :max]))))

(defn bonus-price-double-every-level-fn [base]
  (fn [level]
    (reduce (fn [acc _] (* 2 acc))
            base
            (range 1 (inc level)))) )

(defn can-find-deposits [state]
  (let [depo (get-in @state [:game :deposits])]
    {:stone (zero? (:stone depo))
     :metals (zero? (:metals depo))
     })
  )

(defn exploration-finds [state]
  (let [r (rand)
        has-metal-detector (get-in @state [:game :craftable :metal-detector :crafted])
        depo (can-find-deposits state)
        stone-depo (if (and (> r 0.3) (:stone depo))
                     {:deposits {:stone 1 }}
                     {}
                     )
        metals-depo (if (and (> r 0.7) (:metals depo))
                     {:deposits {:metals 1 }}
                     {}
                     )
        stone (if (> r 0.4)
                (int (* 20 (rand)))
                0)
        metals (if (> r
                      (if has-metal-detector
                        0.5
                        0.7))
                (int (* 10 (rand)))
                0)
        ]
    (merge stone-depo metals-depo {:resources {:stone stone :metals metals}})))

(defn ts [] (.getTime (js/Date.)))

(defn log-event [state evt]
  ;(swap! state update-in [:journal] (fn [j] (cons {:text evt :ts (ts)} j))) 
  (swap! state assoc-in [:journal] [{:text evt :ts (ts)}]))

(defn has-resources? [state resources]
  (let [rsk (keys resources)
        has (select-keys (get-in @state [:game :resources]) rsk)]
    (every? #(not (neg? (second %))) (merge-with - has resources))))

(defn resource-display [p]
  (clojure.string/join " / "(map (fn [[k v]] (str (name k) " : " v)) p)))

(defn price-display [p]
  [:div {:class "price"}
   (str "Price : " (resource-display p))])


(def base-state
  {
     :screen :mission-start ;:game-main ;:home
     :game
     {
      :hunger {:current 0
               :max 30000}
      :resources {
                  :oxygen 1450
                  :stone 16
                  :water 10
                  :metals 6
                  :electronics 0
                  :potatoes 10
                  }
      :deposits {
                 :stone 0
                 :metals 0
                 }
      :equipment {
                  :oxygen-tank {
                                :capacity 2000
                                }}

      :time 0
      :selected-activity :rest
      :activities {
                   :rest
                   {
                    :order 0
                    :label "Rest"
                    :explain "Wait and consume less oxygen, hunger grows slower"
                    :effect (fn [state]
                              {:hunger (fn [decrease] (/ decrease 2))
                               :oxygen (fn [decrease] (/ decrease 2))
                               }
                              )
                    :available true
                    } 
                   :eat
                   {
                    :order 1
                    :label "Eat"
                    :explain "Reduces hunger, consumes 1 potato"
                    :effect (fn [state]
                              (when (pos? (get-in @state [:game :resources :potatoes]))
                                (swap! state update-in [:game :resources :potatoes] #(- % 1))
                                (swap! state update-in [:game :hunger :current] #(- % 20)))
                              )
                    :available true
                    }
                   :explore
                   {
                    :order 2
                    :label "Explore"
                    :explain "Go explore surroundings to find resources or more ..."
                    :effect (fn [state]
                              (let [finds (exploration-finds state)]
                                (println "Found : " finds)
                                (log-event state
                                           (str "Found : "
                                                (resource-display (:resources finds))
                                                (when (get-in finds [:deposits :metals])
                                                  " And a metals deposit !")
                                                (when (get-in finds [:deposits :stone])
                                                  " And a stone deposit !")
                                                ))
                                (swap! state update-in [:game :resources]
                                     (fn [rs] (merge-with + rs (:resources finds))))
                                (swap! state update-in [:game :deposits]
                                       (fn [ds] (merge-with + ds (:deposits finds))))

                                {:hunger (fn [factor] (int (* 1.5 factor)))
                                 :oxygen (fn [factor] (int (* 1.5 factor)))
                                 })
                              )
                    :available true
                    }
                   :mine-stone
                   {
                    :order 3
                    :label "Mine stone"
                    :explain "Mine a stone deposit, high oxygen and hunger"
                    :effect
                    (fn [state]
                      (let [jackhammer-bonus
                            (if (get-in @state [:game :craftable :jackhammer :crafted])
                              #(* 3 %)
                              identity)]
                        (swap! state update-in [:game :resources :stone] #(+ % (jackhammer-bonus 5))))
                      {:hunger (fn [factor] (int (* 1.7 factor)))
                       :oxygen (fn [factor] (int (* 1.7 factor)))
                       })
                    :condition (fn [state] (pos? (get-in @state [:game :deposits :stone])))
                    :available true
                    }
                   :mine-metals
                   {
                    :order 4
                    :label "Mine metals"
                    :explain "Mine a metals deposit, high oxygen and hunger"
                    :effect 
                    (fn [state]
                      (let [jackhammer-bonus
                            (if (get-in @state [:game :craftable :jackhammer :crafted])
                              #(* 3 %)
                              identity)]
                        (swap! state update-in [:game :resources :metals] #(+ % (jackhammer-bonus 5))))
                      {:hunger (fn [factor] (int (* 1.7 factor)))
                       :oxygen (fn [factor] (int (* 1.7 factor)))
                       }) 
                    :condition (fn [state] (pos? (get-in @state [:game :deposits :metals])))
                    :available true
                    }
                   :plant
                   {
                    :order 5
                    :label "Plant potatoes"
                    :explain "Yummy!"
                    :effect (fn [state]
                              (swap! state update-in [:game :buildings :farm :plants] 
                                     (fn [p] (cons {:time-left 5} p))
                                     )
                                {:hunger (fn [factor] (int (* 1.7 factor)))
                                 :oxygen (fn [factor] (int (* 1.7 factor)))
                                 })
                    :condition (fn [state]
                                      (< (count (get-in @state [:game :buildings :farm :plants]))
                                         (get-in @state [:game :buildings :farm :max-plants])))
                    :available true
                    }

                   :harvest
                   {
                    :order 6
                    :label "Harvest potatoes"
                    :explain "Yummy!"
                    :effect (fn [state]
                              (swap! state update-in [:game :resources :potatoes]
                                     #(+ %
                                         (count
                                           (remove
                                             (fn [plant] (pos? (:time-left plant)))
                                             (get-in @state [:game :buildings :farm :plants])))))
                              (swap! state update-in [:game :buildings :farm :plants]
                                     (fn [p] (filter #(pos? (:time-left %)) p)))
                                {:hunger (fn [factor] (int (* 1.7 factor)))
                                 :oxygen (fn [factor] (int (* 1.7 factor)))
                                 })
                    :condition (fn [state]
                                 (some #(not (pos? (:time-left %)))
                                      (get-in @state [:game :buildings :farm :plants])))
                    :available true
                    }

                   :make-electronics
                   {
                    :order 7
                    :label "Make electronics"
                    :explain "a useful component for many things"
                    :price (fn [state] {:stone 4 :metals 2})
                    :effect (fn [state]
                              (let [price ((get-in @state [:game :activities :make-electronics :price]) state)]
                                (swap! state update-in [:game :resources]
                                     #(merge-with - % price)))
                                (swap! state update-in [:game :resources :electronics] inc) 
                                {})
                    :condition (fn [state]
                                 (and
                                   (get-in @state [:game :buildings :craft-bench :built])
                                   (has-resources? state
                                                 ((get-in @state [:game :activities :make-electronics :price]) state))))
                    :available true
                    }

                  :extract-water
                   {
                    :order 8
                    :label "Extract water"
                    :explain "the source of life!"
                    :effect (fn [state]
                                (swap! state update-in [:game :resources :water] #(+ % 10)) 
                                {})
                    :condition (fn [state]
                                 (get-in @state [:game :buildings :water-extractor :built]))
                    :available true
                    }
    
      }
      :buildings {
                  :water-extractor
                  {
                   :label "Water extractor"
                   :price (fn [state] {:stone 30 :metals 10})
                   :built false
                   :condition (fn [state]
                                (has-resources? state {:stone 15 :metals 5}))
                   :effect (fn [state]
                             ;(swap! state update-in [:game :resources :water] #(+ % 10))
                             )
                   }
                  :farm
                  {
                   :label "Potato farm"
                   :price (fn [state] {:stone 30 :metals 5})
                   :built true
                   :condition (fn [state]
                                (has-resources? state {:stone 15 :metals 2}))
                   :effect (fn [state]
                             (println "Apply potato farm")
                             (let [
                                   ;water-needed
                                   ;(count (filter #(pos? (:time-left %)) (get-in @state [:game :buildings :farm :plants])))
                                   water-available (get-in @state [:game :resources :water])

                                   update-plants-fn
                                   (fn [acc p]
                                     (if (and
                                           (pos? (:water acc))
                                           (pos? (:time-left p)))
                                       {:water (dec (:water acc))
                                        :plants (cons (update p :time-left dec) (:plants acc)) }
                                       (update acc
                                               :plants
                                               #(cons p %))
                                       ))

                                   {:keys [water plants]}
                                   (reduce update-plants-fn
                                           {:water water-available :plants []}
                                           (get-in @state [:game :buildings :farm :plants]))
                                     ]
                               (swap! state assoc-in [:game :buildings :farm :plants] plants)
                               (swap! state assoc-in [:game :resources :water] water)
                             ))
                   :max-plants 10
                   :plants []
                   }
                  ;:laboratory {}
                  :craft-bench
                  {
                   :label "Craft bench"
                   :price (fn [state] {:stone 10 :metals 2})
                   :built false
                   :condition (fn [state]
                                (has-resources? state {:stone 5 :metals 1}))
                   :effect (fn [state]
                             ;(println "effect of water xtracv")
                             ;(swap! state update-in [:game :resources :water] #(+ % 10))
                             )
                   }
                  }
      :craftable
      {
       :jackhammer {
                    :label "Jackhammer"
                    :explain "Increase mining yield"
                    :price (fn [state] {:metals 10})
                    :crafted false
                    }
       :metal-detector {
                    :label "Metal detector"
                    :explain "Increase findings when exploring"
                    :price (fn [state] {:metals 10 :electronics 4})
                    :crafted false
                    }
            }
      :journal []
      }
     :coins 100
     :bonuses
     {
      :potatoes-qty {
                     :label "More potatoes"
                     :level 0
                     :level-max 100
                     :next-price (bonus-price-double-every-level-fn 20)
                     :bonus-value (fn [level] (* level 10))
                     :bonus-apply (fn [state bvalue]
                                    (swap! state
                                           update-in
                                           [:game :resources :potatoes]
                                           #(+ % bvalue)))}



      :water-qty  {
                   :label "More water"
                   :level 0
                   :level-max 100
                   :next-price (bonus-price-double-every-level-fn 30)
                   :bonus-value (fn [level] (* level 10))
                   :bonus-apply (fn [state bvalue]
                                  (swap! state
                                         update-in
                                         [:game :resources :water]
                                         #(+ % bvalue)))
                   }

      :oxygen-qty  {
                   :label "Oxygen tank"
                   :level 0
                   :level-max 20
                   :next-price (bonus-price-double-every-level-fn 25)
                   :bonus-value (fn [level] (* level 10))
                   :bonus-apply (fn [state bvalue]
                                  (swap! state
                                         update-in
                                         [:game :equipment :oxygen-tank :capacity]
                                         #(+ % bvalue)))
                   }
      }
     }
  )

(defonce state
  (r/atom base-state))


(defn reinit-resources []
  (swap! state assoc-in [:game :resources] (get-in base-state [:game :resources]))
  (swap! state assoc-in [:game :hunger :current] 0)
  (swap! state assoc-in [:game :time] 0)
  (doall
    (map
      (fn [[_ b]] ((:bonus-apply b) state ((:bonus-value b) (:level b))))
      (:bonuses @state)))
  (swap! state
         assoc-in
         [:game :resources :oxygen]
         (get-in @state [:game :equipment :oxygen-tank :capacity]))
  )

(defn launch-timer []
  (js/setTimeout
    (fn []
      (next-hour state)
      (when-not (dead? state)
        (launch-timer)))
    5000))

(defn timeout [fun ms]
  (js/setTimeout fun ms))

(defn start-mission [coins]
  (reinit-resources)
  (swap! state update :coins #(+ coins %))
  (swap! state assoc :screen :mission-start))

(defn start-run []
  ;(launch-timer)
  (swap! state assoc :screen :game-main)
  (log-event state "You arrived on a deserted planet, you should find resources and survive as much as you can"))

;; -------------------------
;; Views

(defn buy-bonus [[k b]]
  (let [price ((:next-price b) (:level b))]
    (swap! state update :coins #(- % price))
    (swap! state update-in [:bonuses k :level] inc)
    (reinit-resources)
    ))

(defn has-enough-coins? [c]
  (>= (:coins @state) c))

(defn home-screen []
  [:div [:h2 "LD50 Space Company"]
   [:div [:h4 "Are you ready to go on an adventure?"]
    [:button {:on-click #(start-mission 0)} "Let's go!"]]
   ])

(defn show-bonus [[k b]]
  (let [lvl (:level b)]
    ^{:key (name k)}
    [:div {:class "bonus-container"}
     [:div {:class "bonus-label"} (str (:label b) " (" lvl ")")]
     [:div {:class "bonus-description"} (str "Bonus : +" ((:bonus-value b) lvl))]
     [:button
      {:on-click #(buy-bonus [k b])
       :disabled (not (has-enough-coins? ((:next-price b) lvl)))}
      (str "Buy: " ((:next-price b) lvl) " coins (+" ((:bonus-value b) (inc lvl)) ")" )]
     ]))

(defn bonuses []
  [:div
   [:strong (str "total coins : " (:coins @state))]
   [:div {:class "bonuses-container"}
    (doall
      (for [b (:bonuses @state)]
        (show-bonus b)))]])

(defn mission-start-screen []
  [:div {:class "black-bkg"}
   [:span "Starting a mission with: "]
   [:ul
    (for [resource (remove #(zero? (second %)) (get-in @state [:game :resources]))]
      ^{:key (name (first resource))} [:li (str (name (first resource)) " : " (second resource))]
     )]
   [:span "Good luck!"]
   [:hr]
   [:button {:on-click #(start-run)} "Let's Go!"]
   [:hr]
   [:h3 "Bonuses"]
   (bonuses)])

(defn oxygen-bar-filled-percent []
 (int (* 100
         (/ (get-in @state [:game :resources :oxygen])
            (get-in @state [:game :equipment :oxygen-tank :capacity])))))

(defn hunger-bar-filled-percent []
 (int (* 100
         (/ (get-in @state [:game :hunger :current])
            (get-in @state [:game :hunger :max])))))

(defn time-passed-human-readable []
  (let [h (get-in @state [:game :time])]
    (str h " hours")
    ))

(defn top-bar-stats []
  (let [oxygen-prc (oxygen-bar-filled-percent)
        hunger-prc (hunger-bar-filled-percent)
        time-passed (time-passed-human-readable)
        ;show-progress (not (dead? state))
        ]
    [:div {:class "top-bar-stats"}
     [:div {:class "time"}
      [:span (str "Time passed : " time-passed)]
      ;(when show-progress [:div {:class "progress"}])
      ]
     [:div
      [:div {:class "bar-container oxygen-bar-container"}
       [:div {:class "bar-fill oxygen-bar-fill"
              :style {:width (str oxygen-prc "%")}}
        ]
       [:span {:class "bar-text" } (str "O2 : " oxygen-prc "%")]  
       ]]
     [:div
      [:div {:class "bar-container hunger-bar-container"}
       [:div {:class "bar-fill hunger-bar-fill"
              :style {:width (str hunger-prc "%")}}
        ]

       [:span {:class "bar-text" } (str "Hunger: " hunger-prc "%")]]]
     ]))

;(defn next-hour-button [] [:button {:on-click #(next-hour)} "Next hour"] )

(defn gains-from-run []
  (let [from-time (* 10 (get-in @state [:game :time]))]
    (+ from-time)))

(defn death-screen []
  (let [gains (gains-from-run)]
    [:div
   [:h2 "You died"]
   [:span (str "You gained : " gains " coins")]
   [:button {:on-click #(start-mission gains)} "Go to shop"]

   ]))

(defn select-activity [k]
  (swap! state assoc-in [:game :selected-activity] k)
  (swap! state assoc-in [:game :activities k :available] false)
  (timeout #(swap! state assoc-in [:game :activities k :available] true)
           500)
  (next-hour state))

(defn activity [k a]
  (let [
        ;current-selected-activity (get-in @state [:game :selected-activity])
        ]
    ^{:key (name k)}
    [:div
     {:class (str "block activity" ;(when (= current-selected-activity k) " selected")
                  (if (:available a)
                    " can-do"
                    " disabled need-wait")
                  )
      :on-click #(select-activity k)}
     [:div (:label a)]
     [:div {:class "explain"} (:explain a)]
     (condp = k
       :harvest [:span (str (count (remove
                                     #(pos? (:time-left %))
                                     (get-in @state [:game :buildings :farm :plants]))) " potatoes available")]
       nil)
     (when (:price a) (price-display ((:price a) state)))
     ]))

(defn activities []
  [:div {:class "activities"}
   ;[:h4 "Activities"]
   (doall
     (for [[k a] (sort-by #(:order (second %)) (get-in @state [:game :activities]))]
       (when ((or (:condition a)
                  (fn [_] true)) state)
         (activity k a))
       ))
   ]
  )

(defn construct-building [k]
  (let [b (get-in @state [:game :buildings k])
        p ((:price b) state)]
    (if (has-resources? state p)
      (do
        (swap! state update-in [:game :resources] (fn [r] (merge-with - r p)))
        (swap! state assoc-in [:game :buildings k :built] true))
      (log-event state (str "not enough resources for " (:label b))))))

(defn building [k b]
  ^{:key (name k)}
  [:div
   {:class (str "block building"
                (if (:built b)
                  " built has-done"
                  (if (has-resources? state ((:price b) state))
                    " can-build can-do"
                    " cant-do")))
    :on-click (when-not (:built b) #(construct-building k))
    }
   [:span
    (:label b)
    (when-not (:built b) (price-display ((:price b) state)))
    (condp = k
      :farm [:div {:class "explain"} (str "Plants : " (count (:plants b)) "/" (:max-plants b))]
      nil
      ;[:code (:plants b)]
      )
    ]
   ])

(defn buildings []
  [:div {:class "buildings"}
   [:h4 "Buildings"]
   (doall
     (for [[k b] (get-in @state [:game :buildings])]
       (when (or (:built b)
                 ((:condition b) state))
         (building k b))
       ))
   ]
  )

(defn craft-item [k]
  (let [c (get-in @state [:game :craftable k])
        p ((:price c) state)]
    (if (has-resources? state p)
      (do
        (swap! state update-in [:game :resources] (fn [r] (merge-with - r p)))
        (swap! state assoc-in [:game :craftable k :crafted] true))
      (log-event state (str "not enough resources for " (:label c))))))


(defn craftable [k c]
  ^{:key (name k)}
  [:div {:class (str "block craftable"
                     (when (:crafted c) " crafted has-done")
                     (if (has-resources? state ((:price c) state))
                         " can-craft can-do"
                         " cant-do") )
         :on-click (when-not (:crafted c) #(craft-item k))}
   [:div (:label c)]
   [:div {:class "explain"} (:explain c)]
   (when-not (:crafted c) (price-display ((:price c) state)))
   ])

(defn craftables []
  [:div {:class "craftables"}
   [:h4 "Craftables"]
   (doall
     (for [[k b] (get-in @state [:game :craftable])]
       (craftable k b)
       ))])

(defn journal []
  [:div {:class "journal"}
   "Last event : "
   (for [evt (get-in @state [:journal])]
     ^{:key (:ts evt)} [:div {:class "event"} (:text evt)]
     )])

(defn alive-view []
  [:div
   [:div {:class "half half-left"}
    [:div {:class "container"} (activities)]
    ]
   [:div {:class "half half-right"}
    [:div {:class "container"} (journal)]
    [:div {:class "container"} (buildings)]
    (when (get-in @state [:game :buildings :craft-bench :built])
      [:div {:class "container"} (craftables)])
    ]
   ]
  )

(defn resources []
  [:table {:class "resources"}
   [:tbody
    (for [rrows (partition 3 (get-in @state [:game :resources]))]
      ^{:key (reduce str "" (map first rrows))}
      [:tr
       (for [[k r] rrows]
         ^{:key (name k)}
         [:td
          [:span {:class "rname"} (name k)]
          [:span {:class "rqty"} r]
          ]
         )])]
   ])

(defn game-main-screen []
  (let [is-dead (dead? state)]
    [:div {:class "game-main-screen"}
     (top-bar-stats)
     (resources)
     (if is-dead
       (death-screen)
       (alive-view)
       )
     [:hr]
     ]))

(defn root-page []
  (let
    [screen (:screen @state)]
    [:div
   ;[:code (with-out-str (cljs.pprint/pprint @state)) ]
   (condp = screen
    :home (home-screen)
    :mission-start (mission-start-screen)
    :game-main (game-main-screen)
    (home-screen)
    )])
  )

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [root-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
