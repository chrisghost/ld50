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
  (swap! state
         update-in
         [:game :resources :oxygen]
         #(clamp-low (- % (modifier-fn (:base-oxygen-consumption @state))) 0))
  )

(defn increase-hunger [state modifier-fn]
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
        stone (if (> r 0.2)
                (int (* 10 (rand)))
                0)
        metals (if (> r
                      (if has-metal-detector
                        0.1
                        0.3))
                (int (* 5 (rand)))
                0)
        alien-mineral (if (> r
                             (if has-metal-detector
                               0.7
                               0.9))
                        1
                        0)
        ]
    (merge stone-depo metals-depo
           {:resources {:stone stone :metals metals :alien-mineral alien-mineral}})))

(defn ts [] (.getTime (js/Date.)))

(defn log-event [state evt]
  ;(swap! state update-in [:journal] (fn [j] (cons {:text evt :ts (ts)} j))) 
  (swap! state assoc-in [:journal] [{:text evt :ts (ts)}]))

(defn has-resources? [state resources]
  (let [rsk (keys resources)
        has (select-keys (get-in @state [:game :resources]) rsk)]
    (every? #(not (neg? (second %))) (merge-with - has resources))))

(defn resource-display
  ([p] (resource-display nil p))
  ([state p]
   (doall
     (map
       (fn [[k v]]
         (let [rstr (str (clojure.string/replace (name k) #"-" " ") " : " v " / ")]
           (if (nil? state)
             ^{:key k} [:span rstr]
             (if (has-resources? state {k v})
               ^{:key k} [:span rstr]
               ^{:key k} [:span {:style {:color "red"}} rstr]))))
       (filter #(pos? (second %)) p)))))


(defn price-display [state p]
  [:div {:class "price"}
   (resource-display state p)])


(def base-state
  {
     ;:screen :achievements
     ;:screen :mission-start
     ;:screen :game-main 
     :screen :home
     :screen-start-mission-tab :bonuses
     :won false
     :total-time 0
     :total-runs 0

     :hunger-decrease-from-eating 20
     :base-oxygen-consumption 10
     :potato-time-to-grow 5
     :water-extraction-n 10
     :oxygen-extraction-n 20

     :game
     {
      :hunger {:current 0
               :max 300}
      :resources {
                  :oxygen 100
                  :stone 0
                  :water 10
                  :metals 0
                  :electronics 0
                  :potatoes 1
                  :alien-mineral 0
                  :diamond 0
                  }
      :deposits {
                 :stone 0
                 :metals 0
                 }
      :equipment {
                  :oxygen-tank {
                                :capacity 100
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
                                (swap! state update-in [:game :hunger :current]=
                                       #(- % (get-in @state [:hunger-decrease-from-eating]))))
                              )
                    :condition (fn [state]
                                    (pos? (get-in @state [:game :resources :potatoes])))
                    :available true
                    }
                   :explore
                   {
                    :order 2
                    :label "Explore"
                    :explain "Go explore surroundings to find resources or more ..."
                    :effect (fn [state]
                              (let [finds (exploration-finds state)]
                                (log-event state
                                           [:div "Found : "
                                                (if (pos? (reduce + (map second (:resources finds))))
                                                  (resource-display (:resources finds))
                                                  "nothing ...")
                                                (when (get-in finds [:deposits :metals])
                                                  " And a metals deposit !")
                                                (when (get-in finds [:deposits :stone])
                                                  " And a stone deposit !")
                                                ])
                                (swap! state update-in [:game :resources]
                                     (fn [rs] (merge-with + rs (:resources finds))))
                                (swap! state update-in [:game :deposits]
                                       (fn [ds] (merge-with + ds (:deposits finds))))

                                {:hunger (fn [factor] (int (* 1.5 factor)))
                                 :oxygen (fn [factor] (int (* 1.5 factor)))
                                 }
                                {})
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
                                     (fn [p] (cons {:time-left (get-in @state [:potato-time-to-grow])} p))
                                     )
                                {:hunger (fn [factor] (int (* 1.7 factor)))
                                 :oxygen (fn [factor] (int (* 1.7 factor)))
                                 })
                    :condition (fn [state]
                                      (and
                                        (get-in @state [:game :buildings :farm :built])
                                        (< (count (get-in @state [:game :buildings :farm :plants]))
                                           (get-in @state [:game :buildings :farm :max-plants]))))
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

                              (swap! state update-in [:game :achievements :grow-food] inc)
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
                                {:oxygen #(/ % 3)})
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
                                (swap! state update-in [:game :resources :water] #(+ % (get-in @state [:water-extraction-n]))) 
                                {})
                    :condition (fn [state]
                                 (get-in @state [:game :buildings :water-extractor :built]))
                    :available true
                    }

                  :extract-oxygen
                   {
                    :order 9
                    :label "Extract oxygen"
                    :explain "the second source of life!"
                    :effect (fn [state]
                                (swap! state update-in [:game :resources :oxygen] #(+ % (get-in @state [:oxygen-extraction-n]))) 
                                {})
                    :condition (fn [state]
                                 (get-in @state [:game :buildings :oxygen-extractor :built]))
                    :available true
                    }

                  :analyze-mineral
                   {
                    :order 10
                    :label "Analyze alien mineral"
                    :explain "Who knows what we'll find!"
                    :effect (fn [state]
                                (swap! state update-in [:game :resources :alien-mineral] dec) 
                                (swap! state update-in [:game :achievements :analyze-mineral] inc)
                                (let [r (rand)
                                      finds (condp < r
                                              0.1 {:resource :diamond :text "An alien diamond!"}
                                              0.4 {:resource :metals :text "Iron, we can use this"}
                                              {:resource :stone :text "Oh wait, it's just stone"}
                                              )]
                                  (swap! state update-in [:game :resources (:resource finds)] inc)
                                  (log-event state (:text finds))
                                  )
                                {})
                    :condition (fn [state]
                                 (and
                                   (get-in @state [:game :buildings :laboratory :built])
                                   (pos? (get-in @state [:game :resources :alien-mineral]))))
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

                  :oxygen-extractor
                  {
                   :label "Oxygen extractor"
                   :price (fn [state] {:stone 50 :metals 20})
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
                   :price (fn [state] {:stone 10 :metals 2})
                   :built false
                   :condition (fn [state]
                                true
                                ;(has-resources? state {:stone 15 :metals 2})
                                )
                   :effect (fn [state]
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
                  :craft-bench
                  {
                   :label "Craft bench"
                   :price (fn [state] {:stone 10 :metals 2})
                   :built false
                   :condition (fn [state]
                                (has-resources? state {:stone 5 :metals 1}))
                   :effect (fn [state]
                             ;(swap! state update-in [:game :resources :water] #(+ % 10))
                             )
                   }
                  :laboratory
                  {
                   :label "Laboratory"
                   :price (fn [state] {:metals 20 :electronics 10 :stone 50})
                   :built false
                   :condition (fn [state]
                                (has-resources? state {:metals 10 :electronics 5 :stone 10}))
                   :effect (fn [state])
                   }
                  :alien-beacon
                  {
                   :label "Alien beacon"
                   :price (fn [state] {:metals 20 :electronics 10 :stone 50})
                   :built false
                   :condition (fn [state]
                                (and
                                  (get-in @state [:game :craftable :alien-crystal :crafted])
                                  (has-resources? state {:metals 100 :electronics 50 :stone 300})))
                   :effect (fn [state]
                             (swap! state assoc :won true))
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
       :alien-crystal {
                       :label "Alien crystal"
                       :explain "A glowing crystal with untold power"
                       :price (fn [state] {:diamond 4})
                       :crafted false
                 }
            }
      :journal []
      :achievements {
                     :grow-food 0
                     :analyze-mineral 0
                     }
      }
     :coins 100
     :bonuses
     {
      :potatoes-qty {
                     :label "More potatoes"
                     :level 0
                     :level-max 100
                     :next-price (bonus-price-double-every-level-fn 20)
                     :bonus-value (fn [level] (* level 2))
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

      :oxygen-consumption-decrease  {
                   :label "Yoga"
                   :description "Decrease oxygen consumption"
                   :level 0
                   :level-max 9
                   :next-price (bonus-price-double-every-level-fn 25)
                   :bonus-value (fn [level] (* level 1))
                   :bonus-apply (fn [state bvalue]
                                  (swap! state
                                         assoc-in
                                         [:base-oxygen-consumption]
                                         (- (:base-oxygen-consumption base-state) bvalue)))
                   }

      :better-potatoes  {
                   :label "Ketchup"
                   :description "Improves potatoes nutritional value"
                   :level 0
                   :level-max 9
                   :next-price (bonus-price-double-every-level-fn 25)
                   :bonus-value (fn [level] (* level 4))
                   :bonus-apply (fn [state bvalue]
                                  (swap! state
                                         assoc-in
                                         [:hunger-decrease-from-eating]
                                         (+ (:hunger-decrease-from-eating base-state) bvalue)))
                   }

      :jackhammer  {
                   :label "Free jackhammer!"
                   :level 0
                   :level-max 1
                   :next-price (bonus-price-double-every-level-fn 300)
                   :bonus-value (fn [level] (* level 1))
                   :bonus-apply (fn [state bvalue]
                                  (swap! state
                                         assoc-in
                                         [:game :craftable :jackhammer :crafted] (= bvalue 1)))
                   }

      :metal-detector  {
                   :label "Coins grabber"
                   :description "Always have a metal detector on you"
                   :level 0
                   :level-max 1
                   :next-price (bonus-price-double-every-level-fn 250)
                   :bonus-value (fn [level] (* level 1))
                   :bonus-apply (fn [state bvalue]
                                  (swap! state
                                         assoc-in
                                         [:game :craftable :metal-detector :crafted] (= bvalue 1)))
                   }

      :stone-deposit  {
                   :label "Landing spot 1"
                   :description "Land near a discovered stone deposit"
                   :level 0
                   :level-max 1
                   :next-price (bonus-price-double-every-level-fn 20)
                   :bonus-value (fn [level] (* level 1))
                   :bonus-apply (fn [state bvalue]
                                  (swap! state
                                         assoc-in
                                         [:game :deposits :stone]
                                         bvalue))
                   }

      :metal-deposit  {
                   :label "Landing spot 2"
                   :description "Land near a discovered metal deposit"
                   :level 0
                   :level-max 1
                   :next-price (bonus-price-double-every-level-fn 40)
                   :bonus-value (fn [level] (* level 1))
                   :bonus-apply (fn [state bvalue]
                                  (swap! state
                                         assoc-in
                                         [:game :deposits :metals]
                                         bvalue))
                   }

      }
      :achievements
      {
       :grow-food
       {
        :done false
        :label "Grow food on the planet"
        :reward 100
        :condition (fn [state] (pos? (get-in @state [:game :achievements :grow-food])))
        }
       :analyze-mineral
       {
        :done false
        :label "Analyze alien mineral"
        :reward 200
        :condition (fn [state] (pos? (get-in @state [:game :achievements :analyze-mineral])))
        }
       :survive-ten-days
       {
        :done false
        :label "Survive 10 days"
        :reward 20
        :condition (fn [state] (> (get-in @state [:game :time]) 9))
        }
       }
     }
  )

(defonce state
  (r/atom base-state))


(defn reinit-resources []
  (swap! state assoc-in [:game :resources] (get-in base-state [:game :resources]))
  (swap! state assoc-in [:game :deposits] (get-in base-state [:game :deposits]))
  (swap! state assoc-in [:game :buildings] (get-in base-state [:game :buildings]))
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

(defn run-achievements []
  (filter
    (fn [[k a]]
      (and
        (not (:done a))
        ((:condition a) state)))
    (get-in @state [:achievements])))

(defn start-mission [coins]
  (doseq [[k a] (run-achievements)]
    (swap! state assoc-in [:achievements k :done] true))

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
    [:button {:on-click #(start-mission 0) :class "button big-button"} "Let's go!"]]
   ])

(defn show-bonus [k b]
  (let [lvl (:level b)]
    ^{:key (name k)}
    [:div {:class "bonus-container"}
     [:div {:class "bonus-label"} (str (:label b) " (" lvl ")")]
     [:div {:class "bonus-description"} (str (:description b))]
     (when (> (:level-max b) 1)
       [:div {:class "bonus-description"} (str "Current bonus : +" ((:bonus-value b) lvl))])
     [:button
      {:on-click #(buy-bonus [k b])
       :class "button small-button"
       :disabled (or
                   (>= (:level b) (:level-max b))
                   (not (has-enough-coins? ((:next-price b) lvl))))}
      (if (>= (:level b) (:level-max b))
        (str "Maxed")
        (str "Buy: " ((:next-price b) lvl) " coins (+" ((:bonus-value b) (inc lvl)) ")" ))]
     ]))

(defn bonuses []
  [:div
   [:strong (str "Coins : " (:coins @state))]
   [:table {:class "bonuses"}
    [:tbody
     (doall
       (for [rows (partition 3 3 [] (seq (:bonuses @state)))]
      ^{:key (reduce str "" (map first rows))}
      [:tr
       (doall
         (for [[k b] rows]
         ^{:key (name k)}
         [:td
          (show-bonus k b)
          ]
         ))]  
       ))
     ]
    ]
   ])

(defn mission-start-screen []
  [:div {:style {:text-align "center"}}
   ;[:span "Starting a mission with: "]
   (comment [:ul
    (for [resource (remove #(zero? (second %)) (get-in @state [:game :resources]))]
      ^{:key (name (first resource))} [:li (str (name (first resource)) " : " (second resource))]
      )])
   [:h2 "Mission preparation"]
   [:hr]
   [:button {:on-click #(start-run) :class "start-run-button button big-button"} "Let's Go!"]
   [:hr]
   (let [tab (get-in @state [:screen-start-mission-tab])]
     [:div 
      [:h3 {:style {:float "left" :cursor "pointer" :text-decoration "underline"}
            :on-click #(swap! state assoc :screen-start-mission-tab :bonuses)}
       "Bonuses"]
      [:h3 {:style {:float "right" :cursor "pointer" :text-decoration "underline"}
            :on-click #(swap! state assoc :screen-start-mission-tab :achievements)}
       "Achievements"]
      [:div {:style {:clear "both"}}]
      (if (= :bonuses tab)
        [:div
         (bonuses)]
        [:div {:class "achievements"}
         (for [[k a] (get-in @state [:achievements])]
           ^{:key k} [:div {:class (str "achievement" (when (:done a) " done"))}
                      [:span {:class "label"} (:label a)]
                      [:span {:class "reward"} (str (:reward a) " coins")]
                      ])
         ]
        )])
   ])

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
      [:span (str "Time : " time-passed)]
      ;(when show-progress [:div {:class "progress"}])
      ]
     [:div
      [:div {:class "bar-container oxygen-bar-container"}
       [:div {:class "bar-fill oxygen-bar-fill"
              :style {:background (str "linear-gradient(to right, blue " oxygen-prc "%, transparent 0)");
                      }}
        [:div {:class "bar-text" } (str "O2 : " oxygen-prc "%")]
        ]

       ]]
     [:div
      [:div {:class "bar-container hunger-bar-container"}
       [:div {:class "bar-fill hunger-bar-fill"
              :style {:background (str "linear-gradient(to right, red " hunger-prc "%, transparent 0)");
                      }}
        [:div {:class "bar-text" } (str "Hunger : " hunger-prc "%")]
        ]

       ]]
     ]))

;(defn next-hour-button [] [:button {:on-click #(next-hour)} "Next hour"] )

(defn gains-from-run []
  (let [from-time (* 10 (get-in @state [:game :time]))
        from-diamonds (* 100 (get-in @state [:game :resources :diamond]))

        achievs (run-achievements)

        from-achievements (reduce + (map :reward (map second achievs)))
        ]

    {:total (+ from-time
               from-diamonds
               from-achievements
               )
     :description (remove nil? [
                   (str from-time " from time spent on the planet")
                   (when (pos? from-diamonds) (str from-diamonds " from diamonds"))
                   (when (pos? from-achievements) (str from-achievements " from achievements"))
                   ])}))

(defn won-screen []
  [:div
   [:h2 "We made contact with the aliens!"]
   [:div "Great job! You successfully established contact with the aliens on this planet"]
   [:div (str "It took you " (:total-time @state) " hours accross " (:total-runs @state) " expeditions")]
   ])


(defn death-screen []
  (let [gains (gains-from-run)]
    [:div {:style {:text-align "center"}}
     [:h2 "You died"]
     [:div (str "You gained : " (:total gains) " coins !")]
     [:br]
     (for [g (:description gains)]
       ^{:key g} [:div g])
     [:br]
     [:button {:on-click #(start-mission (:total gains)) :class "button medium-button"} "Go to shop"]

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
                                     (get-in @state [:game :buildings :farm :plants]))) " potato(es) available")]
       nil)
     (when (:price a) (price-display state ((:price a) state)))
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
    (when-not (:built b) (price-display state ((:price b) state)))
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
   (when-not (:crafted c) (price-display state ((:price c) state)))
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
   [:code (get-in @state [:game :deposits])]
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
    (for [rrows (partition 4 4 [] (get-in @state [:game :resources]))]
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
     (if (:won @state)
       (won-screen)
       (if is-dead
         (death-screen)
         (alive-view)
         ))
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
