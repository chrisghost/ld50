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

(defn next-hour [state]
  (swap! state update-in [:game :time] inc)
  (let [current-activity (get-in @state
                                 [:game :activities (get-in @state [:game :selected-activity])])
        modifiers ((:effect current-activity) state)
        _ (println modifiers)
        ]
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
        depo (can-find-deposits state)
        _ (println depo)
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
        metals (if (> r 0.7)
                (int (* 10 (rand)))
                0)
        ]
    (merge stone-depo metals-depo {:resources {:stone stone :metals metals}})))

(defn ts [] (.getTime (js/Date.)))

(defn log-event [state evt]
  (swap! state update-in [:journal] (fn [j] (cons {:text evt :ts (ts)} j))))


(def base-state
  {
     :screen :mission-start ;:game-main ;:home
     :game
     {
      :hunger {:current 0
               :max 30000}
      :resources {
                  :oxygen 1450
                  :stone 0
                  :water 10
                  :metals 0
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
                    :label "Rest"
                    :effect (fn [state]
                              {:hunger (fn [decrease] (/ decrease 2))
                               :oxygen (fn [decrease] (/ decrease 2))
                               }
                              )} 
                   :eat
                   {
                    :label "Eat"
                    :effect (fn [state]
                              (when (pos? (get-in @state [:game :resources :potatoes]))
                                (swap! state update-in [:game :resources :potatoes] #(- % 1))
                                (swap! state update-in [:game :hunger :current] #(- % 20)))
                              )}
                   :explore
                   {
                    :label "Explore"
                    :effect (fn [state]
                              (let [finds (exploration-finds state)]
                                (println "Found : " finds)
                                (log-event state (str "Found : " finds))
                                (swap! state update-in [:game :resources]
                                     (fn [rs] (merge-with + rs (:resources finds))))
                                (swap! state update-in [:game :deposits]
                                       (fn [ds] (merge-with + ds (:deposits finds))))

                                {:hunger (fn [factor] (int (* 1.5 factor)))
                                 :oxygen (fn [factor] (int (* 1.5 factor)))
                                 })
                              )
                    }
                   :mine-stone
                   {
                    :label "Mine stone"
                    :effect (fn [state]
                              (swap! state update-in [:game :resources :stone] #(+ % 5))
                                {:hunger (fn [factor] (int (* 1.7 factor)))
                                 :oxygen (fn [factor] (int (* 1.7 factor)))
                                 })
                    :condition (fn [state] (pos? (get-in @state [:game :deposits :stone])))
                    }
                   :mine-metals
                   {
                    :label "Mine metals"
                    :effect (fn [state]
                              (swap! state update-in [:game :resources :metals] #(+ % 5))
                                {:hunger (fn [factor] (int (* 1.7 factor)))
                                 :oxygen (fn [factor] (int (* 1.7 factor)))
                                 })
                    :condition (fn [state] (pos? (get-in @state [:game :deposits :metals])))
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

(defn start-mission [coins]
  (reinit-resources)
  (swap! state update :coins #(+ coins %))
  (swap! state assoc :screen :mission-start))

(defn start-run []
  (launch-timer)
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
        show-progress (not (dead? state))]
    [:div {:class "top-bar-stats"}
     [:div {:class "time"}
      [:span (str "Time passed : " time-passed)]
      (when show-progress [:div {:class "progress"}])]
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
  (swap! state assoc-in [:game :selected-activity] k))

(defn activity [k a]
  (let [current-selected-activity (get-in @state [:game :selected-activity])]
    ^{:key (name k)}
    [:div
     {:class (str "activity" (when (= current-selected-activity k) " selected"))
      :on-click #(select-activity k)}
     [:span (:label a)]
     ]))

(defn activities []
  [:div {:class "activities"}
   [:h1 "Activities"]
   (doall
     (for [[k a] (get-in @state [:game :activities])]
       (when ((or (:condition a)
                  (fn [_] true)) state)
         (activity k a))
       ))
   ]
  )

(defn journal []
  [:div {:class "journal"}
   (for [evt (get-in @state [:journal])]
     ^{:key (:ts evt)} [:div {:class "event"} (:text evt)]
     )])

(defn alive-view []
  [:div
   [:div {:class "half half-left"} [:div {:class "container"} (activities)]]
   [:div {:class "half half-right"} [:div {:class "container"} (journal)]]] 
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
