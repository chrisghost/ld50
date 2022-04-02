(ns game.core
    (:require
      [reagent.core :as r]
      [reagent.dom :as d]))


(defn decrease-oxygen [state modifier-fn]
  (println (str "Decrease oxygen by " (modifier-fn 10)))
  (swap! state update-in [:game :resources :oxygen] #(- % (modifier-fn 10)))
  )

(defn increase-hunger [state modifier-fn]
  (println (str "Increase hunger by " (modifier-fn 10)))
  (swap! state update-in [:game :hunger :current] #(+ % (modifier-fn 10)))
  )

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


(defonce state
  (r/atom
    {
     :screen :game-main ;:home
     :game
     {
      :hunger {:current 0
               :max 30}
      :resources {
                  :oxygen 145
                  :stone 0
                  :water 10
                  :metals 0
                  :electronics 0
                  :potatoes 10
                  }
      :equipment {
                  :oxygen-tank {
                                :capacity 200
                                }}

      :time 0
      :selected-activity :rest
      :activities {
                   :rest {
                          :label "Rest"
                          :effect (fn [state]
                                   {:hunger (fn [decrease] (/ decrease 2))
                                    :oxygen (fn [decrease] (/ decrease 2))
                                    }
                                   )} 
                   :eat {
                          :label "Eat"
                          :effect (fn [state]
                                    (when (pos? (get-in @state [:game :resources :potatoes]))
                                      (swap! state update-in [:game :resources :potatoes] #(- % 1))
                                      (swap! state update-in [:game :hunger :current] #(- % 20)))
                                    )}}
      }
     }))


(defn launch-timer []
  (js/setTimeout
    (fn []
      (next-hour state)
      (when-not (dead? state)
        (launch-timer)))
    5000))

(defonce timer
  (launch-timer)
  )

(defn start-mission []
  (swap! state assoc :screen :mission-start))

;; -------------------------
;; Views

(defn home-screen []
  [:div [:h2 "LD50 Space Company"]
   [:div [:h4 "Are you ready to go on an adventure?"]
    [:button {:on-click #(start-mission)} "Let's go!"]]
   ])

(defn mission-start-screen []
  [:div {:class "black-bkg"}
   [:span "Starting a mission with: "]
   [:ul
    (for [resource (remove #(zero? (second %)) (get-in @state [:game :resources]))]
     [:li ^{:key (name (first resource))} (str (name (first resource)) " : " (second resource))]
     )]
   [:span "Good luck!"]])

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

(defn death-screen []
  [:div [:h2 "You died"]])

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
   (doall
     (for [[k a] (get-in @state [:game :activities])]
       (activity k a)
       ))
   [:hr]
   ]
  )

(defn game-main-screen []
  (let [is-dead (dead? state)]
    [:div {:class "game-main-screen"}
     (top-bar-stats)
     (if is-dead
       (death-screen)
       (activities)
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
