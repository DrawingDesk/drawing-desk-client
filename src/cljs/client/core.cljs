(ns client.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [ajax.core :refer [GET POST]]
            [utils.routes :as routes]
            [cemerick.url :as url]
            [accountant.core :as accountant]
            [cljs-uuid-utils.core :as uuid]
            [eval.core :as e]
            [clojure.walk :as walk]))

(defonce state (reagent/atom {:events [] :outgoing [] :server-state {:chat-log []} :client-state {:chat-log []} :user nil :room nil :sync 0}))
(defonce canvas (reagent/atom {:element nil :ctx nil :actions [] :draw false :position {:prev {:x 0 :y 0} :curr {:x 0 :y 0}}}))

(defn not-in?
  [coll elm]
  (some #(not= (:tag elm) (:tag %)) coll))

(defn concat-events [current events]
  (into [] (concat current events)))

(defn resolve-event [event]
  (e/invoke (str "client.core/" (:method event)) (:args event) (:id event) (:user event) (:tag event)))

(defn resolve-events [events]
  (let [last (last events)]
    (if (some? last)
      (do
        (swap! state assoc :outgoing (into [] (filter (fn [outgoing] (not-in? events outgoing)) (:outgoing @state))))
        (swap! state update-in [:events] concat-events events)
        (swap! state assoc :client-state (:server-state @state))
        (.putImageData (:ctx @canvas) (-> @state :server-state :canvas) 0 0)
        (mapv resolve-event events)
        (mapv resolve-event (:outgoing @state))
        (swap! state assoc :sync (:id last))))))

(defn save []
  (let [event {:id nil :tag (uuid/uuid-string (uuid/make-random-uuid)) :method "pencil" :user (:user @state) :args {:actions (:actions @canvas)}}]
    (POST (routes/post-event-url (:room @state)) {:params event :format :json :response-format :json :keywords? true})
    (swap! state update-in [:outgoing] conj event)
    (swap! canvas assoc :actions [])))

(defn draw [position]
  (-> @canvas :ctx .beginPath)
  (.moveTo (:ctx @canvas) (-> position :prev :x) (-> position :prev :y))
  (.lineTo (:ctx @canvas) (-> position :curr :x) (-> position :curr :y))
  (set! (. (:ctx @canvas) -lineWidth) 1)
  (.stroke (:ctx @canvas))
  (-> @canvas :ctx .closePath))

(defn move [e]
  (if (true? (:draw @canvas))
    (do
      (swap! canvas assoc :position {
                                     :prev {
                                            :x (-> @canvas :position :curr :x)
                                            :y (-> @canvas :position :curr :y)
                                            }
                                     :curr {
                                            :x (- (.-clientX e) (-> @canvas :element .-offsetLeft))
                                            :y (- (.-clientY e) (-> @canvas :element .-offsetTop))
                                            }
                                     })
      (swap! canvas update-in [:actions] conj (:position @canvas))
      (draw (:position @canvas)))))

(defn down [e]
  (swap! canvas assoc :position {
                                 :prev {
                                        :x (-> @canvas :position :curr :x)
                                        :y (-> @canvas :position :curr :y)
                                        }
                                 :curr {
                                        :x (- (.-clientX e) (-> @canvas :element .-offsetLeft))
                                        :y (- (.-clientY e) (-> @canvas :element .-offsetTop))
                                        }
                                 })
  (swap! canvas assoc :draw true)
  ;(-> @canvas :ctx .beginPath)
  ;(set! (. :ctx -fillStyle) (str "rgb(" r "," g "," b ")"))
  ;(.fillRect (:ctx @canvas) (-> @canvas :position :curr :x) (-> @canvas :position :curr :y) 1 1)
  ;(-> @canvas :ctx .closePath)
  )

(defn up []
  (swap! canvas assoc :draw false)
  (save))

(defn desk-init []
  (let [canvas-element (.getElementById js/document "desk")
        ctx (.getContext canvas-element "2d")]
    (swap! canvas assoc :element canvas-element)
    (swap! canvas assoc :ctx ctx)
    (swap! state update-in [:server-state] assoc :canvas (.getImageData ctx 0 0 500 500))
    (.addEventListener canvas-element "mousemove" move)
    (.addEventListener canvas-element "mousedown" down)
    (.addEventListener canvas-element "mouseup" up)
    (.addEventListener canvas-element "mouseout" up)))

(defn init [events]
  (desk-init)
  (js/setInterval #(GET (routes/get-events-url (:room @state) (:sync @state)) {:handler resolve-events :response-format :json :keywords? true}) 3000)
  (resolve-events events))

(defn send-message [message]
  (let [event {:id nil :tag (uuid/uuid-string (uuid/make-random-uuid)) :method "show-message" :user (:user @state) :args {:text message}}]
    (POST (routes/post-event-url (:room @state)) {:params event :format :json :response-format :json :keywords? true})
    (resolve-event event)
    (swap! state update-in [:outgoing] conj event)
    ""))

(defn ^:export show-message [args id user tag]
  (if (some? id)
    (swap! state update-in [:server-state] update-in [:chat-log] conj {:id id :tag tag :user user :text (:text args)}))
  (swap! state update-in [:client-state] update-in [:chat-log] conj {:id id :tag tag :user user :text (:text args)}))

(defn ^:export pencil [args id]
  (mapv draw (:actions args))
  (if (some? id)
    (swap! state update-in [:server-state] assoc :canvas (.getImageData (:ctx @canvas) 0 0 500 500))))

(defn set-user [user]
  (swap! state assoc :user user)
  "")

(defn chat-log []
  [:div [:h3 "Chat log:"]
   (for [message (:chat-log (:client-state @state))]
     ^{:key message} [:div (:user message) ": " (:text message) " (" (:id message) " " (:tag message) ") "])])

(defn desk-component []
  [:div {:class "desk-container"} [:h3 "Drawing-desk:"]
   [:canvas {:width 500 :height 500 :id "desk"}]])

(defn set-room []
      (let [room (-> js/window .-location .-href url/url :query walk/keywordize-keys :room)
            id (uuid/uuid-string (uuid/make-random-uuid))]
        (if (nil? room) (accountant/navigate! (str "?room=" id)))
        (if (nil? room) (swap! state assoc :room id) (swap! state assoc :room room))))

(defn chat-component []
  (let [message (reagent/atom "")]
    (fn []
      [:div {:class "chat-container"}
       (chat-log)
       [:input {:type "text"
                :value @message
                :required ""
                :on-change #(reset! message (-> % .-target .-value))}]
       [:button {:on-click #(swap! message send-message)} "Send"]])))

(defn index-page []
  (set-room)
  (GET (routes/get-events-url (:room @state)) {:handler init :response-format :json :keywords? true})
  (fn [] [:div [:h2 (:user @state) ", welcome to drawing desk"]
          [desk-component]
          [chat-component]]))

(defn login-page []
  (let [user (reagent/atom "")]
    (fn []
      [:div {:class "login-container"} [:h2 "Please, enter your name:"]
       [:input {:type "text"
                :value @user
                :required ""
                :on-change #(reset! user (-> % .-target .-value))}]
       [:button {:on-click #(swap! user set-user)} "Enter"]])))

(defn home-page []
  [(if (nil? (:user @state)) login-page index-page)])


(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
