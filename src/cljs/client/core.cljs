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

(defn not-in?
  [coll elm]
  (some #(not= (:tag elm) (:tag %)) coll))

(defonce state (reagent/atom {:events [] :outgoing [] :server-state {:chat-log []} :client-state {:chat-log []} :user nil :room nil :sync 0}))

(defn concat-events [current events]
  (into [] (concat current events)))

(defn resolve-event [event]
  (e/invoke (str "client.core/" (:method event)) (:args event) (:sync-id event) (:user event) (:tag event)))

(defn resolve-events [events]
  (let [last (last events)]
    (if (some? last)
      (do
        (swap! state assoc :outgoing (into [] (filter (fn [outgoing] (not-in? events outgoing)) (:outgoing @state))))

        (swap! state update-in [:events] concat-events events)
        (swap! state assoc :client-state (:server-state @state))
        (mapv resolve-event events)
        (mapv resolve-event (:outgoing @state))
        (swap! state assoc :sync (:sync-id last))))))

(defn init [events]
  (resolve-events events)
  (js/setInterval #(GET (routes/get-events-url (:room @state) (:sync @state)) {:handler resolve-events :response-format :json :keywords? true}) 3000))

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

(defn set-user [user]
  (swap! state assoc :user user)
  "")

(defn chat-log []
  [:div "Chat log:"
   (for [message (:chat-log (:client-state @state))]
     ^{:key message} [:div (:user message) ": " (:text message) " (" (:id message) " " (:tag message) ") "])])

(defn set-room []
      (let [room (-> js/window .-location .-href url/url :query walk/keywordize-keys :room)
            id (uuid/uuid-string (uuid/make-random-uuid))]
        (if (nil? room) (accountant/navigate! (str "?room=" id)))
        (if (nil? room) (swap! state assoc :room id) (swap! state assoc :room room))))

(defn chat []
  (let [message (reagent/atom "")]
    (set-room)
    (GET (routes/get-events-url (:room @state)) {:handler init :response-format :json :keywords? true})
    (fn []
      [:div [:h2 (:user @state) ", welcome to chat"]
       (chat-log)
       [:input {:type "text"
                :value @message
                :required ""
                :on-change #(reset! message (-> % .-target .-value))}]
       [:button {:on-click #(swap! message send-message)} "Send"]])))

(defn login []
  (let [user (reagent/atom "")]

    (fn []
      [:div [:h2 "Please, enter your name:"]
       [:input {:type "text"
                :value @user
                :required ""
                :on-change #(reset! user (-> % .-target .-value))}]
       [:button {:on-click #(swap! user set-user)} "Enter"]])))

(defn home-page []
  [(if (nil? (:user @state)) login chat)])


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
