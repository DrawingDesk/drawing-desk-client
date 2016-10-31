(ns client.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [ajax.core :refer [GET POST]]
            [cemerick.url :as url]
            [accountant.core :as accountant]
            [cljs-uuid-utils.core :as uuid]
            [eval.core :as e]
            [clojure.walk :as walk]))

(defonce state (reagent/atom {:events [] :user nil :chat-log []}))

(defn concat-events [current events]
  (into [] (concat current events)))

(defn resolve-event [event]
  (e/invoke (str "client.core/" (:method event)) (:args event) (:id event) (:user event)(:tag event)))

(defn load [events]
  (mapv resolve-event events)
  (swap! state update-in [:events] concat-events events))

(defn send-message [message]
  (let [event {:id nil :tag (uuid/uuid-string (uuid/make-random-uuid)) :method "show-message" :user (:user @state) :args {:text message}}]
    (resolve-event event)
    (swap! state update-in [:events] conj event)
    ""))

(defn ^:export show-message [args id user tag]
  (swap! state update-in [:chat-log] conj {:id id :tag tag :user user :text (:text args)}))

(defn set-user [user]
  (swap! state assoc :user user)
  "")

(defn log-item []
  (fn [{:keys [id tag text user]}]
      [:div user ": " text " (" id " " tag ") "]
    ))

(defn chat-log []
  [:div "Chat log:"
   (for [message (:chat-log @state)]
     ^{:key message} [log-item message])])

(defn get-room []
      (let [room (-> js/window .-location .-href url/url :query walk/keywordize-keys :room)
            id (uuid/uuid-string (uuid/make-random-uuid))]
        (if (nil? room) (accountant/navigate! (str "?room=" id)))
        (if (nil? room) id room)))

(defn chat []
  (let [message (reagent/atom "")
        room (get-room)]
    (GET (str "http://lively-firefly-3821.getsandbox.com/events/" room) {:handler load :response-format :json :keywords? true})
    (fn []
      [:div [:h2 (:user @state) ", welcome to chat"]
       [chat-log]
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
