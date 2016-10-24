(ns client.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [ajax.core :refer [GET POST]]
            [cemerick.url :as url]
            [accountant.core :as accountant]
            [cljs-uuid-utils.core :as uuid]
            [clojure.walk :as walk]))

(defonce state (reagent/atom {:events [] :user-name nil}))

(defn concat-events [current events]
  (into [] (concat current events)))

(defn load [events]
  (.log js/console events)
  (swap! state update-in [:events] concat-events events)
  (.log js/console state))

(defn send [message]
  (swap! state update-in [:events] conj {:id nil :tag (uuid/uuid-string (uuid/make-random-uuid)) :text message})
  (.log js/console state)
  "")

(defn set-user-name [user-name]
  (swap! state assoc :user-name user-name)
  (.log js/console state)
  "")

(defn log-item []
  (fn [{:keys [id tag text]}]
      [:div id " " tag " " text]
    ))

(defn chat-log []
  [:div "Chat log:"
   (for [message (:events @state)]
     ^{:key message} [log-item message])])

(defn get-room []
      (let [room (-> js/window .-location .-href url/url :query walk/keywordize-keys :room)
            id (uuid/uuid-string (uuid/make-random-uuid))]
        (if (nil? room) (accountant/navigate! (str "?room=" id)))
        (if (nil? room) id room)))

(defn chat []
  (let [message (reagent/atom "")
        room (get-room)]
    ;580375512400003007135c0c
    (GET (str "http://www.mocky.io/v2/" room) {:handler load :response-format :json :keywords? true})
    (fn []
      [:div [:h2 (:user-name @state) ", welcome to chat"]
       [chat-log]
       [:input {:type "text"
                :value @message
                :required ""
                :on-change #(reset! message (-> % .-target .-value))}]
       [:button {:on-click #(swap! message send)} "Send"]])))

(defn login []
  (let [user-name (reagent/atom "")]

    (fn []
      [:div [:h2 "Please, enter your name:"]
       [:input {:type "text"
                :value @user-name
                :required ""
                :on-change #(reset! user-name (-> % .-target .-value))}]
       [:button {:on-click #(swap! user-name set-user-name)} "Enter"]])))

(defn home-page []
  [(if (nil? (:user-name @state)) login chat)])


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
