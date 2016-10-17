(ns client.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [ajax.core :refer [GET POST]]
            [cemerick.url :as url]
            [accountant.core :as accountant]
            [cljs-uuid-utils.core :as uuid]
            [clojure.walk :as walk]))

;; -------------------------
;; Views

(defonce state (reagent/atom {:events []}))
(defonce counter (reagent/atom 3))

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

(defn log-item []
  (fn [{:keys [id tag text]}]
      [:div id " " tag " " text]
    ))

(defn chat-log []
  [:div "Chat log:"
   (for [message (:events @state)]
     ^{:key message} [log-item message])])

(defn get-room []
      (let [room (-> js/window .-location .-href url/url :query walk/keywordize-keys :room)]
      (if (nil? room) (uuid/uuid-string (uuid/make-random-uuid)) room)))

(defn home-page []
  (let [message (reagent/atom "")
        room (get-room)]

     (GET "http://www.mocky.io/v2/580375512400003007135c0c" {:handler load :response-format :json :keywords? true})
     (.log js/console room )
     (fn []
        [:div [:h2 "Welcome to chat"]
        [chat-log]
        [:input {:type "text"
                :value @message
                :required ""
                :on-change #(reset! message (-> % .-target .-value))}]
        [:button {:on-click #(swap! message send)} "Send"]])))



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
