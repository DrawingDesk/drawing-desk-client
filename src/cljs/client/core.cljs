(ns client.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [ajax.core :refer [GET POST]]
            [utils.routes :as routes]
            [cemerick.url :as url]
            [accountant.core :as accountant]
            [cljs-uuid-utils.core :as uuid]
            [eval.core :as e]
            [clojure.walk :as walk]))

(defonce state (reagent/atom {:events [] :outgoing [] :server-state {:chat-log []} :client-state {:chat-log []} :user nil :room nil :sync 0 :draw-type :pencil}))
(defonce canvas (reagent/atom {:ctx nil :actions [] :draw false :position {:prev {:x 0 :y 0} :curr {:x 0 :y 0} :start {:x 0 :y 0}}}))

(defn not-in?
  [coll elm]
  (some #(not= (:tag elm) (:tag %)) coll))

(defn concat-events [current events]
  (into [] (concat current events)))

(defn resolve-event [event]
  (if (not (:disabled event)) (e/invoke (str "client.core/" (:method event)) (:args event) (:sync-id event) (:user event) (:tag event))))

(defn update-events [events]
  (let [updated (into [] (filter (fn [event] (= (:method event) "update-event")) events))]
    (mapv resolve-event updated)))

(defn resolve-events [events]
  (let [last (last events)
        filtered (into [] (filter (fn [event] (not= (:method event) "update-event")) events))]
    (if (some? last)
      (do
        (update-events events)
        ;TODO: fix error when outgoing is array
        (swap! state assoc :outgoing (into [] (filter (fn [outgoing] (not-in? filtered outgoing)) (:outgoing @state))))
        (swap! state update-in [:events] concat-events filtered)
        (swap! state assoc :client-state (:server-state @state))
        (.putImageData (:ctx @canvas) (-> @state :server-state :canvas) 0 0)
        (mapv resolve-event filtered)
        (mapv resolve-event (:outgoing @state))
        (swap! state assoc :sync (:sync-id last))))))

(defn save []
  (let [event {:sync-id nil :tag (uuid/uuid-string (uuid/make-random-uuid)) :method (name (:draw-type @state)) :user (:user @state) :args {:actions (:actions @canvas)}}]
    (POST (routes/post-event-url (:room @state)) {:params event :format :json :response-format :json :keywords? true})
    (swap! state update-in [:outgoing] conj event)
    (.putImageData (:ctx @canvas) (-> @state :client-state :canvas) 0 0)
    (swap! canvas assoc :actions [])
    (resolve-event event)))

(defn drawPencil [position]
  (-> @canvas :ctx .beginPath)
  (.moveTo (:ctx @canvas) (-> position :prev :x) (-> position :prev :y))
  (.lineTo (:ctx @canvas) (-> position :curr :x) (-> position :curr :y))
  (set! (. (:ctx @canvas) -lineWidth) 1)
  (.stroke (:ctx @canvas))
  (-> @canvas :ctx .closePath))

(defn drawLine [position]
  (-> @canvas :ctx .beginPath)
  (.moveTo (:ctx @canvas) (-> position :start :x) (-> position :start :y))
  (.lineTo (:ctx @canvas) (-> position :curr :x) (-> position :curr :y))
  (set! (. (:ctx @canvas) -lineWidth) 1)
  (.stroke (:ctx @canvas))
  (-> @canvas :ctx .closePath))

(defn drawRectangle [position]
  (-> @canvas :ctx .beginPath)
  (.rect (:ctx @canvas)
         (js/Math.min (-> position :start :x) (-> position :curr :x))
         (js/Math.min (-> position :start :y) (-> position :curr :y))
         (js/Math.abs (- (-> position :start :x) (-> position :curr :x)))
         (js/Math.abs (- (-> position :start :y) (-> position :curr :y))))
  (set! (. (:ctx @canvas) -lineWidth) 1)
  (.stroke (:ctx @canvas))
  (-> @canvas :ctx .closePath))


(defn updatePosition [e]
  (swap! canvas update-in [:position] assoc :prev {
                                                   :x (-> @canvas :position :curr :x)
                                                   :y (-> @canvas :position :curr :y)
                                                    })
  (swap! canvas update-in [:position] assoc :curr {
                                                   :x (.-offsetX e)
                                                   :y (.-offsetY e)
                                                    }))

(defn movePencil []
  (swap! canvas update-in [:actions] conj (:position @canvas))
  (mapv drawPencil (:actions @canvas)))

(defn moveLine []
  (swap! canvas assoc :actions [(:position @canvas)])
  (mapv drawLine (:actions @canvas)))

(defn moveRectangle []
  (swap! canvas assoc :actions [(:position @canvas)])
  (mapv drawRectangle (:actions @canvas)))

(defn move [e]
  (if (true? (:draw @canvas))
    (do
      (updatePosition e)
      (.putImageData (:ctx @canvas) (-> @state :client-state :canvas) 0 0)
      (case (:draw-type @state)
        :pencil (movePencil)
        :line (moveLine)
        :rectangle (moveRectangle)))
      ))

(defn down [e]
  (updatePosition e)
  (swap! canvas update-in [:position] assoc :start {
                                                    :x (.-offsetX e)
                                                    :y (.-offsetY e)
                                                    })
  (swap! canvas assoc :draw true)
  )

(defn up []
  (if (true? (:draw @canvas)) (save))
  (swap! canvas assoc :draw false))

(defn desk-init []
  (let [canvas-element (.getElementById js/document "desk")
        ctx (.getContext canvas-element "2d")]
    (swap! canvas assoc :ctx ctx)
    (swap! state update-in [:server-state] assoc :canvas (.getImageData ctx 0 0 500 500))
    (swap! state update-in [:client-state] assoc :canvas (.getImageData ctx 0 0 500 500))
    (.addEventListener canvas-element "mousemove" move)
    (.addEventListener canvas-element "mousedown" down)
    (.addEventListener canvas-element "mouseup" up)
    (.addEventListener canvas-element "mouseout" up)))

(defn init [events]
  (desk-init)
  (js/setInterval #(GET (routes/get-events-url (:room @state) (:sync @state)) {:handler resolve-events :response-format :json :keywords? true}) 3000)
  (resolve-events events))

(defn send-message [message]
  (let [event {:sync-id nil :tag (uuid/uuid-string (uuid/make-random-uuid)) :method "show-message" :user (:user @state) :args {:text message}}]
    (POST (routes/post-event-url (:room @state)) {:params event :format :json :response-format :json :keywords? true})
    (resolve-event event)
    (swap! state update-in [:outgoing] conj event)
    ""))

(defn ^:export show-message [args id user tag]
  (if (some? id)
    (swap! state update-in [:server-state] update-in [:chat-log] conj {:sync-id id :tag tag :user user :text (:text args)}))
  (swap! state update-in [:client-state] update-in [:chat-log] conj {:sync-id id :tag tag :user user :text (:text args)}))

(defn ^:export pencil [args id]
  (mapv drawPencil (:actions args))
  (swap! state update-in [:client-state] assoc :canvas (.getImageData (:ctx @canvas) 0 0 500 500))
  (if (some? id)
    (swap! state update-in [:server-state] assoc :canvas (.getImageData (:ctx @canvas) 0 0 500 500))))

(defn ^:export line [args id]
  (mapv drawLine (:actions args))
  (swap! state update-in [:client-state] assoc :canvas (.getImageData (:ctx @canvas) 0 0 500 500))
  (if (some? id)
    (swap! state update-in [:server-state] assoc :canvas (.getImageData (:ctx @canvas) 0 0 500 500))))

(defn ^:export rectangle [args id]
  (mapv drawRectangle (:actions args))
  (swap! state update-in [:client-state] assoc :canvas (.getImageData (:ctx @canvas) 0 0 500 500))
  (if (some? id)
    (swap! state update-in [:server-state] assoc :canvas (.getImageData (:ctx @canvas) 0 0 500 500))))

(defn ^:export update-event [args id]
  (let [new-events (mapv (fn [item]
                          (if (= (str (:sync-id item)) (:sync-id args))
                            (assoc item :disabled (-> args :data :disabled))
                            item)) (:events @state))]
    (swap! state assoc :sync 0)
    (swap! state assoc :events [])
    (swap! state assoc :client-state {:chat-log []})
    (swap! state assoc :server-state {:chat-log []})
    (.clearRect (:ctx @canvas) 0 0 500 500)
    (swap! state update-in [:server-state] assoc :canvas (.getImageData (:ctx @canvas) 0 0 500 500))
    (swap! state update-in [:client-state] assoc :canvas (.getImageData (:ctx @canvas) 0 0 500 500))
    (resolve-events new-events)))

(defn set-user [user]
  (swap! state assoc :user user)
  "")

(defn chat-log []
  [:div [:h3 "Chat log:"]
   (for [message (:chat-log (:client-state @state))]
     ^{:key message} [:div (:user message) ": " (:text message) " (" (:sync-id message) " " (:tag message) ") "])])

(def menu-component
  [:div {:class "menu-container"}
   [:label [:input.radio {:field :radio :value :pencil :name :draw-type :checked true}] "Pencil"]
   [:label [:input.radio {:field :radio :value :line :name :draw-type}] "Line"]
   [:label [:input.radio {:field :radio :value :rectangle :name :draw-type}] "Rectangle"]])

(defn desk-component []
  [:div {:class "desk-container"} [:h3 "Drawing desk:"]
   [bind-fields menu-component state]
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
       [:input {:type      "text"
                :value     @message
                :required  ""
                :on-change #(reset! message (-> % .-target .-value))}]
       [:button {:on-click #(swap! message send-message)} "Send"]])))

(defn toggle-event [sync-id]
  (mapv (fn [item]
     (if (= (:sync-id item) sync-id)
       (POST (routes/patch-event-url (:room @state) sync-id) {:params {:disabled (not (:disabled item))} :format :json :response-format :json :keywords? true}))) (:events @state)))

(defn events-item []
  (fn [{:keys [sync-id user method args disabled]}]
    [:li
     [:div
      [:label [:input {:type "checkbox" :checked (not disabled) :on-change #(toggle-event sync-id)}]
       sync-id " " user ": " method " (" (subs (str args) 0 50) "...) "]]]))

(defn events-component []
  [:div {:class "events-container"} [:h3 "Events list:"]
   (for [event (:events @state)]
     ^{:key event} [events-item event])])

(defn index-page []
  (set-room)
  (GET (routes/get-events-url (:room @state)) {:handler init :response-format :json :keywords? true})
  (fn [] [:div [:h2 (:user @state) ", welcome to drawing desk"]
          [desk-component]
          [chat-component]
          [events-component]]))

(defn login-page []
  (let [user (reagent/atom "")]
    (fn []
      [:div {:class "login-container"} [:h2 "Please, enter your name:"]
       [:input {:type      "text"
                :value     @user
                :required  ""
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
