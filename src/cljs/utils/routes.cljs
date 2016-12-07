(ns utils.routes)

(def app-base-url "http://localhost:3000/")
(def api-base-url (str app-base-url "api/"))

(def api-events-url (str api-base-url "events/"))

(defn get-events-url
  ([room-id]
    (str api-events-url room-id))
  ([room-id sync-id]
    (str api-events-url room-id "/" sync-id)))

(defn post-event-url [room-id]
  (str api-events-url room-id))

(defn get-sign-in-url []
  (str app-base-url "sign-in"))

(defn patch-event-url [room-id event-id]
  (str api-events-url room-id "/" event-id))
