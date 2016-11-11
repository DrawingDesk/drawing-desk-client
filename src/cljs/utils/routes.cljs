(ns utils.routes)

(def api-base-url "http://localhost:3000/api/")

(def api-events-url (str api-base-url "events/"))

(defn get-events-url
  ([room-id]
    (str api-events-url room-id))
  ([room-id sync-id]
    (str api-events-url room-id "/" sync-id)))

(defn post-event-url [room-id]
  (str api-events-url room-id))