(ns utils.security
  (:require [ajax.core :as ajax]))

(def token (atom ""))

(def token-interceptor
  (ajax/to-interceptor {:name "Token Interceptor"
                   :request #(assoc-in % [:headers :Authorization] (str "Token " @token))}))
(swap! ajax/default-interceptors (partial cons token-interceptor))


