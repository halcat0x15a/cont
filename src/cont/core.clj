(ns cont.core)

(defprotocol Continuation
  (call [cont k]))

(extend-protocol Continuation
  Object
  (call [obj k] (k obj))
  nil
  (call [_ k] (k nil)))

(deftype Context [f]
  Continuation
  (call [ctx k]
    (f k)))

(declare reset)

(defmacro pass [cont param expr]
  `(call (reset ~cont) (fn [~param] ~expr)))

(defmulti transform
  (fn [& exprs]
    (first exprs)))

(defmethod transform 'if
  ([_ test then]
     (transform 'if test then nil))
  ([_ test then else]
     `(if (reset ~test) (reset ~then) (reset ~else))))

(defmethod transform 'do [_ & exprs]
  (if exprs
    (if (next exprs)
      `(pass ~(first exprs) _# ~(apply transform 'do (next exprs)))
      (first exprs))))

(defmethod transform 'let* [_ bindings & exprs]
  (let [bindings (partition 2 bindings)]
    (reduce (fn [exprs [param expr]]
              `(pass ~expr ~param ~exprs))
            (apply transform 'do exprs)
            (reverse bindings))))

(defmethod transform 'letfn* [_ & exprs]
  (apply transform 'let* exprs))

(defmethod transform 'loop* [_ & exprs]
  (apply transform 'let* exprs))

(defmethod transform 'throw [_ expr]
  `(throw (reset ~expr)))

(defn app [expr & exprs]
  (if exprs
    (let [param (gensym)]
      `(pass ~(first exprs) ~param ~(apply app (concat expr [param]) (next exprs))))
    expr))

(defn tagged? [symbol expr]
  (and (seq? expr) (= symbol (first expr))))

(defmethod transform 'try [_ & exprs]
  (let [[exprs clauses] (split-with #(not (or (tagged? 'catch %) (tagged? 'finally %))) exprs)]
    `(try ~(apply transform 'do exprs) ~@(map #(apply transform %) clauses))))

(defmethod transform 'catch [_ class param & exprs]
  `(catch ~class ~param ~(apply transform 'do exprs)))

(defmethod transform 'finally [_ & exprs]
  `(finally ~(apply transform 'do exprs)))

(defmethod transform '. [_ expr method & exprs]
  (if (and (symbol? expr) (class? (resolve expr)))
    (apply app (list '. expr method) exprs)
    (let [param (gensym)]
      `(pass ~expr ~param ~(apply app (list '. param method) exprs)))))

(defmethod transform 'new [_ class & exprs]
  (apply app (list 'new class) exprs))

(defmethod transform 'set! [_ symbol expr]
  `(set! ~symbol (reset ~expr)))

(defmethod transform nil [] '())

(defmethod transform :default [expr & exprs]
  (if (special-symbol? expr)
    (cons expr exprs)
    (let [param (gensym)]
      `(pass ~expr ~param ~(apply app (list param) exprs)))))

(defmacro reset [expr]
  (if (seq? expr)
    (apply transform (macroexpand expr))
    expr))

(defmacro shift [param & body]
  `(Context. (fn [~param] ~@body)))
