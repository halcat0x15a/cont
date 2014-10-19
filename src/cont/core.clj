(ns cont.core)

(defprotocol Continuation
  (call [this k]))

(extend-protocol Continuation
  Object
  (call [obj k]
    (k obj))
  nil
  (call [_ k]
    (k nil)))

(deftype Context [cc]
  Continuation
  (call [ctx k]
    (cc k)))

(defmulti application
  (fn [cont & exprs]
    (first exprs)))

(declare transform)

(defn cps
  ([cont] (cont ()))
  ([cont expr & exprs]
     (if exprs
       (transform (fn [e] (apply cps (fn [es] (cont `(~e ~@es))) exprs)) expr)
       (transform (fn [e] (cont `(~e))) expr))))

(defn transform [cont expr]
    (cond (symbol? expr) (let [a (gensym)] `(call ~expr (fn* [~a] ~(cont a))))
          (seq? expr) (apply application cont (macroexpand expr))
          (vector? expr) (apply cps #(cont (vec %)) expr)
          (map? expr) (apply cps #(cont (into {} %)) expr)
          :else (cont expr)))

(defmethod application 'if
  ([cont _ test then]
     (application cont 'if test then nil))
  ([cont _ test then else]
     (transform (fn [e] `(if ~e ~(transform cont then) ~(transform cont else))) test)))

(defmethod application 'do [cont _ & exprs]
  (if exprs
    (if (next exprs)
      (transform (fn [_] (apply application cont 'do (next exprs))) (first exprs))
      (transform cont (first exprs)))
    (cont nil)))

(defmethod application 'let* [cont _ bindings & exprs]
  (if (seq bindings)
    (transform (fn [e] `(let* [~(first bindings) ~e] ~(apply application cont 'let* (nnext bindings) exprs))) (second bindings))
    (apply application cont 'do exprs)))

(defmethod application 'letfn* [cont _ & exprs]
  (apply application cont 'let* exprs))

(defmethod application 'loop* [cont _ & exprs]
  (apply application cont 'let* exprs))

(defmethod application 'throw [cont _ expr]
  (transform (fn [e] (cont `(throw ~e))) expr))

(defmethod application '. [cont _ expr method & exprs]
  (let [a (gensym)]
    (if (and (symbol? expr) (class? (resolve expr)))
      (apply cps (fn [es] `(call (. ~expr ~method ~@es) (fn* [~a] ~(cont a)))) exprs)
      (transform (fn [e] (apply cps (fn [es] `(call (. ~e ~method ~@es) (fn* [~a] ~(cont a)))) exprs)) expr))))

(defmethod application 'new [cont _ class & exprs]
  (let [a (gensym)]
    (apply cps (fn [es] `(call (new ~class ~@es) (fn* [~a] ~(cont a)))) exprs)))

(defmethod application 'set! [cont _ symbol expr]
  (transform (fn [e] (cont `(set! ~symbol ~e))) expr))

(defmethod application nil [cont]
  (cont ()))

(defmethod application :default [cont expr & exprs]
  (if (special-symbol? expr)
    (cont `(~expr ~@exprs))
    (let [a (gensym)]
      (apply cps (fn [e] `(call ~e (fn* [~a] ~(cont a)))) expr exprs))))

(defmacro reset [& exprs]
  (transform identity `(do ~@exprs)))

(defmacro shift [param & exprs]
  `(Context. (fn* [~param] ~@exprs)))
