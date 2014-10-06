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

(defn transform [cont expr]
  (if (seq? expr)
    (apply application cont (macroexpand expr))
    (cont expr)))

(defmethod application 'if
  ([cont _ test then]
     (application cont 'if test then nil))
  ([cont _ test then else]
     (transform (fn [expr] `(call ~expr (fn [param#] (if param# ~(transform cont then) ~(transform cont else))))) test)))

(defmethod application 'do [cont _ & exprs]
  (if exprs
    (if (next exprs)
      (transform (fn [expr] `(call ~expr (fn [_#] ~(apply application cont 'do (next exprs))))) (first exprs))
      (transform cont (first exprs)))
    (cont nil)))

(defmethod application 'let* [cont _ bindings & exprs]
  (let [bindings (partition 2 bindings)]
    (reduce (fn [expr [param init]]
              (transform (fn [init] `(call ~init (fn [~param] ~expr))) init))
            (apply application cont 'do exprs)
            (reverse bindings))))

(defmethod application 'letfn* [cont _ & exprs]
  (apply application cont 'let* exprs))

(defmethod application 'loop* [cont _ & exprs]
  (apply application cont 'let* exprs))

(defmethod application 'throw [cont _ expr]
  (transform (fn [params] (cont `(throw ~params))) expr))

(defmethod application '. [cont _ expr method & exprs]
  (if (and (symbol? expr) (class? (resolve expr)))
    (apply application (fn [params] (cont `(. ~expr ~method ~@params))) exprs)
    (let [param (gensym)]
      (application (fn [param] (apply application (fn [params] (cont `(. ~@param ~method ~@exprs))) exprs)) expr))))

(defmethod application 'new [cont _ class & exprs]
  (apply application (fn [exprs] (cont `(new ~class ~@exprs))) exprs))

(defmethod application 'set! [cont _ symbol expr]
  (transform (fn [exprs] (cont `(set! ~symbol ~expr))) expr))

(defmethod application nil [cont]
  (cont '()))

(defmethod application :default [cont expr & exprs]
  (if (special-symbol? expr)
    (cont `(~expr ~@exprs))
    (let [param (gensym)]
      (if exprs
        (transform (fn [expr] `(call ~expr (fn [~param] ~(apply application (fn [params] (cont `(~param ~@params))) exprs)))) expr)
        (transform (fn [expr] `(call ~expr (fn [~param] ~(cont `(~param))))) expr)))))

(defmacro reset [& exprs]
  (transform identity `(identity (do ~@exprs))))

(defmacro shift [param & exprs]
  `(Context. (fn [~param] ~@exprs)))
