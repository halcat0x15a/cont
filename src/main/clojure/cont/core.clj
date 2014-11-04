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
  (fn [cont ctx & exprs]
    (first exprs)))

(declare transform)

(defn cps
  ([cont ctx] (cont ()))
  ([cont ctx expr & exprs]
     (if exprs
       (transform (fn [e] (apply cps (fn [es] (cont `(~e ~@es))) ctx exprs)) ctx expr)
       (transform (fn [e] (cont `(~e))) ctx expr))))

(defn transform [cont ctx expr]
    (cond (symbol? expr) (let [a (gensym)] `(call ~expr (fn* [~a] ~(cont a))))
          (seq? expr) (apply application cont ctx (macroexpand expr))
          (vector? expr) (apply cps #(cont (vec %)) ctx expr)
          (map? expr) (apply cps #(cont (into {} %)) ctx expr)
          :else (cont expr)))

(defmethod application 'if
  ([cont ctx _ test then]
     (application cont ctx 'if test then nil))
  ([cont ctx _ test then else]
     (transform (fn [e] `(if ~e ~(transform cont ctx then) ~(transform cont ctx else))) ctx test)))

(defmethod application 'do [cont ctx _ & exprs]
  (if exprs
    (if (next exprs)
      (transform (fn [_] `(let* [~@(interleave (keys ctx) (vals ctx))] ~(apply application cont ctx 'do (next exprs)))) ctx (first exprs))
      (transform cont ctx (first exprs)))
    (cont nil)))

(defmethod application 'let* [cont ctx _ bindings & exprs]
  (if (seq bindings)
    (transform (fn [e] `(let* [~(first bindings) ~e] ~(apply application cont (assoc ctx (first bindings) e) 'let* (nnext bindings) exprs))) ctx (second bindings))
    (apply application cont ctx 'do exprs)))

(defmethod application 'letfn* [cont ctx _ & exprs]
  (apply application cont ctx 'let* exprs))

(defmethod application 'loop* [cont ctx _ & exprs]
  (apply application cont ctx 'let* exprs))

(defmethod application 'throw [cont ctx _ expr]
  (transform (fn [e] (cont `(throw ~e))) ctx expr))

(defmethod application '. [cont ctx _ expr method & exprs]
  (let [a (gensym)]
    (if (and (symbol? expr) (class? (resolve expr)))
      (apply cps (fn [es] `(call (. ~expr ~method ~@es) (fn* [~a] ~(cont a)))) ctx exprs)
      (transform (fn [e] (apply cps (fn [es] `(call (. ~e ~method ~@es) (fn* [~a] ~(cont a)))) ctx exprs)) ctx expr))))

(defmethod application 'new [cont ctx _ class & exprs]
  (let [a (gensym)]
    (apply cps (fn [es] `(call (new ~class ~@es) (fn* [~a] ~(cont a)))) ctx exprs)))

(defmethod application 'set! [cont ctx _ symbol expr]
  (transform (fn [e] (cont `(set! ~symbol ~e))) ctx expr))

(defmethod application nil [cont]
  (cont ()))

(defmethod application :default [cont ctx expr & exprs]
  (if (special-symbol? expr)
    (cont `(~expr ~@exprs))
    (let [a (gensym)]
      (apply cps (fn [e] `(call ~e (fn* [~a] ~(cont a)))) ctx expr exprs))))

(defmacro reset [& exprs]
  (transform identity {} `(do ~@exprs)))

(defmacro shift [param & exprs]
  `(Context. (fn* [~param] ~@exprs)))
