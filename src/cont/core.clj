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
  (fn [env cont & exprs]
    (first exprs)))

(declare transform)

(defn cps
  ([cont env] (cont ()))
  ([cont env expr & exprs]
     (if exprs
       (transform (fn [e] (apply cps (fn [es] (cont `(~e ~@es))) env exprs)) env expr)
       (transform (fn [e] (cont `(~e))) env expr))))

(defn transform [cont env expr]
    (cond (and (symbol? expr) (contains? env expr)) (cont (get env expr))
          (symbol? expr) (let [e (gensym expr)] `(call ~expr (fn* [~e] ~(cont e))))
          (seq? expr) (apply application cont env (macroexpand expr))
          (vector? expr) (apply cps #(cont (vec %)) env expr)
          (map? expr) (apply cps #(cont (into {} %)) env expr)
          :else (cont expr)))

(defmethod application 'if
  ([cont env _ test then]
     (application cont env 'if test then nil))
  ([cont env _ test then else]
     (transform (fn [e] `(if ~e ~(transform cont env then) ~(transform cont env else))) env test)))

(defmethod application 'do [cont env _ & exprs]
  (if exprs
    (if (next exprs)
      (transform (fn [_] (apply application cont env 'do (next exprs))) env (first exprs))
      (transform cont env (first exprs)))
    (cont nil)))

(defmethod application 'let* [cont env _ bindings & exprs]
  (if (seq bindings)
    (transform (fn [e] (apply application cont (assoc env (first bindings) e) 'let* (nnext bindings) exprs)) env (second bindings))
    (apply application cont env 'do exprs)))

(defmethod application 'letfn* [cont env _ & exprs]
  (apply application cont env 'let* exprs))

(defmethod application 'loop* [cont env _ & exprs]
  (apply application cont env 'let* exprs))

(defmethod application 'throw [cont env _ expr]
  (transform (fn [e] (cont `(throw ~e))) env expr))

(defmethod application '. [cont env _ expr method & exprs]
  (let [a (gensym)]
    (if (and (symbol? expr) (class? (resolve expr)))
      (apply cps (fn [es] `(call (. ~expr ~method ~@es) (fn* [~a] ~(cont a)))) env exprs)
      (transform (fn [e] (apply cps (fn [es] `(call (. ~e ~method ~@es) (fn* [~a] ~(cont a)))) env exprs)) env expr))))

(defmethod application 'new [cont env _ class & exprs]
  (let [a (gensym)]
    (apply cps (fn [es] `(call (new ~class ~@es) (fn* [~a] ~(cont a)))) env exprs)))

(defmethod application 'set! [cont env _ symbol expr]
  (transform (fn [e] (cont `(set! ~symbol ~e))) env expr))

(defmethod application nil [cont]
  (cont ()))

(defmethod application :default [cont env expr & exprs]
  (if (special-symbol? expr)
    (cont `(~expr ~@exprs))
    (let [a (gensym)]
      (apply cps (fn [e] `(call ~e (fn* [~a] ~(cont a)))) env expr exprs))))

(defmacro reset [& exprs]
  (transform identity {} `(do ~@exprs)))

(defmacro shift [param & exprs]
  `(Context. (fn* [~param] ~@exprs)))
