(ns shift-reset.core)

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

(defmulti transform
  (fn [& exprs]
    (first exprs)))

(defmethod transform 'if
  ([_ test then]
     (transform 'if test then nil))
  ([_ test then else]
     `(call (reset ~test)
            (fn [test#]
              (if test#
                (reset ~then)
                (reset ~else))))))

(defmethod transform 'do [_ & exprs]
  (if exprs
    `(call (reset ~(first exprs))
           (fn [_#] ~(apply transform 'do (next exprs))))))

(defmethod transform 'let* [_ bindings & exprs]
  (let [bindings (partition 2 bindings)]
    (reduce (fn [exprs [sym expr]]
              `(call (reset ~expr) (fn [~sym] ~exprs)))
            (apply transform 'do exprs)
            (reverse bindings))))

(defmethod transform 'fn* [_ & exprs]
  (cons 'fn* exprs))

(defn app [expr & exprs]
  (if exprs
    (let [arg (gensym)]
      `(call (reset ~(first exprs))
             (fn [~arg] ~(apply app (concat expr [arg]) (next exprs)))))
    expr))

(defmethod transform 'new [_ class & exprs]
  (apply app (list 'new class) exprs))

(defmethod transform nil [] '())

(defmethod transform :default [expr & exprs]
  `(call ~expr
         ~(let [expr (gensym)]
            `(fn [~expr]
               ~(apply app (list expr) exprs)))))

(defmacro reset [expr]
  (if (seq? expr)
    (apply transform (macroexpand expr))
    expr))

(defmacro shift [sym & body]
  `(Context. (fn [~sym] ~@body)))
