(def list
  (fn [& xs] xs))

(def cons (fn [x l] (RT/listCons x l)))

(def type (fn [x] (RT/type x)))

(def inc (fn [n] (RT/plus 1 n)))
(def dec (fn [n] (RT/plus -1 n)))

(def =
  (fn [a b]
    (RT/eq a b)))

(def take
  (fn take [n coll]
    (if (= 0 n)
      ()
      (cons (RT/listFirst coll)
            (take (dec n) (RT/listRest coll))))))

(def range
  (fn range
    ([] ((fn f [n] (cons n (f (inc n)))) 0))
    ([n] (take n (range)))
    ([start end]
       (if (= start end)
         ()
         (cons start (range (inc start) end))))))

(def reverse
  (fn [coll]
    ((fn recur [coll acc]
       (if (= coll ())
         acc
         (recur (RT/listRest coll) (cons (RT/listFirst coll) acc))))
     coll ())))

(defmacro comment [& xs] nil)

(def not (fn [x] (if x false true)))

(def first
  (fn [coll]
    (if (= (quote List) (type coll))
      (RT/listFirst coll)
      (if (= (quote Vector) (type coll))
        (if (not (= [] coll))
          (RT/vectorNth coll 0))))))

(def rest
  (fn [coll]
    (if (= () coll)
      nil
      (if (= [] coll)
        nil
        (if (= (quote List) (type coll))
          (RT/listRest coll)
          (if (= (quote Vector) (type coll))
            (map (fn [i] (RT/vectorNth coll i)) (range 1 (RT/vectorCount coll)))))))))

(def reduce
  (fn reduce
    ([f coll]
       (if (= () coll)
         (f)
         (reduce f (first coll) (rest coll))))
    ([f x coll]
       (if (= () coll)
         x
         (reduce f (f x (first coll)) (rest coll))))))

(def +
  (fn [& xs]
    (reduce (fn [a b] (RT/plus a b)) 0 xs)))

(def *
  (fn [& xs]
    (reduce (fn [a b] (RT/times a b)) 1 xs)))

(def vec
  (fn [xs]
    (reduce (fn [v x] (RT/vectorConj v x)) [] xs)))

(def vector (fn [& args] (vec args)))

(def list*
  (fn list*
    ([xs] (RT/apply list xs))
    ([x & args] (cons x (RT/apply list* args)))))

(def apply
  (fn
    ([f args] (RT/apply f args))
    ([f & moreargs] (RT/apply f (RT/apply list* moreargs)))))

(def concat
  (fn concat [list1 list2]
    (if (= () list1)
      list2
      (cons (first list1) (concat (RT/listRest list1) list2)))))

(def partial
  (fn [f & args]
    (fn [& args2]
      (apply f (concat args args2)))))

(def count
  (fn count [coll]
    (if (= (quote Vector) (type coll))
      (RT/vectorCount coll)
      (if (= (quote List) (type coll))
        (if (= () coll)
          0
          (+ 1 (count (RT/listRest coll))))))))

(def map
  (fn map [f list]
    (if (= () list)
      ()
      (cons (f (first list)) (map f (rest list))))))

;; haha this is terrible
(def vec2list
  (fn [vec]
    (map (fn [i] (RT/vectorNth vec i)) (range (count vec)))))


(defmacro let
  [bindings expr]
  ((fn let
     [bindings]
     (if (= () bindings)
       expr
       (list
        (list (quote fn)
              (vector (first bindings))
              (let (rest (rest bindings))))
        (first (rest bindings)))))
   (vec2list bindings)))

(defmacro and
  [& exprs]
  (if (= () exprs)
    true
    (if (= () (rest exprs))
      (first exprs)
      (list (quote let)
            (vector (quote val) (first exprs))
            (list (quote if)
                  (quote val)
                  (list* (quote and) (rest exprs))
                  (quote val))))))

(defmacro or
  [& exprs]
  (if (= () exprs)
    false
    (if (= () (rest exprs))
      (first exprs)
      (list (quote let)
            (vector (quote val) (first exprs))
            (list (quote if)
                  (quote val)
                  (quote val)
                  (list* (quote or) (rest exprs)))))))

(def assoc
  (fn assoc
    ([m k v] (RT/mapAssoc m k v))
    ([m k v & kvs]
       (apply assoc (assoc m k v) kvs))))

(def dissoc
  (fn [m & ks] (reduce (fn [m k] (RT/mapDissoc m k)) m ks)))

(def get
  (fn [m k] (RT/mapGet m k)))
