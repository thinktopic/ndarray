(ns thi.ng.ndarray.macros
  (:require [clojure.core.matrix.protocols :as mp]))

(defn- type-hinted
  [type x] (if type (with-meta x {:tag (name type)}) x))

(defn- do-cast
  [cast body]
  (if cast `(~cast ~body) body))

(defn- make-symbols
  [id n] (mapv #(symbol (str id %)) (range n)))

(defn- pair-fn
  [f coll]
  (let [coll (->> coll
                  (partition-all 2)
                  (map #(if (< 1 (count %)) (cons f %) (first %))))]
    (if (> (count coll) 2) (recur f coll) coll)))

(defn- make-indexer
  [dim ->st p]
  `(int (+ ~@(->> (range dim)
                  (map #(list '* (->st %) `(int (~p ~%))))
                  (cons '_offset)
                  (pair-fn '+)))))

(defn- make-indexer-syms
  [dim ->st ->p]
  `(int (+ ~@(->> (range dim)
                  (map #(list '* (->st %) `(int ~(->p %))))
                  (cons '_offset)
                  (pair-fn '+)))))

(defmacro and*
  "Like clojure.core/and, but avoids intermediate let bindings and
  only ever returns either result of last form (if all previous
  succeeded) or nil."
  ([x] x)
  ([x & more] `(if ~x (and* ~@more))))

(defn- with-bounds-check
  [dim psyms shapes clj? & body]
  `(if (and*
        ~@(mapcat
           #(let [p (symbol (psyms %))]
              (list `(>= ~p 0) `(< ~p ~(symbol (shapes %)))))
           (range dim)))
     (do ~@body)
     (throw
      (new ~(if clj? 'IndexOutOfBoundsException 'js/Error)
           (str "Invalid index: " (pr-str [~@psyms]))))))

(defn- for*
  [->a ->sh rdim body]
  `(for [~@(mapcat #(vector (->a %) `(range ~(->sh %))) rdim)] ~body))

(defn- reduce-1
  [f acc ->p ->sh ->st get data rdim clj? init?]
  (let [x    (->p 0)
        sx   (->sh 0)
        st   (->st 0)
        idx  (gensym)
        acc0 `(~get ~data ~'_offset)
        acc0 (if clj? `(num ~acc0) acc0)]
    `(if (zero? ~sx)
       ~(if init? nil `(~f))
       (loop ~(if init?
                [acc acc x 0 idx '_offset]
                [acc acc0 x 1 idx `(unchecked-add-int ~'_offset ~st)])
         (if (reduced? ~acc)
           @~acc
           (if (< ~x ~sx)
             (let [~acc (~f ~acc (~get ~data ~idx))]
               (recur ~acc (unchecked-inc-int ~x) (unchecked-add-int ~idx ~st)))
             (if (reduced? ~acc)
               @~acc ~acc)))))))

(defn- reduce-2
  [f acc ->p ->sh ->st get data rdim clj? init?]
  (let [[y x]     (map ->p rdim)
        [sy sx]   (map ->sh rdim)
        [sty stx] (map ->st rdim)
        idx       (gensym)
        acc0      `(~get ~data ~'_offset)
        acc0      (if clj? `(num ~acc0) acc0)]
    `(if (zero? (* ~sy ~sx))
       ~(if init? acc `(~f))
       (let [sy# (dec ~sy)]
         (loop ~(if init?
                  [acc acc y 0 x 0 idx '_offset]
                  [acc acc0 y 0 x 1 idx `(unchecked-add-int ~'_offset ~stx)])
           (if (reduced? ~acc)
             @~acc
             (if (< ~x ~sx)
               (recur (~f ~acc (~get ~data ~idx))
                      ~y
                      (unchecked-inc-int ~x)
                      (unchecked-add-int ~idx ~stx))
               (if (< ~y sy#)
                 (let [~idx (+ ~'_offset (* ~sty (unchecked-inc-int ~y)))]
                   (recur (~f ~acc (~get ~data ~idx))
                          (unchecked-inc-int ~y)
                          1
                          (unchecked-add-int ~idx ~stx)))
                 (if (reduced? ~acc)
                   @~acc ~acc)))))))))

(defn- reduce-3
  [f acc ->p ->sh ->st get data rdim clj? init?]
  (let [[z y x]       (map ->p rdim)
        [sz sy sx]    (map ->sh rdim)
        [stz sty stx] (map ->st rdim)
        idx           (gensym)
        acc0          `(~get ~data ~'_offset)
        acc0          (if clj? `(num ~acc0) acc0)]
    `(if (zero? (* (* ~sz ~sy) ~sx))
       ~(if init? acc `(~f))
       (let [sz# (dec ~sz) sy# (dec ~sy)]
         (loop ~(if init?
                  [acc acc z 0 y 0 x 0 idx '_offset]
                  [acc acc0 z 0 y 0 x 1 idx `(unchecked-add-int ~'_offset ~stx)])
           (if (reduced? ~acc)
             @~acc
             (if (< ~x ~sx)
               (recur (~f ~acc (~get ~data ~idx))
                      ~z
                      ~y
                      (unchecked-inc-int ~x)
                      (unchecked-add-int ~idx ~stx))
               (if (< ~y sy#)
                 (let [~idx (+ ~'_offset (+ (* ~stz ~z) (* ~sty (unchecked-inc-int ~y))))]
                   (recur (~f ~acc (~get ~data ~idx))
                          ~z
                          (unchecked-inc-int ~y)
                          1
                          (unchecked-add-int ~idx ~stx)))
                 (if (< ~z sz#)
                   (let [~idx (+ ~'_offset (* ~stz (unchecked-inc-int ~z)))]
                     (recur (~f ~acc (~get ~data ~idx))
                            (unchecked-inc-int ~z)
                            0
                            1
                            (unchecked-add-int ~idx ~stx)))
                   (if (reduced? ~acc)
                     @~acc ~acc))))))))))

(defn- reduce-4
  [f acc ->p ->sh ->st get data rdim clj? init?]
  (let [[w z y x]         (map ->p rdim)
        [sw sz sy sx]     (map ->sh rdim)
        [stw stz sty stx] (map ->st rdim)
        idx               (gensym)
        acc0              `(~get ~data ~'_offset)
        acc0              (if clj? `(num ~acc0) acc0)]
    `(if (zero? (* (* (* ~sw ~sz) ~sy) ~sx))
       ~(if init? acc `(~f))
       (let [sw# (dec ~sw) sz# (dec ~sz) sy# (dec ~sy)]
         (loop ~(if init?
                  [acc acc w 0 z 0 y 0 x 0 idx '_offset]
                  [acc acc0 w 0 z 0 y 0 x 1 idx `(unchecked-add-int ~'_offset ~stx)])
           (if (reduced? ~acc)
             @~acc
             (if (< ~x ~sx)
               (recur (~f ~acc (~get ~data ~idx))
                      ~w
                      ~z
                      ~y
                      (unchecked-inc-int ~x)
                      (unchecked-add-int ~idx ~stx))
               (if (< ~y sy#)
                 (let [~idx (+ ~'_offset (+ (* ~stw ~w) (+ (* ~stz ~z) (* ~sty (unchecked-inc-int ~y)))))]
                   (recur (~f ~acc (~get ~data ~idx))
                          ~w
                          ~z
                          (unchecked-inc-int ~y)
                          1
                          (unchecked-add-int ~idx ~stx)))
                 (if (< ~z sz#)
                   (let [~idx (+ ~'_offset (+ (* ~stw ~w) (* ~stz (unchecked-inc-int ~z))))]
                     (recur (~f ~acc (~get ~data ~idx))
                            ~w
                            (unchecked-inc-int ~z)
                            0
                            1
                            (unchecked-add-int ~idx ~stx)))
                   (if (< ~w sw#)
                     (let [~idx (+ ~'_offset (* ~stw (unchecked-inc-int ~w)))]
                       (recur (~f ~acc (~get ~data ~idx))
                              (unchecked-inc-int ~w)
                              0
                              0
                              1
                              (unchecked-add-int ~idx ~stx)))
                     (if (reduced? ~acc)
                       @~acc ~acc)))))))))))

(def ^:private reduce-impls
  [nil reduce-1 reduce-2 reduce-3 reduce-4])

(defn- inject-clj-protos
  [clj? get data ->a ->sh ->st idx rdim]
  (let [[f init] (repeatedly gensym)
        r-impl   (reduce-impls (count rdim))
        reduce*  (partial r-impl f init ->a ->sh ->st get data rdim clj?)]
    (if clj?
      (list
       'clojure.lang.Seqable
       `(~'seq
         [_#]
         ~(for* ->a ->sh rdim `(~get ~data ~idx)))
       'clojure.core.protocols/CollReduce
       `(~'coll-reduce
         [_# ~f] ~(reduce* false))
       `(~'coll-reduce
         [_# ~f ~init] ~(reduce* true)))
      (list
       'ISeqable
       `(~'-seq
         [_#]
         ~(for* ->a ->sh rdim `(~get ~data ~idx)))
       'IReduce
       `(~'-reduce
         [_# ~f] ~(reduce* false))
       `(~'-reduce
         [_# ~f ~init] ~(reduce* true))))))

(defmacro def-ndarray
  [dim cast type-hint type-id data-ctor get set & [clj?]]
  (let [type-name (symbol (str "NDArray" dim (name type-id)))
        raw-name  (symbol (str "make-raw-ndarray" dim "-" (name type-id)))
        strides   (make-symbols "_stride" dim)
        shapes    (make-symbols "_shape" dim)
        asyms     (make-symbols "a" dim)
        bsyms     (make-symbols "b" dim)
        psyms     (make-symbols "p" dim)
        [->st ->sh ->a ->b ->p] (map #(comp symbol %) [strides shapes asyms bsyms psyms])
        [c d f p o] (repeatedly gensym)
        idx       (make-indexer dim ->st p)
        idx-syms  (make-indexer-syms dim ->st ->p)
        data      (type-hinted type-hint '_data)
        rdim      (range dim)]
    `(do
       (deftype ~type-name
           [~data ~'_offset ~@strides ~@shapes]
         ~@(inject-clj-protos clj? get data ->a ->sh ->st (make-indexer-syms dim ->st ->a) rdim)
         ~'thi.ng.ndarray.core/PNDArray
         (~'data
           [_#] ~data)
         (~'data-type
           [_#] ~type-id)
         (~'dimension
           [_#] ~dim)
         (~'stride
           [_#] [~@strides])
         (~'shape
           [_#] [~@shapes])
         (~'offset
           [_#] ~'_offset)
         (~'size
           [_#] (* ~@(pair-fn '* shapes)))
         (~'extract
           [_#]
           (let [buf#      ~(type-hinted type-hint `(~data-ctor (* ~@(pair-fn '* shapes))))
                 [~@asyms] (thi.ng.ndarray.core/shape->stride [~@shapes])
                 arr#      (new ~type-name buf# 0 ~@asyms ~@shapes)]
             (loop [~c (thi.ng.ndarray.core/index-seq _#)
                    ~d (thi.ng.ndarray.core/index-seq arr#)]
               (when ~c
                 (~set buf# (int (first ~d)) ~(do-cast cast `(~get ~data (int (first ~c)))))
                 (recur (next ~c) (next ~d))))
             arr#))
         (~'index-at
           [_# ~@psyms] ~idx-syms)
         (~'index-pos
           [_# ~p]
           (let [~p (int ~p)
                 ~c (- ~p ~'_offset)
                 ~@(drop-last
                    2 (mapcat
                       #(let [a (->a %) s (->st %)]
                          (list a `(int (/ ~c ~s))
                                c `(- ~c (* ~a ~s))))
                       rdim))]
             [~@asyms]))
         (~'index-seq
           [_#]
           ~(let [idx (make-indexer-syms dim ->st ->a)]
              (for* ->a ->sh rdim idx)))
         (~'position-seq
           [_#] ~(for* ->a ->sh rdim `[~@asyms]))
         (~'get-at
           [_# ~@psyms] (~get ~data ~idx-syms))
         (~'get-at-safe
           [_# ~@psyms]
           ~(with-bounds-check dim psyms shapes clj?
              `(~get ~data ~idx-syms)))
         (~'get-at-index
           [_# i#] (~get ~data (int i#)))
         (~'set-at
           [_# ~@psyms ~c] (~set ~data ~idx-syms ~(do-cast cast c)) _#)
         (~'set-at-safe
           [_# ~@psyms ~c]
           ~(with-bounds-check dim psyms shapes clj?
              `(~set ~data ~idx-syms ~(do-cast cast c)))
           _#)
         (~'set-at-index
           [_# i# ~c] (~set ~data (int i#) ~(do-cast cast c)) _#)
         (~'update-at
           [_# ~@psyms ~f]
           (let [~c ~idx-syms]
             (~set ~data ~c ~(do-cast cast `(~f ~@psyms (~get ~data ~c)))))
           _#)
         (~'update-at-safe
           [_# ~@psyms ~f]
           ~(with-bounds-check dim psyms shapes clj?
              `(let [~c ~idx-syms]
                 (~set ~data ~c ~(do-cast cast `(~f ~@psyms (~get ~data ~c))))))
           _#)
         (~'update-at-index
           [_# ~c ~f] (~set ~data ~c ~(do-cast cast `(~f ~c (~get ~data (int ~c))))) _#)
         (~'truncate-h
           [_# ~@psyms]
           (new ~type-name ~data ~'_offset ~@strides
                ~@(map
                   #(let [p (->p %) s (->sh %)]
                      `(if (number? ~p)
                         (if (neg? ~p)
                           (+ ~s (int ~p))
                           (int ~p))
                         ~s))
                   rdim)))
         (~'truncate-l
           [_# ~@psyms]
           (let [~@(mapcat
                    #(let [p (->p %) sh (->sh %) st (->st %)]
                       (list
                        [(->a %) (->b %)]
                        `(if (pos? ~p)
                           [(- ~sh (int ~p))
                            (* ~st (int ~p))]
                           [~sh 0])))
                    rdim)
                 ~o (+ ~@(->> rdim (map ->b) (cons '_offset) (pair-fn '+)))]
             (new ~type-name ~data ~o ~@strides ~@asyms)))
         (~'transpose
           [_# ~@psyms]
           (let [~@(mapcat #(let [p (->p %)] (list p `(if ~p (int ~p) ~%))) rdim)
                 ~c [~@strides]
                 ~d [~@shapes]]
             (new ~type-name ~data ~'_offset
                  ~@(map #(list c (->p %)) rdim)
                  ~@(map #(list d (->p %)) rdim))))
         (~'step
           [_# ~@psyms]
           (let [~o ~'_offset
                 ~@(mapcat
                    #(let [p (->p %) sh (->sh %) st (->st %)
                           stride' `(* ~st (int ~p))]
                       (list
                        [(->a %) (->b %) o]
                        `(if (number? ~p)
                           (if (neg? ~p)
                             [(int (~'Math/ceil (/ (- ~sh) (int ~p))))
                              ~stride'
                              (+ ~o (* ~st (dec ~sh)))]
                             [(int (~'Math/ceil (/ ~sh (int ~p))))
                              ~stride'
                              ~o])
                           [~sh ~st ~o])))
                    rdim)]
             (new ~type-name ~data ~o ~@bsyms ~@asyms)))
         (~'pick
           [_# ~@psyms]
           (let [~o ~'_offset, ~c [], ~d []
                 ~@(mapcat
                    #(let [p (->p %) sh (->sh %) st (->st %)]
                       (list
                        [c d o]
                        `(if (and (number? ~p) (>= ~p 0))
                           [~c ~d (+ ~o (* ~st (int ~p)))]
                           [(conj ~c ~sh) (conj ~d ~st) ~o])))
                    rdim)
                 cnt# (count ~c)]
             (if (pos? cnt#)
               ((get-in @~'thi.ng.ndarray.core/ctor-registry [cnt# ~type-id :ctor]) ~data ~o ~d ~c)
               (~get ~data (int ~o)))))
         ~'Object
         (~'toString
           [_#]
           (pr-str
            {:data ~data :type ~type-id
             :size (* ~@(pair-fn '* shapes)) :total (count (seq _#)) :offset ~'_offset
             :shape [~@shapes] :stride [~@strides]}))

         ~'clojure.core.matrix.protocols/PImplementation
         (~'implementation-key [_#] :ndarray)
         (~'meta-info [_#] {:doc "thi.ng.ndarray matrix"})
         (~'construct-matrix [_# data#]
           (let [dims# (long (mp/dimensionality data#))]
             (cond
               (== dims# 0) (mp/get-0d data#)
               :default (~'thi.ng.ndarray.core/ndarray ~type-id (mat/eseq data#) (mat/shape data#)))))
         (~'new-vector [_# length#]
           (let [r# (~'thi.ng.ndarray.core/ndarray ~type-id (~data-ctor length#) [length#])]
             (dotimes [i# length#] (aset r# i# 0.0))
             r#))
         (~'new-matrix [_# rows# columns#]
           (let [r# (~'thi.ng.ndarray.core/ndarray ~type-id (~data-ctor (* rows# columns#)) [rows# columns#])]
             (dotimes [i# (* rows# columns#)] (aset r# i# 0.0))
             r#))
         (~'new-matrix-nd [_# shape#]
           (if-let [shape# (seq shape#)]
             (~'thi.ng.ndarray.core/ndarray ~type-id (~data-ctor (apply * shape#)) shape#)
             0.0))
         (~'supports-dimensionality? [_# dimensions#]
           (<= dimensions# 4))

         ~'clojure.core.matrix.protocols/PDimensionInfo
         (~'dimensionality [_#] (count [~@shapes]))
         (~'get-shape [_#] [~@shapes])
         (~'is-scalar? [_#] false)
         (~'is-vector? [_#] (= 1 ~dim))
         (~'dimension-count [_# dimension-number#]
           (nth [~@shapes] dimension-number#))

         ~'clojure.core.matrix.protocols/PIndexedAccess
         (~'get-1d [m# i#] (~'get-at m# i#))
         (~'get-2d [m# row# column#] (~'get-at m# row# column#))
         (~'get-nd [m# indexes#] (apply ~'get-at m# indexes#))

         ~'clojure.core.matrix.protocols/PIndexedSetting
         (~'set-1d [m# i# x#] (~'set-at (~'mat/clone m#) i# x#))

         (~'set-2d [m# row# column# x#]
           (~'set-at (~'mat/clone m#) row# column# x#))

         (~'set-nd [m# indexes# v#]
           (let [new-m# (~'mat/clone m#)]
             (apply ~'set-at new-m# (concat indexes# [v#]))
             new-m#))

         (~'is-mutable? [m#] true)

         ~'clojure.core.matrix.protocols/PIndexedSettingMutable
           (~'set-1d! [m# x# v#]
             (~'set-at m# x# v#))

           (~'set-2d! [m# x# y# v#]
             (~'set-at m# x# y# v#))

           (~'set-nd! [m# indexes# v#]
             (apply ~'set-at m# (concat indexes# [v#]))
             m#)

         ~'clojure.core.matrix.protocols/PMatrixCloning
         (~'clone [m#]
           (let [cloned-ary# (.slice ~data 0)]
             (~'thi.ng.ndarray.core/ndarray ~type-id cloned-ary# [~@shapes])))

         ~'clojure.core.matrix.protocols/PTypeInfo
         (~'element-type [_#] ~type-id)

         ~'clojure.core.matrix.protocols/PFunctionalOperations
         (~'element-seq [_#] (cljs.core/IndexedSeq. ~data 0))

         ~'clojure.core.matrix.protocols/PVectorView
         (~'as-vector [_#]
           (~'thi.ng.ndarray.core/ndarray ~type-id ~data [(apply * [~@shapes])]))

         ~'clojure.core.matrix.protocols/PMatrixEquality
         (~'matrix-equals [a# b#]
           (cond
             (identical? a# b#) true
             (mp/same-shape? a# b#)
             (if (== 0 (long (mp/dimensionality a#)))
               (== (mp/get-0d a#) (clojure.core.matrix.macros/scalar-coerce b#))
               (not-any? false? (map == (mp/element-seq a#) (mp/element-seq b#))))
             :else false))

         ~'clojure.core.matrix.protocols/PValueEquality
         (~'value-equals [a# b#]
           (and
             (mp/same-shape? a# b#)
             (every? true? (map = (mp/element-seq a#) (mp/element-seq b#)))))

         ~'clojure.core.matrix.protocols/PNumerical
         (~'numerical? [_#] true)
         )

       (defn ~(with-meta raw-name {:export true})
         [data# o# [~@strides] [~@shapes]]
         (new ~type-name data# o# ~@strides ~@shapes))

       (swap!
        ~'thi.ng.ndarray.core/ctor-registry
        assoc-in [~dim ~type-id]
        {:ctor ~raw-name
         :data-ctor ~data-ctor}))))

