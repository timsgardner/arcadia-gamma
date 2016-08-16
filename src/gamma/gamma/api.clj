(ns gamma.api
  (:refer-clojure
   :exclude [aget
             not
             not=
             or
             set
             *
             +
             -
             <
             >
             <=
             >=
             ==
             max
             mod
             and
             if
             for

             int
             float])
  (:require [gamma.ast :as ast]))

(def standard-functions
  (group-by :name
            (map
             (fn [x]
               {:name      (x 1)
                :input     (vec (take-nth 2 (x 2)))
                :output    (x 0)
                :parameter {:T #{:float :vec2 :vec3 :vec4}}})
             [[:int :int [:T :x]]
              [:float :float [:T :x]]
              [:bool :bool [:T :x]]

              [:T :radians [:T :degrees]]
              [:T :degrees [:T :radians]]
              [:T :sin [:T :angle]]
              [:T :cos [:T :angle]]
              [:T :tan [:T :angle]]
              [:T :asin [:T :x]]
              [:T :acos [:T :x]]
              [:T :atan [:T :y :T :x]]
              [:T :atan [:T :y_over_x]]

              [:T :pow [:T :x :T :y]]
              [:T :exp [:T :x]]
              [:T :log [:T :x]]
              [:T :exp2 [:T :x]]
              [:T :log2 [:T :x]]
              [:T :sqrt [:T :x]]
              [:T :inversesqrt [:T :x]]

              [:T :abs [:T :x]]
              [:T :sign [:T :x]]
              [:T :floor [:T :x]]
              [:T :ceil [:T :x]]
              [:T :fract [:T :x]]
              [:T :mod [:T :x :T :y]]
              [:T :max [:T :x :float :y]]
              [:T :clamp [:T :x :T :minVal :T :maxVal]]
              [:T :clamp [:T :x :float :minVal :float :maxVal]]
              [:T :step [:T :edge :T :x]]
              [:T :step [:float :edge :T :x]]
              [:T :smoothstep [:T :edge0 :T :edge1 :T :x]]
              [:T :smoothstep [:float :edge0 :float :edge1 :T :x]]

              [:float :length [:T :x]]
              [:float :distance [:T :p0 :T :p1]]
              [:float :dot [:T :x :T :y]]
              [:vec3 :cross [:vec3 :x :vec3 :y]]
              [:T :normalize [:T :x]]
              [:T :faceforward [:T :N :T :I :T :Nref]]
              [:T :reflect [:T :I :T :N]]
              [:T :refract [:T :I :T :N :float :eta]]

              [:vec4 :texture2DLod [:sampler2D :sampler :vec2 :coord :float :lod]]
              [:vec4 :texture2DProjLod [:sampler2D :sampler :vec3 :coord :float :lod]]
              [:vec4 :texture2DProjLod [:sampler2D :sampler :vec4 :coord :float :lod]]
              [:vec4 :textureCubeLod [:sampler2D :sampler :vec4 :coord :float :lod]]
              ;; available only in fragement shaders
              [:vec4 :texture2D [:sampler2D :sampler :vec2 :coord :float :bias]]
              [:vec4 :texture2DProj [:sampler2D :sampler :vec3 :coord :float :bias]]
              [:vec4 :texture2DProj [:sampler2D :sampler :vec4 :coord :float :bias]]
              [:vec4 :textureCube [:samplerCube :sampler :vec3 :coord :float :bias]]
              ;; available in both
              [:vec4 :texture2D [:sampler2D :sampler :vec2 :coord]]
              [:vec4 :texture2DProj [:sampler2D :sampler :vec3 :coord]]
              [:vec4 :texture2DProj [:sampler2D :vec4 :coord]]
              [:vec4 :textureCube [:samplerCube :sampler :vec3 :coord]]

              ])))

(def operators
  (into
    {}
    (map
      #(vector (:operator %) %)
      [
       {:operator :increment :literal "++" :class :postfix}
       {:operator :decrement :literal "--" :class :postfix}
       {:operator :pre-increment :literal "++" :class :prefix}
       {:operator :pre-decrement :literal "--" :class :prefix}
       {:operator :+- :literal "+-" :class :infix}
       {:operator :conditional-choice}
       {:operator :set :literal "=" :class :infix}
       {:operator :set+ :literal "+=" :class :infix}
       {:operator :set- :literal "-=" :class :infix}
       {:operator :set* :literal "*=" :class :infix}
       {:operator :set-div :literal "/=" :class :infix}
       {:operator :constructor}
       ])))

;; these guys should also do arg checking and type inference
(defn ^:private gen-fn [tag]
  `(defn ~(symbol (name tag)) [& body#]
     (apply gamma.ast/term ~tag body#)
     #_(gamma.ast/->Term ~tag body# (gamma.ast/gen-term-id))))

(defn define-standard-function [[n specs]]
  `(defn ~(symbol (name n)) [& body#]
     (build-standard-function-term ~n ~specs body#)))

(defn gen-constructor [tag]
  `(defn ~(symbol (name tag)) [& body#]
     (assoc (apply gamma.ast/term ~tag body#) :type ~tag)))

(defmacro ^:private gen-fns []
  `(do
     ~@(clojure.core/map define-standard-function standard-functions)
     ~@(clojure.core/map gen-fn
                         (concat
                           ;(keys gamma.ast/functions)
                           (keys operators)
                           [ :for :block :continue :break :discard]))
     ~@(clojure.core/map gen-constructor
                         [:vec2 :vec3 :vec4 :bvec2 :bvec3 :bvec4 :ivec2 :ivec3 :ivec4 :mat2 :mat3 :mat4])))

(defn vector-type [l]
  {:tag :vector :member-type :float :length l})

(defn array-type [type length]
  {:tag :array :member-type type :length length})

(defn attribute [name type]
  {:tag :variable :name name :type type :storage :attribute})

(defn uniform [name type]
  {:tag :variable :name name :type type :storage :uniform})

(defn varying
  ([name type]
   {:tag :variable :name name :type type :storage :varying})
  ([name type precision]
   {:tag :variable :name name :type type :storage :varying :precision precision}))

(defn variable
  ([x] (variable x nil))
  ([x t] {:tag :variable :name x :type t}))

;; built-in glsl variables

(defn gl-position []
  {:tag :variable :name "gl_Position" :type :vec4})

(defn gl-point-size []
  {:tag :variable :name "gl_PointSize" :type :float})

(defn gl-frag-coord []
  {:tag :variable :name "gl_FragCoord" :type :vec4})

(defn gl-front-facing []
  {:tag :variable :name "gl_FrontFacing" :type :bool})

(defn gl-point-coord []
  {:tag :variable :name "gl_PointCoord" :type :vec2})

(defn gl-frag-color []
  {:tag :variable :name "gl_FragColor" :type :vec4})

(defn gl-frag-data [n]
  {:tag :variable :name (str "gl_FragData" n) :type :vec4})

(defn gl-depth-range []
  {:tag :variable :name "gl_DepthRange" :type :vec2})

(defn ensure-term [x]
  (if (ast/term? x)
    x
    (ast/literal x)))

;; operators

(defn aget-return-type [a]
  (assert (= :aget (:head a)))
  (let [ar-term (first (:body a))]
    (cond
      (#{:vec2 :vec3 :vec4} (:type ar-term))
      :float ;; right? no?

      ;;... other cases go here :-/
      :else (throw (Exception. "Can't analyze aget return type")))))

(defn arithmetic-type [a b]
  (let [tf (fn [x]
             (if (= :aget (:head x))
               (aget-return-type x)
               (:type x)))
        t (into #{} (map tf [a b]))]
    (if (= 1 (count t))
      (#{:float :int :vec2 :vec3 :vec4 :ivec2 :ivec3 :ivec4}
        (first t)))))

(defn + [a b]
  (let [a (ensure-term a)
        b (ensure-term b)
        t (arithmetic-type a b)]
    (if t
      (assoc (ast/term :+ a b) :type t)
      (throw (Exception. (str "Arguments to + must have type :int or :float"))))))

(defn - [a b]
  (let [a (ensure-term a)
        b (ensure-term b)
        t (arithmetic-type a b)]
    (if t
      (assoc (ast/term :- a b) :type t)
      (throw (Exception. (str "Arguments to - must have type :int or :float"))))))

(defn * [a b]
  (let [a (ensure-term a)
        b (ensure-term b)
        at (:type a)
        bt (:type b)]
    (if-let
      [t (clojure.core/or
           ({[:float :float] :float
            [:mat4 :vec4]   :vec4
            [:mat3 :vec3]   :vec3
            [:mat2 :vec2]   :vec2
            [:mat4 :mat4]   :mat4
            [:mat3 :mat3]   :mat3
            [:mat2 :mat2]   :mat2
            [:vec4 :vec4]   :vec4
            [:vec3 :vec3]   :vec3
            [:vec2 :vec2]   :vec2}
            [at bt])
           ({
             #{:mat2 :float} :mat2
             #{:mat3 :float} :mat3
              #{:mat4 :float} :mat4
              #{:vec2 :float} :vec2
              #{:vec3 :float} :vec3
              #{:vec4 :float} :vec4} #{at bt}))]
      (assoc (ast/term :* a b) :type t)
      (throw (Exception. (str "Arguments to * of incompatible type: " at "," bt))))))

(defn div [a b]
  (let [a (ensure-term a)
        b (ensure-term b)
        t (arithmetic-type a b)]
    (if t
      (assoc (ast/term :div a b) :type t)
      (throw (Exception. (str "Arguments to div must have type :int or :float"))))))



(defn < [a b] (assoc (ast/term :< (ensure-term a) (ensure-term b)) :type :bool))

(defn > [a b] (assoc (ast/term :> (ensure-term a) (ensure-term b)) :type :bool))

(defn <= [a b] (assoc (ast/term :<= (ensure-term a) (ensure-term b)) :type :bool))

(defn >= [a b] (assoc (ast/term :>= (ensure-term a) (ensure-term b)) :type :bool))

(defn == [a b] (assoc (ast/term :== (ensure-term a) (ensure-term b)) :type :bool))

(defn not= [a b] (assoc (ast/term :!= (ensure-term a) (ensure-term b)) :type :bool))

(defn and [a b] (assoc (ast/term :and (ensure-term a) (ensure-term b)) :type :bool))

(defn or [a b] (assoc (ast/term :or (ensure-term a) (ensure-term b)) :type :bool))

(defn xor [a b] (assoc (ast/term :xor (ensure-term a) (ensure-term b)) :type :bool))

(defn not [a] (assoc (ast/term :not (ensure-term a)) :type :bool))

(defn if [c a b]
  (let [a (ensure-term a)
        b (ensure-term b)
        at (:type a) bt (:type b)]
    (if (= at bt)
      (assoc
        (ast/term :if c
                 (ast/term :block a)
                 (ast/term :block b))
        :type at)
      (throw (Exception. (str "Branches of if term are not of same type: " at ", " bt))))))

(defn infer-parameterized-type [rule args]
  (let [prule (:parameter rule)
        input-types (:input rule)]
    (if (clojure.core/not= (count input-types) (count args))
      :fail
      (loop [input args
             expected input-types
             parameter nil]
        (if (clojure.core/or (empty? expected) (empty? input))
          (if (clojure.core/and (empty? expected) (empty? input))
            (if (prule (:output rule)) parameter (:output rule))
            :fail)
          (let [i (first input) e (first expected) p (prule e)]
            (if p
              ;; expecting a parameterized type
              (if parameter
                ;; already have determined the concrete parameter
                (if (= parameter i)
                  (recur (next input) (next expected) parameter)
                  :fail)
                ;; need to determine concrete parameter
                (if (p i)
                  ;; type is within the parameterized range
                  (recur (next input) (next expected) (p i))
                  :fail))
              ;; not expected a parameterized type
              (if (= i e)
                (recur (next input) (next expected) parameter)
                :fail))))))))

(defn build-standard-function-term [name specs args]
  (let [t (apply ast/term name args)]
    (if-let [result
             (first
               (filter #(clojure.core/not= :fail %)
                       (map #(infer-parameterized-type % (map :type (:body t)))
                            specs)))]
      (assoc t :type result)
      (throw (Exception. (apply str "Wrong argument types for term " (clojure.core/name name)
                          ": " (interpose " ," (map :type (:body t)))))))))

(gen-fns)

(defn swizzle-type [x c]
  (let [swizzle-length (count (name c))
        depth-range-swizzles #{:near :far}]
    ;; Special-case the depth-range swizzles since their length
    ;; doesn't follow the standard rules
    (if (get depth-range-swizzles c)
      :float
      (get {1 :float 2 :vec2 3 :vec3 4 :vec4} swizzle-length))))

(defn collection-element-type [x]
  ({:vec4 :float :vec3 :float :vec2 :float
    :ivec4 :int :ivec3 :int :ivec2 :int
    :bvec4 :bool :bvec3 :bool :bvec2 :bool} x))

(defn aget [x i]
  (let [t (ast/term :aget x i)]
    (assoc
     t
     :type (collection-element-type (:type (first (:body t)))))))

(defn swizzle [x c]
  (assoc
    (ast/term :swizzle x)
    :swizzle c
    :type (swizzle-type x c)))
