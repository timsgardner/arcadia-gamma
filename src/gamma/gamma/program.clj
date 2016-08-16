(ns gamma.program
  (:require gamma.compiler.core
            ;;fipp.engine
            [gamma.emit.emit :as emit]
            gamma.emit.fun
            gamma.emit.operator
            gamma.emit.statement
            gamma.emit.tag
            gamma.emit.construct
            [gamma.api :as g]))

(defn ast [inputs]
  (apply g/block
         (mapv
           (fn [[k v]]
             (g/set k v))
           inputs)))

(defn precision-defaults [p]
  (let [x (map (fn [[k v]] (str "precision " (name v) " " (name k) ";\n")) p)]
    (if (empty? x)
      ""
      (apply str x))))

(defn unfipp [x]
  (->> x
    (clojure.walk/prewalk
      (fn [x]
        (if (and (vector? x) (= (first x) :nest))
          (drop 2 x)
          x)))
    flatten
    (remove keyword?)
    (apply str)))

(defn glsl [shader precision]
  (let [p precision]
    (str
      (precision-defaults p)
      (unfipp
        (emit/emit (:ir shader) shader)))))

(defn shader [shader opts]
  (let [ast (ast shader)
        ir (gamma.compiler.core/compile ast)
        v (gamma.compiler.core/variables ir)
        locals (filter
                 #(not
                   (#{:attribute :uniform :varying} (:storage %)))
                 v)
        globals (filter
                  #(#{:attribute :uniform :varying} (:storage %))
                  v)
        outputs (keys shader)
        inputs (clojure.set/difference (into #{} globals) (into #{} outputs))
        ]
    (let
      [p
       {:tag     :shader
        :inputs  inputs
        :outputs outputs
        :locals  locals
        :ir      ir
        :ast     ast}]
      p
      (assoc p :glsl (glsl p opts)))))



(defn program-inputs [vs fs]
  (into
    (:inputs vs)
    (filter #(= :uniform (:storage %))
            (:inputs fs))))

(defn program
  ([x]
   (let [{:keys [vertex-shader fragment-shader precision]} x
         vs (shader vertex-shader precision)
         fs (shader fragment-shader precision)]
     (merge
       x
       {:tag             :program
       :vertex-shader   vs
       :fragment-shader fs
       :inputs          (program-inputs vs fs)}))))







