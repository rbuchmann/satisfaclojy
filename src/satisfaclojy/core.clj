(ns satisfaclojy.core)


(def x 5)

(defn boo []
  "foo")

(def recipes {:rotor      {:screw 25
                           :rod   5}
              :screw      {:rod 1/4}
              :rod        {:iron-ingot 1}
              :iron-ingot {:iron-ore 1}})

(def prod-speeds {:rotor 4})


(defn primitive? [product]
  (not (get recipes product)))

(defn scale-recipe [recipe count]
  (update-vals recipe #(* count %)))

; (fn [x] (* x count))
; #(* % count)

(defn resource-chain
  ([product]
   (resource-chain product 1))
  ([product product-count]
   (if (primitive? product)
     {product {:count product-count}}
     (let [recipe (-> product
                      recipes
                      (scale-recipe product-count))]
       (into {}
             (map (fn [[component-product count]]
                    (if (primitive? component-product)
                      [component-product {:count count}]
                      [component-product {:count        count
                                          :requirements (resource-chain component-product count)}]))
                  recipe))))))


(defn total-counts
  ([chain]
   (total-counts chain {}))
  ([chain current]
   (apply merge-with +
          current
          (map (fn [[product {:keys [count requirements]}]]
                 (total-counts requirements {product count}))
               chain))))

;; (foo (bar (baz x)))
;; =
;; (-> x
;;     baz
;;     (bar 2)
;;     foo)


(defn count-odd-lengths [col]
  (->> col
       (map count)
       (filter odd?)
       count))
