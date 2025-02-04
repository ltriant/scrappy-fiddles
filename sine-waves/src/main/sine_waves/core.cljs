;; messing around with sine waves
;; @ltriant@hachyderm.io

(ns sine-waves.core
  (:require [cljs.math :as m]
            [goog.string :as gstring]
            [goog.string.format]))

(def line-thickness 1) ;; pixels
(def tick-rate 32)     ;; milliseconds

(def generation (atom 0))

(defn plot-sine-wave
  [canvas
   ctx
   {:keys [color y a b c]}]

  (doto ctx
    (.beginPath)
    (set! -lineWidth line-thickness)
    (.moveTo 0 y))

  (doseq [t (range (.-width canvas))]
    (let [calculated-color (gstring/format "hsl(%d, %d%%, %d%%)"
                                           (:hue color)
                                           (:sat color)
                                           (* 100 (/ t (.-width canvas))))

          ;; y = a * sin(b * t + c)
          y-off (-> t
                    (* b)
                    (+ c)
                    (m/to-radians)
                    (m/sin)
                    (* a (/ t 300))
                    (+ (/ t 8))
                    (- 60))]
      (set! ctx -strokeStyle calculated-color)
      (.lineTo ctx t (+ y y-off))))

  (doto ctx
    (.stroke)))

(defn tick
  [canvas
   ctx]

  (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))

  (let [sine-waves (concat
                     (for [a (range 60 180 3)]
                       {:color {:hue 197
                                :sat 94}
                        :y (/ (.-height canvas) 2)
                        :a a
                        :b 0.5
                        :c @generation}))]
    (doseq [y sine-waves]
      (plot-sine-wave canvas ctx y)))
  
  (swap! generation inc))

(defn init []
  (let [canvas (.getElementById js/document "linebox")
        ctx (.getContext canvas "2d")]
    (.setInterval
      js/window
      (fn [] (tick canvas ctx))
      tick-rate)))
