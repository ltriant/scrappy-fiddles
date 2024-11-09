;; a sand simulator, because I'm obsessed with the Dune book series, and also the movies, and
;; now it's bleeding into all of my hobbies
;;
;; @ltriant@hachyderm.io

(ns sand-simulator.core
  (:require [goog.string :as gstring]
            [goog.string.format]))

;; sand grain size, in pixels
(def resolution 5)

;; tick rate, in milliseconds
(def tick-rate 32)

;; drops a grid of sand grains of certain diameter
(def sand-diameter 3)

;; dynamically change the lightness of the color of the sand
(def sand-color-min    50)  ; starting lightness
(def sand-color-max    90)  ; maximum lightnes
(def sand-color-step 0.20)  ; how much to step the lightness each frame

(def sand-color-index (atom 1))

;; keeping track of the mouse click state, so we can make new grains of sand
(def mouse-state (atom :none))

;; the grid
(def canvas (.getElementById js/document "sandbox"))
(def ctx (.getContext canvas "2d"))

(def grid-cols (/ (.-width canvas) resolution))
(def grid-rows (/ (.-height canvas) resolution))
(def grid (atom (into [] (repeat grid-rows (into [] (repeat grid-cols {:type :air}))))))

;; off-screen canvas
(def off-canvas
  (doto (.createElement js/document "canvas")
    ;; make enough room for the background color, and each sand grain color
    (set! -width (* resolution (+ 2 (- sand-color-max sand-color-min))))
    (set! -height resolution))) 
(def off-ctx (.getContext off-canvas "2d"))

;; har har har
(def quips
  ["Wow. Much sand. So color."
   "May thy knife chip and shatter"
   "Welcome to Sand Theft Auto"
   "How do beaches greet each other? With a sandshake"
   "We're off to never-never sand"
   "Sand nudes... I mean send dunes"])

(defn draw-diff
  "Draw only the difference between two grids. Assumes they are both of the correct size."
  [old-grid new-grid]

  (doseq [y (range grid-rows)
          x (range grid-cols)]
    (let [old-grain (get-in old-grid [y x])
          new-grain (get-in new-grid [y x])]
      (when (and (= :sand (:type new-grain))
                 (not= (:type old-grain) (:type new-grain)))
        (.drawImage ctx
                    off-canvas
                    (* resolution (:color new-grain))
                    0
                    resolution
                    resolution
                    (* resolution x)
                    (* resolution y)
                    resolution
                    resolution))))

  (comment set! ctx -fillStyle "hsl(0, 0%, 100%)")
  (doseq [y (range grid-rows)
          x (range grid-cols)]
    (let [old-grain (get-in old-grid [y x])
          new-grain (get-in new-grid [y x])]
      (when (and (= :air (:type new-grain))
                 (not= (:type old-grain) (:type new-grain)))
        (.drawImage ctx
                    off-canvas
                    0
                    0
                    resolution
                    resolution
                    (* resolution x)
                    (* resolution y)
                    resolution
                    resolution)
        (comment .fillRect ctx
                   (* resolution x)
                   (* resolution y)
                   resolution
                   resolution)))))

(defn next-grid
  "Get the next step of the grid."
  []

  ;; start from the second row from the bottom, and move up
  (loop [new-grid @grid
         pts (for [y (reverse (range (- grid-rows 1)))
                   x (range grid-cols)]
               [y x])]
    (if (empty? pts)
      new-grid
      (let [[y x] (first pts)]
        (recur
          (let [grain (get-in new-grid [y x])
                grain-below (:type (get-in new-grid [(inc y) x]))
                grain-below-left (:type (get-in new-grid [(inc y) (dec x)]))
                grain-below-right (:type (get-in new-grid [(inc y) (inc x)]))]
            (cond
              ;; if we can fall straight down
              (and (= :sand (:type grain))
                   (= :air grain-below)) 
              (-> new-grid
                  (assoc-in [y x] {:type :air})
                  (assoc-in [(inc y) x] grain))

              ;; if we can only fall diagonally left
              (and (= :sand (:type grain))
                   (= :air grain-below-left)
                   (not= :air grain-below-right))
              (-> new-grid
                  (assoc-in [y x] {:type :air})
                  (assoc-in [(inc y) (dec x)] grain))

              ;; if we can only fall diagonally right
              (and (= :sand (:type grain))
                   (= :air grain-below-right)
                   (not= :air grain-below-left))
              (-> new-grid
                  (assoc-in [y x] {:type :air})   
                  (assoc-in [(inc y) (inc x)] grain))

              ;; if we can fall diagonally left or right, pick randomly
              (and (= :sand (:type grain))
                   (= :air grain-below-left)
                   (= :air grain-below-right))
              (-> new-grid
                  (assoc-in [y x] {:type :air})
                  (assoc-in [(inc y)
                             (if (< 0.5 (rand 1))
                               (dec x)
                               (inc x))]
                            grain))

              ;; otherwise, no change
              :else
              new-grid))
          (rest pts))))))

(defn tick
  "Process one frame of the simulation."
  []

  ;; we create a new grid on each tick, and then we draw only the changed squares, otherwise it's
  ;; too slow at finer resolutions... although this is still too slow at the finest resolutions
  (let [new-grid (next-grid)]
    (draw-diff @grid new-grid)
    (reset! grid new-grid)

    ;; vary the color value slowly over time to create some pretty texturing
    (swap! sand-color-index
           #(-> %
                (+ sand-color-step)
                (mod (- sand-color-max
                        sand-color-min))))))

(defn ->grid-coords
  "Get the grid coordinates from a mouse event."
  [e]

  (let [rect (.getBoundingClientRect canvas)
        mouse-x (- (.-pageX e) (.-left rect)) 
        mouse-y (- (.-pageY e) (.-top rect)) 
        grid-x (int (/ mouse-x resolution)) 
        grid-y (int (/ mouse-y resolution))]
    [grid-x grid-y]))

(defn make-sand
  "Place some sand at a particular grid coordinate."
  [x y]

  (let [extent (int (/ sand-diameter 2))
        new-sand (for [offset-y (->> (range (* -1 extent) (inc extent))
                                     (map #(+ y %))
                                     (filter #(and (>= % 0) (< % grid-rows))))
                       offset-x (->> (range (* -1 extent) (inc extent))
                                     (map #(+ x %))
                                     (filter #(and (>= % 0) (< % grid-cols))))]
                   [offset-y offset-x])]
    (swap! grid
           (fn [grid]
             (reduce (fn [acc [y x]]
                       (assoc-in acc
                                 [y x]
                                 {:type :sand
                                  :color (inc @sand-color-index)}))
                     grid
                     new-sand)))))

(defn on-mousedown [e]
  (let [[x y] (->grid-coords e)]
    (make-sand x y))

  (reset! mouse-state :mousedown))

(defn on-mousemove [e]
  (when (= @mouse-state :mousedown)
    (let [[x y] (->grid-coords e)]
      (make-sand x y))))

(defn reset-mouse-state [e]
  (swap! mouse-state :none))

(defn init []
  ;; initialise the off-screen canvas with all the colors we want to use as single pixel rectangles.
  ;; we will then drawImage from this rendered canvas, which should be faster than calling fillRect
  ;; a bunch of times every frame, because my old Macbook without tons of RAM cries loudly if I do
  ;; that.
  (doto off-ctx
    (set! -fillStyle "hsl(0, 0%, 100%)")
    (.fillRect 0 0 resolution resolution))
  (loop [i 1
         b sand-color-min]
    (when (< b sand-color-max)
      (doto off-ctx
        (set! -fillStyle (gstring/format "hsl(40, 60%%, %d%%)" b))
        (.fillRect (* i resolution) 0 resolution resolution))
      (recur (inc i)
             (+ b sand-color-step))))

  (.addEventListener canvas "mousedown" on-mousedown)
  (.addEventListener canvas "mousemove" on-mousemove)
  (.addEventListener canvas "mouseup" reset-mouse-state)
  (.addEventListener canvas "mouseleave" reset-mouse-state)

  ;; set the tick rate
  (.setInterval js/window tick tick-rate)

  ;; choose a random, witty quip because I'm so funny
  (.setInterval
    js/window
    (fn []
      (doto (.getElementById js/document "quip")
        (set! -innerText (rand-nth quips))))
    ;; 10 seconds
    (* 1000 10)))
