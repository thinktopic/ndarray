(require
 '[thi.ng.ndarray.core :as nd]
 '[thi.ng.ndarray.contours :as contours]
 '[thi.ng.geom.core :as g]
 '[thi.ng.geom.core.vector :as v]
 '[thi.ng.geom.svg.core :as svg]
 '[thi.ng.math.core :as m]
 '[thi.ng.math.simplexnoise :as n]
 '[thi.ng.color.gradients :as grad])

(def res 128)
(def width 640.0)
(def scale (/ width (- res 2)))
(def clipped (- width (* 2.0 scale)))
(def n-scale 0.03)
(def num-contours 60)

(defn contour->svg
  "Takes a single seq of contour coordinates and converts it into an
  SVG polygon (hiccup format)."
  [contour]
  (-> (map #(-> % v/vec2 v/yx (g/scale scale)) contour)
      (svg/polygon)))

(defn noise-matrix
  "Creates a new 2D matrix of size res and populates it with simplex
  noise, then sets border cells to 1.0 and returns matrix"
  [res ns]
  (let [mat (nd/ndarray :float32 (float-array (* res res)) [res res])]
    (dorun
     (for [[y x] (nd/position-seq mat)]
       (nd/set-at mat x y (+ 0.5 (* 0.5 (n/noise2 (+ 101 (* x ns)) (* y ns)))))))
    (contours/set-border-2d mat 1)))

(defn circle-matrix
  "Creates new 2D matrix of size res and populates it w/ modulated
  normalized distance values from center. `spikes` arg is number of
  oscillations used to modulate. `amp` is modulation strength. Sets
  border cells to 1.0 and returns matrix."
  [res spikes amp]
  (let [mat (nd/ndarray :float32 (float-array (* res res)) [res res])
        c (/ res 2.0)
        dmax (* m/SQRT2 0.5 res)]
    (dorun
     (for [[y x] (nd/position-seq mat)
           :let [dx (- c x)
                 dy (- c y)
                 t  (Math/atan2 dy dx)
                 d  (Math/sqrt (* (+ 0.5 (* amp (Math/sin (* t spikes))))
                                  (+ (* dx dx) (* dy dy))))]]
       (nd/set-at mat x y (/ d dmax))))
    (contours/set-border-2d mat 1)))

(def palette
  (apply grad/cosine-gradient num-contours (:orange-blue grad/cosine-schemes)))

;; choose one...
(def mat (noise-matrix res n-scale))
;;(def mat (circle-matrix res 8 0.25))

(->> (m/norm-range num-contours)
     (rest)
     (map
      #(svg/group
        {:stroke (palette (int (* % (dec num-contours))))
         :fill "none"}
        (map contour->svg (contours/find-contours-2d mat %))))
     (svg/svg
      {:width width
       :height width
       :viewBox (format "%1.2f %1.2f %1.2f %1.2f" scale scale clipped clipped)})
     (svg/serialize)
     (spit "iso.svg"))
