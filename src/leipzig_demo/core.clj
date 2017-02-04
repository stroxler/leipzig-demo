(ns leipzig-demo.core
  (:require [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.temperament :as temperament]
            [leipzig.live :as live]))

;; utilities --------------------

(defn take-cycle [n seq]
  (take n (cycle seq)))

(defn cycle-times [n vec]
  (take-cycle (* n (count vec)) vec))

;; the tune -------------------

(definst bass [freq 110]
  (-> freq
      saw
      (rlpf (line:kr (* freq 10) freq 1))
      (* (env-gen (perc 0.1 0.4) :action FREE))))

(defn bassline [root]
  (->>
   (phrase (cycle-times 2 [1 1/2 1/2 1 1]) (cycle-times 2 [0 -3 -1 0 2]))
   (where :pitch (scale/from root))
   (where :pitch (comp scale/lower scale/lower))
   (where :part (is :bass))))

(def progression [0 0 3 0 4 0])

(def blues-track
  (->>
   (mapthen bassline progression)
   (where :pitch (comp temperament/equal scale/E scale/minor))
   (where :time (bpm 90))
   (where :duration (bpm 90))))

(defmethod live/play-note :bass [{hertz :pitch}]
  (bass hertz))

(comment
  ;; overtone stuff
  (bass)
  (stop)
  (take-cycle 2 [1 2 3])
  (cycle-times 2 [1 2 3])
  )

(comment
  ;; leipzig stuff
  (live/jam (var blues-track))
  (live/stop)
  (live/play track)
  )
