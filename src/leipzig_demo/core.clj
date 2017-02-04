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

(defn adding [k v notes]
  (map #(assoc % k v) notes))

;; the instruments and methods -------------------

(definst inst:bass [freq 110]
  (-> freq
      saw
      (rlpf (line:kr (* freq 10) freq 1))
      (* (env-gen (perc 0.1 0.4) :action FREE))))

;;(defmethod live/play-note :bass [{hertz :pitch}]
;;  (inst:bass hertz))

(definst inst:kick [freq 90]
  (-> (line:kr freq (* freq 0.5) 0.5)
      sin-osc
      (+ (sin-osc freq))
      (* (env-gen (perc 0.01 0.3) :action FREE))))

(defmethod live/play-note :kick [{hertz :pitch}]
  (inst:kick))

;; the parts --------------------

(defn part-bass:riff [root]
  (->>
   (phrase (cycle-times 2 [1 1/2 1/2 1 1]) (cycle-times 2 [0 -3 -1 0 2]))
   (where :pitch (scale/from root))
   (where :pitch (comp scale/lower scale/lower))
   (adding :part :bass)))

(def part-bass:progression [0 0 3 0 4 0])

(def part:bass (mapthen part-bass:riff part-bass:progression))

(def part:kick
  (->>
   (phrase (cycle [1/2 1/4 1/4 1/2 1/2]) (repeat 0))
   (take 10)    ;; here 1 is a quarter note, so there are 10 beats per measure
   (times 12)
   (adding :part :kick)))

;; the track --------------

(def track:blues
  (->>
   part:bass
   (with part:kick)
   (where :pitch (comp temperament/equal scale/E scale/minor))
   (where :time (bpm 90))
   (where :duration (bpm 90))))

(comment
  ;; overtone stuff
  (inst:bass)
  (inst:kick)
  (stop)
  (take-cycle 2 [1 2 3])
  (cycle-times 2 [1 2 3])
  )

(comment
  (part-bass:riff 5)
  part:bass
  part:kick
  track:blues
  ;; leipzig stuff
  (live/jam (var track:blues))
  (live/stop)
  (live/play track)
  )
