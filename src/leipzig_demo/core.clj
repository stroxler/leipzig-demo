(ns leipzig-demo.core
  (:require [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.temperament :as temperament]
            [leipzig.live :as live]
            [leipzig.chord :as chord]))

;; utilities --------------------

(defn take-cycle [n seq]
  (take n (cycle seq)))

(defn cycle-times [n vec]
  (take-cycle (* n (count vec)) vec))

(defn adding [k v notes]
  (map #(assoc % k v) notes))

;; the instruments and methods -------------------

;; bass

(definst inst:bass [hz 110]
  (-> hz
      saw
      (rlpf (line:kr (* hz 10) hz 1))
      (* (env-gen (perc 0.1 0.4) :action FREE))
      (* 0.50)))

(defmethod live/play-note :bass [{hertz :pitch}]
  (inst:bass hertz))

;; kick

(definst inst:kick [hz 90]
  (-> (line:kr hz (* hz 0.5) 0.5)
      sin-osc
      (+ (sin-osc hz))
      (* (env-gen (perc 0.01 0.3) :action FREE))))

(defmethod live/play-note :kick [{hertz :pitch}]
  (inst:kick))

;; organ

(definst inst:organ [hz 110  sec 10]
  (let [waver-slow (sin-osc:kr 3)
        waver-fast (sin-osc:kr 4)]
    (->
     (+ (* 0.1 (sin-osc hz)) (* 0.05 (saw hz)))
     (rlpf (mul-add waver-fast 300 (* hz 4)))
     (rlpf (mul-add waver-slow 400 (* hz 3)))
     (* (env-gen (adsr) (line:kr 1 0 sec) :action FREE)))))

(defmethod live/play-note :organ [{hertz :pitch seconds :duration}]
  (inst:organ hertz seconds))

;; the parts --------------------

(def chord-progression:roots [0 0 3 0 4 0])

;; bass

(defn part-bass:riff [root]
  (->>
   (phrase (cycle-times 2 [1 1/2 1/2 1 1]) (cycle-times 2 [0 -3 -1 0 2]))
   (where :pitch (scale/from root))
   (where :pitch (comp scale/lower scale/lower))
   (adding :part :bass)))


(def part:bass (mapthen part-bass:riff chord-progression:roots))

;; kick beat

(def part:kick
  (->>
   (phrase (cycle [1/2 1/4 1/4 1/2 1/2]) (repeat 0))
   (take 10)    ;; here 1 is a quarter note, so there are 10 beats per measure
   (times 12)
   (adding :part :kick)))

;; organ

(defn part-organ:play-chord [root]
  (->> (phrase (repeat 8) [(-> chord/triad (chord/root root))]) ; (8 is duration, holding 2 measures)
       (adding :part :organ)))

(def part:organ (mapthen part-organ:play-chord chord-progression:roots))


;; the track --------------

(def track:blues
  (->>
   part:bass
   (with part:kick)
   (with part:organ)
   (where :pitch (comp temperament/equal scale/E scale/minor))
   (where :time (bpm 90))
   (where :duration (bpm 90))))

(comment
  ;; overtone stuff
  (inst:bass)
  (inst:kick)
  (inst:organ)
  (stop)
  )

(comment
  track:blues
  ;; leipzig stuff
  (live/jam (var track:blues))
  (live/stop)
  (live/play track:blues)
  )
