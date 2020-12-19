;;; emacs-vdot.el --- Summary
;;; Commentary:
;;; Functions meant for calculating training paces for running based on test efforts.
;;; Based in the work described in Jack Daniels' book - Daniels' Running Formula.
;;; Code:

(defun vdot-%VOmax (seconds)
  "Return the percentage of VOmax a runner can sustain for the duration given in SECONDS."
  (let ((minutes (/ (float seconds) 60)))
    (+ 0.8
       (* 0.1894393
          (exp (* -0.012778 minutes)))
       (* 0.2989558
          (exp (* -0.1932605 minutes))))))

(defun vdot-VO2 (speed)
  "Return the oxygen consumption of a runner when running in a given SPEED."
  (+ -4.60
     (* 0.182258 speed)
     (* 0.000104 (expt speed 2))))

(defun vdot-m-min (distance time)
  "Return speed as meters / minutes.

Gilbert's formulas use meters / minutes as units.  This function
calculates the speed required to cover a DISTANCE in the given
TIME using these units."

  (/ distance (/ (float time) 60)))

(defun vdot-VDot  (race-distance race-duration)
  "Calculate the pseudo VO2max, VDot based on a RACE-DISTANCE and RACE-DURATION."

  (/ (vdot-VO2 (vdot-m-min race-distance race-duration))
     (vdot-%VOmax race-duration)))

(defun vdot-VO2->velocity (vo2)
  "Calculate theoretical velocity when runner consumes given amount of oxygen VO2."
  (+ 29.54
     (* 5.000663 vo2)
     (* -0.007546 (expt vo2 2))))

(defun vdot-pace->velocity (pace)
  "Convert PACE into velocity.

Pace is seconds / 1000 m so..."
  (/ 1000 (/ (float pace) 60)))

(defun vdot-velocity->pace (velocity)
  "Convert VELOCITY to pace.

velocity is m / min so...
v = m / min and we want the pace per 1000 m
v = 1000 m / min -> min = 1000 m / v

Unit is seconds per 1000 meters"

  (truncate (* 60 (/ 1000 (float velocity)))))

(defun vdot-pace-at-effort (multiplier vdot)
  "Calculate pace at a MULTIPLIER of VDOT.

Return value is seconds / 1000 meters."

  (let* ((part (* multiplier vdot))
         (velocity (vdot-VO2->velocity part)))
    (vdot-velocity->pace velocity)))

(defun vdot-parse-time (time)
  "Return TIME string parsed into seconds."
  (cl-destructuring-bind (minutes seconds)
      (split-string time "'")
    (+ (* 60 (string-to-number minutes))
       (string-to-number seconds))))

(defun vdot-pace-to-string (pace)
  "Return PACE as readable string."
  (let* ((minutes (/ pace 60))
         (seconds (% pace 60)))
    (format "%02d'%02d / km" minutes seconds)))

(defun vdot-seconds-to-string (time)
  "Return TIME as a readable string."
  (let* ((minutes (/ time 60))
         (seconds (% time 60)))
    (format "%02d'%02d" minutes seconds)))

(defun vdot-predict-performance (vdot distance)
  "Predict a performance for a DISTANCE given a VDOT."
  (let ((guess 60))
    (while (< vdot (vdot-VDot distance guess))
      (setq guess (+ guess 60)))
    (setq guess (- guess 60))
    (while (< vdot (vdot-VDot distance guess))
      (setq guess (1+ guess)))
    (1- guess)))

(provide 'emacs-vdot)
;;; emacs-vdot ends here
