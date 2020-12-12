;;; emacs-vdot.el --- Summary
;;; Commentary:
;;; Functions meant for calculating training paces for running based on test efforts.
;;; Based in the work described in Jack Daniels' book - Daniels' Running Formula.
;;; Code:

(defun %VOmax (seconds)
  "Return the precentage of VOmax a runner can sustain for the duration given in SECONDS."
  (let ((minutes (/ (float seconds) 60)))
    (+ 0.8
       (* 0.1894393
          (exp (* -0.012778 minutes)))
       (* 0.2989558
          (exp (* -0.1932605 minutes))))))

(defun VO2 (SPEED)
  "Return the oxygen consumption of a runner when running in a given SPEED."
  (+ -4.60
     (* 0.182258 SPEED)
     (* 0.000104 (expt SPEED 2))))

(defun m-min (distance time)
  "Return speed as meters / minutes.

Gilbert's formulas use meters / minutes as units.  This function
calculates the speed required to cover a DISTANCE in the given
TIME using these units."

  (/ distance (/ time 60)))

(defun VDot  (race-distance race-duration)
  "Calculate the pseudo VO2max, VDot based on a RACE-DISTANCE and RACE-DURATION."

  (/ (VO2 (m-min race-distance race-duration))
     (%VOmax race-duration)))

(defun VO2->velocity (vo2)
  "Calculate theoretical velocity when runner consumes given amount of oxygen VO2."
  (+ 29.54
     (* 5.000663 vo2)
     (* -0.007546 (expt vo2 2))))

(defun pace->velocity (pace)
  "Convert PACE into velocity.

Pace is seconds / 1000 m so..."
  (/ 1000 (/ (float pace) 60)))

(defun velocity->pace (velocity)
  "Convert VELOCITY to pace.

velocity is m / min so...
v = m / min and we want the pace per 1000 m
v = 1000 m / min -> min = 1000 m / v

Unit is seconds per 1000 meters"

  (truncate (* 60 (/ 1000 (float velocity)))))

(defun effort (multiplier vdot)
  "Calculate pace at a MULTIPLIER of VDOT.

Return value is seconds / 1000 meters."

  (let* ((part (* multiplier vdot))
         (velocity (VO2->velocity part)))
    (velocity->pace velocity)))


(provide 'emacs-vdot)
;;; emacs-vdot ends here
