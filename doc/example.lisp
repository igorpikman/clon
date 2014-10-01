(in-package :cl-user)

;;; If this runs under SLIME you may have to look into the
;;; *inferior-lisp* buffer to see the output depending on your setup.
(let ((n 0))
  (defun say-hello ()
    (incf n)
    (format t "Hello world ~D at ~S~%" n
            (multiple-value-list (decode-universal-time (get-universal-time))))
    (force-output)))

;;; SCHEDULE is not mutated by iterating over it by NEXT-TIME. A
;;; SCHEDULER on the other hand is all about remembering the last
;;; time. The schedule here reads as: 'seconds 0, 15, 30 and 45 of
;;; every even minute that's between 10 and 40'.
(let ((schedule (clon:make-typed-cron-schedule
                 :second '(member 0 15 30 45)
                 :minute '(and (integer 10 40) (satisfies evenp)))))
  (clon:schedule-function 'say-hello
                          (clon:make-scheduler schedule)
                          :name "Hello world 1"
                          :thread t))

;;; SCHEDULE-FUNCTION returned a timer, stop it when we got bored of it.
(sb-ext:unschedule-timer *)

;;; Do something even more simple: say hello once every minute. Note
;;; that the first one will happen immediately (thanks to ALLOW-NOW-P)
;;; while the subsequent ones at second 0.
(let ((schedule (clon:make-typed-cron-schedule :minute '*)))
  (clon:schedule-function 'say-hello
                          (clon:make-scheduler schedule :allow-now-p t)
                          :name "Hello world 2"
                          :thread t))

;;; Let's do something moderately fancy. Take second 0 and 15 in even minutes
;;; and second 30 in odd minutes.
(defun bump-second (second decoded-time n)
  ;; Check that we are indeed the bumper of seconds.
  (assert (= n 0))
  ;; Be painfully correct and return NIL if there is no next second in
  ;; this minute that we want.
  (cond ((oddp (elt decoded-time 1)) 30)
        ((< 15 second) nil)
        ((< 0 second) 15)
        (t 0))
  ;; Or rely on the fact that values less than the current SECOND are
  ;; treated as NIL.
  #+nil
  (cond ((oddp (elt decoded-time 1)) 30)
        ((<= 15) 15)
        (t 0)))

(let ((schedule (clon:make-cron-schedule :second 'bump-second)))
  (clon:schedule-function 'say-hello
                          (clon:make-scheduler schedule)
                          :name "Hello world 3"
                          :thread t))

;;; Unschedule all timers.
(mapc #'sb-ext:unschedule-timer (list-all-timers))
