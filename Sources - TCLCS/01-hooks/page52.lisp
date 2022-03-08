;;;; Taken directly from P. 52 - I want to play with the code

;; this part is from github
(defpackage #:page-52
  (:use #:cl))

(cl:in-package #:page-52)

(defvar *phonebook*
  '((:mom :parent)
    (:dad :parent)
    (:alice :classmate :csgo :homework)
    (:bob :classmate :homework)
    (:catherine :classmate :ex)
    (:dorothy :classmate :girlfriend :csgo)
    (:eric :classmate :homework)
    (:dentist)))

;; More specific condition.
(define-condition grave-mistake (error)
  ((%reason :reader reason :initarg :reason)))

;; main code
(defun receive-phone-call (person)
  "Receive a phone call from a person."
  (format t ";; Answering a call from ~A.~%" (first person))
  (when (member :ex person)
    (format t ";; About to commit a grave mistake...~%")
    (signal 'grave-mistake :reason :about-to-call-your-ex)
    (we do not want to be here)))


;;; without any handling
(dolist (person *phonebook*)
  (receive-phone-call person))

;;; this part is from P 53

(defun defuse-error (condition)
  (declare (ignore condition))
  (format t ";; Nope nope nope, not answering!~%")
  (throw :do-not-answer nil))

(defun defuse-grave-mistake (condition)
  (let ((reason (reason condition)))
    (format t ";; Nope nope nope, not answering - reason was, ~A!~%" reason))
  (throw :do-not-answer nil))

;; from the book
(handler-bind ((grave-mistake #'defuse-grave-mistake))
  (dolist (person *phonebook*)
    (catch :do-not-answer
      (receive-phone-call person)
      (format t "~&Inside catch: Name is ~s~%" (car person)))
    (format t "~&Outside catch: Name is ~s~%" (car person))))

;; from the book
(dolist (person *phonebook*)
  (handler-case (receive-phone-call person)
    (grave-mistake (condition)
      (format t ";; Nope, not this time: ~A~%" (reason condition)))))

;; what about other stuff inside handler-case test area?
(dolist (person *phonebook*)
  (handler-case
      (receive-phone-call person)
    (format t "~&Inside handler-case test: Name is ~s~%" (car person))
    (grave-mistake (condition)
      (format t ";; Nope, not this time: ~A~%" (reason condition))
      (format t "~&Inside grave-mistake case: Name is ~s~%" (car person)))))
