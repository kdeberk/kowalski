(defpackage #:kowalski/ebnf
  (:use #:common-lisp #:cl-ppcre #:trivial-utf-8)
  (:export #:load-ebnf-file))

(in-package #:kowalski/ebnf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; TODO: Move these three to some utils package
  (defun to-string (&rest args)
    (with-output-to-string (s)
      (dolist (a args)
	(princ a s))))

  (defun to-symbol (&rest args)
    (values (intern (apply #'to-string args)))))

(defun is-surrounded-with (string ch)
  (and (char= ch (char string 0))
       (char= ch (char string (- (length string) 1)))))

(defvar +stream+ nil)

(defun peek-next ()
  (first +stream+))

(defun discard-next ()
  (pop +stream+)
  nil)

(defun read-next ()
  (pop +stream+))

;; TODO: we want to be able to include comments in parse tree, so don't do this.
(defun strip-comments (string)
  (cl-ppcre:regex-replace-all ";;.*\\n" string ""))

(defun split-into-tokens (string)
  ;; TODO: perhaps split on the special chars \s{}[]()= in addition to whitespace
  (cl-ppcre:split "\\s+" string))

(defmacro read-method (keyword &body body)
  (let ((fname (to-symbol "READ-" keyword)))
    `(setf (fdefinition ',fname)
           (lambda ()
             (let ((current-stream +stream+))
               (let ((result ,@body))
                 (if result
                     (list ,keyword result)
                     (progn
                       (setf +stream+ current-stream)
                       nil))))))))

(defmacro restore-stream-on-nil (&body body)
  `(let ((current-stream +stream+))
     (let ((result ,@body))
       (if result
           result
           (progn
             (setf +stream+ current-stream)
             nil)))))

(defmacro read-wrapped (open close &body body)
  (let ((result (gensym)))
    `(when (read-expected ,open)
       (let ((,result ,@body))
         (when (and ,result (read-expected ,close))
           ,result)))))

(defun read-expected (el)
  (when (equal el (peek-next))
    (read-next)))

;; group = "(" expression ")" .
(read-method :group
  (read-wrapped "(" ")"
    (read-expression)))

;; option = "[" expression "]" .
(read-method :option
  (read-wrapped "[" "]"
    (read-expression)))

;; repetition = "{" expression "}" .
(read-method :repetition
  (read-wrapped "{" "}"
    (read-expression)))

;; alternative = term { term } .
(defun read-alternative ()
  (let ((term (read-term)))
    (when term
      (loop
         for next = (read-term)
         while next
         collect (list :altenative next) into terms
         finally (return (append (list :alternative term) terms))))))

;; term = production_name | token [ "…" token ] | group | option | repetition .
(defun read-term ()
  (restore-stream-on-nil
    (let ((term (or (read-production-name)
                    (let ((token (read-token)))
                      (if (and token (equal "…" (peek-next)))
                          (progn
                            (discard-next)
                            (list :range token (read-token)))
                          token))
                    (read-group)
                    (read-option)
                    (read-repetition))))
      (when term
        (list :term term)))))

;; a string enclosed by double quotes
(defun read-token ()
  (restore-stream-on-nil
    (let ((next (read-next)))
      (when (and (stringp next)
                 (or (is-surrounded-with next #\")
                     (is-surrounded-with next #\')))
        (list :token next)))))

;; production = name "=" expression "."
(defun read-production ()
  (restore-stream-on-nil
    (let ((name (read-next))
          (assign (read-expected "="))
          (expression (read-expression))
          (dot (read-expected ".")))
      (when (and name assign expression dot)
        (list name expression)))))

;; a string that only consists of (alphanumeric + _) characters 
(defun read-production-name ()
  (restore-stream-on-nil
    (let ((next (read-next)))
      (when (and next
                 (every (lambda (ch) (or (alphanumericp ch) (char= #\_ ch))) next))
        next))))

(defun read-expression ()
  (restore-stream-on-nil
    (let ((alternative (read-alternative)))
      (when alternative
        (loop
           for pipe = (read-expected "|")
           while pipe
           append (list (read-alternative)) into alternatives
           finally (return (list alternative alternatives)))))))

(defun read-ebnf-file (filename)
  (with-open-file (in filename :direction :input :element-type '(unsigned-byte 8))
    (when in
      (read-utf-8-string in :stop-at-eof t))))

(defun load-ebnf-file (filename)
  (let ((contents (read-ebnf-file filename)))
    (when contents
      (let ((+stream+ (split-into-tokens (strip-comments contents))))
        (let ((productions
               (loop
                  for production = (read-production)
                  while production
                  collect production)))
          (when +stream+
              (format t "Could not parse: ~A~%" +stream+))
          productions)))))

