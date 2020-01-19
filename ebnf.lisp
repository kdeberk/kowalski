(in-package #:ebnf)

(defvar +stream+)

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

(defun read-ebnf-file (filename)
  (with-open-file (in filename :direction :input :element-type '(unsigned-byte 8))
    (when in
      (read-utf-8-string in :stop-at-eof t))))

(defun strip-comments (string)
  (cl-ppcre:regex-replace-all ";;.*\\n" string ""))

(defun split-into-tokens (string)
  ;; TODO: perhaps split on unquoted special chars \s{}[]()
  (cl-ppcre:split "\\s+" string))

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

;; expression = alternative { "|" alternative } .
;; (read-method :expression
;;   (let ((alternative (read-alternative)))
;;     (when alternative
;;       (loop
;;          for pipe = (read-expected "|")
;;          while pipe
;;          append (list (read-alternative)) into alternatives
;;          finally (return alternatives)))))

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

(defun read-group ()
  (restore-stream-on-nil
    (read-wrapped :group "(" ")"
      (read-expression))))

;; (defun read-option ()
;;   (restore-stream-on-nil
;;     (read-wrapped :option "[" "]"
;;       (read-expression))))

(read-method :option
  (read-wrapped "[" "]"
    (read-expression)))

(read-method :repetition
  (read-wrapped "{" "}"
    (read-expression)))

;; (defun read-repetition ()
;;   (restore-stream-on-nil
;;     (read-wrapped :repetition "{" "}"
;;       (read-expression))))

;;

(defmacro read-method (keyword &body body)
  (let ((fname (symb "READ-" keyword)))
    `(setf (symbol-function ',fname)
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

(defun peek-next ()
  (first +stream+))

(defun discard-next ()
  (pop +stream+)
  nil)

(defun read-next ()
  (pop +stream+))

(defun is-surrounded-with (string ch)
  (and (char= ch (char string 0))
       (char= ch (char string (- (length string) 1)))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
