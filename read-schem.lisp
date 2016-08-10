(defun read-file (file-name)
  (with-open-file
      (f file-name)
    (let ((lines nil))
      (labels
          ((keep-reading ()
             (let ((line (read-line f nil nil) ; return nil on eof
                     ))
               (if line
                   (progn
                     ;;(format t "~s~c" line #\newline)
                     (unless (some #'(lambda (c) (char= (elt line 0) c))
                                   '(#\. #\*)) ; ignore lines starting with . or *
                       (setq lines (cons line lines)))
                     (keep-reading))
                   (nreverse lines)))))
        (keep-reading)))))

(defun parse-line (line)
  (let ((tokens (split-string line #\space)))
    ;; todo: anticipate more than 2 connected nets
    (multiple-value-bind (name net1 net2) (apply #'values tokens)
      (add-component :name name
                     :pins (list (make-pin :name "0" :net net1)
                                 (make-pin :name "1" :net net2)))
      (add-net :name net1)
      (add-net :name net2))))

(defun parse-file (file-name)
  (setq *schematic* (make-schematic))
  (let ((lines (read-file file-name)))
    (mapcar #'parse-line lines))
  *schematic*)

(defun split-string (src delim)
  "split string into separate words"
  (labels ((recur (src delim count accum)
             (cond
               ((= count (length src))
                (nreverse (cons src accum)))
               ((char= (elt src count) delim)
                (recur (subseq src (1+ count))
                              delim
                              0
                              (cons (subseq src 0 count) accum)))
               (t
                (recur src delim (1+ count) accum)))))
    (recur (string-trim (list delim) src)
           delim 0 nil)))

(defstruct pin
  (name "PIN-NAME" :type string)
  (net "PIN-NET" :type string))

(defstruct component
  (name "NONAME" :type string)
  (pins '()))

(defstruct net
  (name "NET-NAME" :type string))

(defstruct schematic
  (components '())
  (nets '()))

(defparameter *schematic* (make-schematic))

(defun add-component (&key name pins)
  (setf (schematic-components *schematic*)
        (cons (make-component :name name
                              :pins pins)
              (schematic-components *schematic*))))

(defun add-net (&key name)
  (setf (schematic-nets *schematic*)
        (cons (make-net :name name)
              (schematic-nets *schematic*))))
