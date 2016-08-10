(defvar *lines* '())

(defun zener ()
  (with-open-file
      (f "zener.net")
    (labels
        ((keep-reading ()
           (let ((line (read-line f nil nil) ; return nil on eof
                   ))
             (when line
               (format t "~s~c" line #\newline)
               (unless (some #'(lambda (c) (char= (elt line 0) c))
                             '(#\. #\*))
                 ;; (or (char= (elt line 0) #\.)
                 ;;     (char= (elt line 0) #\*))
                 (setq *lines* (cons line *lines*)))
               (keep-reading)))))
      (keep-reading))))

(defun split-string (src
                     delim
                     &optional
                       (count 0)
                       (accum '()))
  "split string into separate words"
  (cond
    ((= count (length src))
     (nreverse (cons src accum)))
    ((char= (elt src count) delim)
     (split-string (subseq src (1+ count))
                   delim
                   0
                   (cons (subseq src 0 count) accum)))
    (t
     (split-string src delim (1+ count) accum))))

(defun split-string/2 (src delim)
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
