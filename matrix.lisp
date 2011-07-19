;;;; matrix.lisp

(in-package #:matrix)

(defmacro while (condition &rest forms)
  `(loop (unless ,condition (return)) ,@forms))

(defun set-elem (ar i j v)
  (setf (nth j (nth i ar)) v))

(defun get-elem (ar i j)
  (nth j (nth i ar)))

(defun swap-rows (ar r1 r2)
  (let ((n (length (nth r1 ar))) (temp))
    (dotimes (i n)
      (setf temp (get-elem ar r1 i))
      (set-elem ar r1 i (get-elem ar r2 i))
      (set-elem ar r2 i temp))))

;apply the specified operation on a given row of a matrix
(defun apply-op-row (ar r val fn)
  (dotimes (i (length (nth r ar)))
    (set-elem ar r i (funcall fn (get-elem ar r i) val))))

;subtract a given list of values from a row
(defun subtract (ar r lst)
  (dotimes (i (length (nth r ar)))
    (set-elem ar r i (- (get-elem ar r i) (nth i lst)))))

;extract a given row from a matrix and multiply it by the given factor
(defun get-row (ar r factor)
  (mapcar (lambda (x) (* factor x)) (nth r ar)))

;get a particular column's values after multiplying them by the given factor
(defun get-col (ar c factor)
  (let ((col))
    (dotimes (i (length ar))
      (my-append col (* factor (get-elem ar i c))))
  col))

(defun print-matrix (m)
  (dotimes (i (length m))
    (dotimes (j (length (nth i m)))
      (format t "~5f " (get-elem m i j)))
    (format t "~%")))

;derive the reduced row echelon form of a given matrix
;from psedocode in http://en.wikipedia.org/wiki/Reduced_row_echelon_form#Reduced_row_echelon_form
(defun reduced-row-echelon-form (matrix)
  (let* ((m (copy-list matrix)) (i) (lead 0) (rows (length m)) (cols (length (nth 0 m))))
    (dotimes (r rows)
      (if (<= cols lead)
          (return-from reduced-row-echelon-form m))
      (setf i r)
      (while (eq (get-elem m i lead) 0)        
	(incf i)
        (if (eq rows i)
            (progn (setf i r)
              (incf lead)
              (if (eq cols lead)
                  (return-from reduced-row-echelon-form m)))))
      (if (not (eq i r))
          (swap-rows m i r))
      (apply-op-row m r (get-elem m r lead) #'/)
      (dotimes (j rows)
        (if (not (eq j r))
            (subtract m j (get-row m r (get-elem m j lead)))))
      (incf lead))
  m))

;create an identity matrix of the specfied dimension
(defun identity-matrix (dim)
  (let ((ident))
    (dotimes (i dim)
      (my-append ident (make-list dim :initial-element 0)))
    (dotimes (i dim) (set-elem ident i i 1))
    ident))

;layer of abstraction, in case we change the internal representation
;of matrices from a list of lists to something else
(defun create-matrix-from-list (lst)
  (labels ((list-of-lists-p (l)
                            (if (null l)
                                t
                              (and (listp (car l)) (list-of-lists-p (cdr l)))))
           (verify-lengths (n l)
                           (if (null l)
                               t
                             (and (eq n (length (car l))) (verify-lengths n (cdr l)))))
           (all-numbers1 (l)
                        (if (null l)
                            t
                          (and (numberp (car l)) (all-numbers1 (cdr l)))))
           (all-numbers2 (l)
                        (if (null l)
                            t
                          (and (all-numbers1 (car l)) (all-numbers2 (cdr l))))))
    (cond ((not (list-of-lists-p lst))                          (error "Not a list of lists"))
          ((not (verify-lengths (length (car lst)) (cdr lst)))  (error "Elements are of unequal lengths"))
          ((not (all-numbers2 lst))                             (error "List contains non-numerical elements"))
          (t                                                    (copy-list lst)))))

;create a matrix of the given dimensions, filled with NILs
;only this method or create-matrix-from-list should
;be used to create matrices
(defun create-matrix (r c)
  (let ((m))
    (dotimes (i r)
      (my-append m (make-list c)))
    m))

;split a matrix into two, on the indicated column position (inclusive; zero-based)
(defun split-matrix (m pos)
  (let ((m1) (m2))
    (dolist (row m)
      (my-append m1 (first-n row (1+ pos)))
      (my-append m2 (last row (- (length row) pos 1))))
  (values m1 m2)))

;paste two matrices together
(defun paste-matrices (m1 m2)
  (let ((a))
    (dotimes (i (length m1))
      (my-append a (append (nth i m1) (nth i m2))))
    a))

;inverse of a matrix
(defun inv (matrix)
  (let ((n (length matrix)))
    (multiple-value-bind (m1 m2) 
        (split-matrix (reduced-row-echelon-form (paste-matrices matrix (identity-matrix n))) (1- n))
      m2)))

;multiply two matrices
(defun mult (m1 m2)
  (let* ((r (length m1)) (c (length (nth 0 m2))) (p (create-matrix r c)))
    (dotimes (i r)
      (dotimes (j c)
        (set-elem p i j (dot-product (get-row m1 i 1) (get-col m2 j 1)))))
    p))

;transpose of a matrix
(defun trans (m)
  (let ((tr))
    (dotimes (i (length (nth 0 m)))
      (my-append tr (get-col m i 1)))
    tr))
