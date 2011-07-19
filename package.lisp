;;;; package.lisp

(defpackage #:matrix
  (:use #:cl #:utils)
  (:export "PRINT-MATRIX"
	   "REDUCED-ROW-ECHELON-FORM"
	   "IDENTITY-MATRIX"
	   "CREATE-MATRIX-FROM-LIST"
	   "CREATE-MATRIX"
	   "SPLIT-MATRIX"
	   "PASTE-MATRICES"
	   "INV"
	   "MULT"
	   "TRANS"))



