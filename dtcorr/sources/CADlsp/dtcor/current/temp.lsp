(defun c:MAJ-BP_date ( / def_date ss atts)
(defun def_date (td / j y d m)
(setq
j (- (fix td) 1721119.0)
y (fix (/ (1- (* 4 j)) 146097.0))
j (- (* j 4.0) 1.0 (* 146097.0 y))
d (fix (/ j 4.0))
j (fix (/ (+ (* 4.0 d) 3.0) 1461.0))
d (- (+ (* 4.0 d) 3.0) (* 1461.0 j))
d (fix (/ (+ d 4.0) 4.0))
m (fix (/ (- (* 5.0 d) 3) 153.0))
d (- (* 5.0 d) 3.0 (* 153.0 m))
d (fix (/ (+ d 5.0) 5.0))
y (+ (* 100.0 y) j)
)
(if (< m 10.0)
(setq m (+ m 3))
(setq m (- m 9) y (1+ y))
)
(strcat
(itoa (fix d))
"."
(itoa (fix m))
"."
(itoa (fix y))
)
)
(vl-load-com)
(if (ssget "_X" '((0 . "INSERT") (8 . "0-MISE-EN-PAGE") (2 . "BP_date_base_DAO") (66 . 1)))
(progn
(setq ss
(vla-get-activeselectionset
(vla-get-activedocument
(vlax-get-acad-object)
)
)
)
(vlax-for blk ss
(setq atts (vlax-invoke blk 'getattributes))
(foreach att atts
(if (eq (vla-get-TagString att) "DATE")
(vla-put-TextString att (def_date (getvar "TDUPDATE")))
)
)
)
)
)
(prin1)
)