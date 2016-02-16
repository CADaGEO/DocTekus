;; source http://cadxp.com/topic/37076-zone-tampon-ou-buffer-dune-polyligne/
;; Merci fabcad et bonuscad

;; Cette commande permet de créer un buffer de type polyligne fermée à partir d'un seule et unique polyligne source à sélectionner
;; Modifiée par Cristel Legrand le 03/12/2014 pour pouvoir l'appeler depuis un programme lisp


(vl-load-com)
(defun l-coor2l-pt (lst flag / )
  (if lst
    (cons
      (list
        (car lst)
        (cadr lst)
        (if flag
          (+ (if (vlax-property-available-p ename 'Elevation) (vlax-get ename 'Elevation) 0.0) (caddr lst))
          (if (vlax-property-available-p ename 'Elevation) (vlax-get ename 'Elevation) 0.0)
        )
      )
      (l-coor2l-pt (if flag (cdddr lst) (cddr lst)) flag)
    )
  )
)
(defun c:buffer ( / js ent e_width)
  (princ "\nSélectionner la polyligne à bufferiser")
  (while
    (not
      (setq js
        (ssget "_+.:E:S" 
          (list
            (cons 0 "*POLYLINE")
            (cons 67 (if (eq (getvar "CVPORT") 2) 0 1))
            (cons 410 (if (eq (getvar "CVPORT") 2) "Model" (getvar "CTAB")))
            (cons -4 "<NOT")
             (cons -4 "&") (cons 70 113)
            (cons -4 "NOT>")
          )
        )
      )
    )
  )
  (setq  ent (ssname js 0))
  
  (setq e_width (getdist "\nLargeur de la zone tampon: "))
 
   (F_buffer ent e_width)
  (prin1)
)

(defun F_buffer (ent e_width / AcDoc Space ent vla_obj ename l_pt)

  (setq
    AcDoc (vla-get-ActiveDocument (vlax-get-acad-object))
    Space
    (if (eq (getvar "CVPORT") 1)
      (vla-get-PaperSpace AcDoc)
      (vla-get-ModelSpace AcDoc)
    )
    vla_obj (vlax-ename->vla-object ent)
  )

  (vla-Offset vla_obj (* 0.5 e_width))
  (setq
    ename (vlax-ename->vla-object (entlast))
    l_pt (l-coor2l-pt (vlax-get ename 'Coordinates) nil)
  )
  (entdel (entlast))
  (vla-Offset vla_obj (- (* 0.5 e_width)))
  (setq
    ename (vlax-ename->vla-object (entlast))
    l_pt (append (reverse (l-coor2l-pt (vlax-get ename 'Coordinates) nil)) l_pt)
  )
  (entdel (entlast))
  (setq nw_pl (vlax-invoke Space 'AddLightWeightPolyline (apply 'append (mapcar 'list (mapcar 'car l_pt) (mapcar 'cadr l_pt)))))
  (vla-put-Closed nw_pl 1)
  (copy_data ent (entlast) nil)
  (prin1)
)