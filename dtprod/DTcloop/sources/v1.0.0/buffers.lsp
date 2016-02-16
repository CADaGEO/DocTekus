;; source http://cadxp.com/topic/37076-zone-tampon-ou-buffer-dune-polyligne/
;; Merci fabcad et bonuscad


;; Ajouté à DTcloop
;; Cette commande permet de créer un buffer de type polyligne fermée à partir d'un jeu de polylignes sources à sélectionner

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
(defun buffer ( item intDist / AcDoc Space ent vla_obj e_width ename l_pt)

  (setq
    AcDoc (vla-get-ActiveDocument (vlax-get-acad-object))
    Space
    (if (eq (getvar "CVPORT") 1)
      (vla-get-PaperSpace AcDoc)
      (vla-get-ModelSpace AcDoc)
    )
    ent (ssname item 0)
    vla_obj (vlax-ename->vla-object ent)
  )
  (initget 7)
  (setq e_width intDist)
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
  (prin1)
)



; -----------------------------------------------------------------------------------------------
(defun c:DTcloop()

;On récupère la distance choisie par l'utilisateur
(setq intDist (getreal "\nDistance : "))

;On récupère la sélection de l'utilisateur
(setq sset (ssget))

; et on filtre pour ne conserver que ce qui convient à notre commande
(setq sset
        (ssget sset 
          (list
            (cons 0 "*POLYLINE")
            (cons 67 (if (eq (getvar "CVPORT") 2) 0 1))
            (cons 410 (if (eq (getvar "CVPORT") 2) "Model" (getvar "CTAB")))
            (cons -4 "<NOT")
             (cons -4 "&") (cons 70 113)
            (cons -4 "NOT>")
          )

;On défini la variable qui boucle
(setq i 0)

;On boucle sur chaque élément
(repeat (sslength sset)
(setq item (ssname sset i))
	(buffer item intDist)
(setq i (1+ i))
);repeat
	
(princ)

) ;defun

(princ)