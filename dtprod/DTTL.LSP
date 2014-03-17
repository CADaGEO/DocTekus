;; par MNT Reponse No 18
;; http://www.cadxp.com/modules.php?op=modload&name=XForum&file=viewthread&tid=2683#pid6828
;; Repris 2014, CADaGEO - Cristel LEGRAND
;;Version 1.0.0 du 06/02/2014
;;
;;This file is part of Doctekus.
;;
;;    Doctekus is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    Doctekus is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with Foobar.  If not, see <http://www.gnu.org/licenses/>
;;
;;
;DESCRIPTION ********************************************************
;********************************************************************
;; Totaux Longueur : ligne, arc, polyligne, cercle, spline, ellipse
;;
;;INSTALLATION ET FONCTIONNEMENT *************************************
;*********************************************************************
;;
;; Charger le fichier LSP - Taper DTtl pour éxécuter
;;
;;CHANGELOG **********************************************************
;;********************************************************************
;;
;;Version 1.0.0 du 06/02/2014
;;- Version initiale, intégrée à DocTekus après modification du mode de sortie du résultat 
;;		(pour pouvoir copier/coller le résultat dans un funiculaire)
;;
;;DEPENDANCES *******************************************************
;;*******************************************************************
;;
;;TODO, pochaine version *********************************************
;;********************************************************************
;;
;**********************************************************************
;**********************************************************************

(defun C:DTtl (/ ss tl n ent itm obj l)
	(setq ss (ssget)
	tl 0
	n (1- (sslength ss)))
	(while (>= n 0)
	(setq ent (entget (setq itm (ssname ss n)))
	obj (cdr (assoc 0 ent))
	l (cond
	((= obj "LINE")
	(distance (cdr (assoc 10 ent))(cdr (assoc 11 ent))))
	((= obj "ARC")
	(* (cdr (assoc 40 ent))
	(if (minusp (setq l (- (cdr (assoc 51 ent))
	(cdr (assoc 50 ent)))))
	(+ pi pi l) l)))
	((or (= obj "CIRCLE")(= obj "SPLINE")(= obj "POLYLINE")
	(= obj "LWPOLYLINE")(= obj "ELLIPSE"))
	(command "_.area" "_o" itm)
	(getvar "perimeter"))
	(T 0))
	tl (+ tl l)
	n (1- n)))
(princ (strcat "TOTAUX Longueur= " (rtos tl)))
(princ)
)
(prompt
  "
	DTTL par MNT Reponse No 18 charge v1.0.0 - taper DTTL pour obtenir le total des longueurs de plusieurs objets (licence GNU GPL v3)"
)

(princ)
;Clean chargement
