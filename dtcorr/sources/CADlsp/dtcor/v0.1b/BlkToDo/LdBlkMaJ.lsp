;; Nom du fichier : LdBlkMaJ.lsp
;; Date premi�re cr�ation : 01/03/2002
;; Date derni�re modification : 01/07/2002 par CADaGEO - CLE

;; Fichier de fonctions utilis� :
;;
;;	1) ld-ut.lsp

;;		Ligne de commande = UPBLK

;; Note :

;; TxtEntities -> Monte une liste de toute les entit�s TEXT ou MTEXT rencontr�es dans la base de donn�e dessin

(defun BlkEntities (/ e1 e2 l1 l2)
  (setq l1 '())
  (setq e1 (entnext))
  (if e1
    (progn
      (if (= (cdr (assoc 0 (entget e1))) "INSERT")

	(setq l1 (cons (entget e1) l1))
      )
      (while (setq e2 (entnext e1))
	(setq e1 e2)
	(if (= (cdr (assoc 0 (entget e1))) "INSERT")

	  (setq l1 (cons (entget e1) l1))
	)
      )
    )
  )
  (UtReturn l1)
)

;; Main function

(defun C:UPBLK (/ TxtLst ep1 nnn nb)
  (load "ld-ut")
  (setq	ep1 (getvar "OSMODE")
	nb  0
  )
  (setvar "OSMODE" 0)
  (setq Tri (strcase (substr (getvar "DWGNAME") 1 3)))
  (command "_-XREF" "Decharger" "*")
  (if (setq BlkLst (BlkEntities))
    (progn
      (foreach nnn BlkLst
	(princ (strcase (substr (cdr(assoc 2 nnn)) 11 2)))
	(princ (strcase (substr (cdr (assoc 2 nnn)) 1 3)))
	(princ (strcase (cdr (assoc 410 nnn))))
	(princ "\n")
	(if (and (= (strcase (substr (cdr (assoc 2 nnn)) 1 3)) Tri)
		(/= (strcase (cdr (assoc 410 nnn))) "MODEL")
		 ;; Uniquement les blocs Esp. PAPIER
		(or
			(= (strcase (substr (cdr(assoc 2 nnn)) 11 2)) "CR")
			;; cartouche
			(= (strcase (substr (cdr (assoc 2 nnn)) 11 2)) "LG")
			;; l�gende
			(= (strcase (substr (cdr(assoc 2 nnn)) 11 2)) "EN")
			;; encartage
			(= (strcase (substr (cdr (assoc 2 nnn)) 11 2)) "Z1")
			;; Agt Chr
			(= (strcase (substr (cdr (assoc 2 nnn)) 11 2)) "Z2")
			;; Fx
			(= (strcase (substr (cdr (assoc 2 nnn)) 11 2)) "Z3")
			;; tranch�e
		)
	    )
	  (progn
	    (princ "ok")
	    (setq BName	(strcat	(cdr (assoc 2 nnn))
				"="
				(substr	(getvar "DWGPREFIX")
					1
					(- (strlen (getvar "DWGPREFIX")) 5)
				)
				"\\BLOCS\\"
				(cdr (assoc 2 nnn)) ".DWG"
			)
	    )
	    (progn
		  (princ "ok 2")
		(command "_-INSERT" BName (cdr (assoc 10 nnn)))
    		(command)
    		(command "_resume")
	      )
	    (setq nb (1+ nb))
	  ); if CR
	); progn
      );if bloc a recharger
    ); for each
    (UtDispError "Entit�s 'Block' introuvables")
  );if
  (command "_-XREF" "Recharger" "*")
					;(alert (strcat (itoa nb) " entit�s 'Block' mises � jour"))
  (command "_ZOOM" "ET")
  (setvar "OSMODE" ep1)
)
(princ)

 ;|�Visual LISP� Format Options�
(80 2 40 2 nil "end of " 80 9 1 0 0 T T nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;