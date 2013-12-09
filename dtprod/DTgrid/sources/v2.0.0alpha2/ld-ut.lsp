;;;             Nom du fichier : ld-ut.lsp
;;; Date dernière modification : 09/02/2000
;;;                  Par : Pierre RIEGERT - Cad.UC
;;;                    
;;;
;;; Commentaires :
;;;
;;;    Fonctions utilitaires
;;;
;;;


;;; Retourne la valeur k

(defun UtReturn (k) (setq k k))


;;; Gestion des variables

(defun UtSaveVar ()
  (setq	$Sav
	 (mapcar 'getvar (mapcar 'car $Var))
  )
)


(defun UtInitVar ()
  (mapcar
    'setvar
    (mapcar 'car $var)
    (mapcar 'cdr $var)
  )
)

(defun UtRestVar ()
  (mapcar
    'setvar
    (mapcar 'car $Var)
    $Sav
  )
  (setq	$Sav nil
	$Var nil
  )
)

;;; Simulation de la touche F3 (Accrochages AC/IN)

(defun UtF3off ()
  (setvar "osmode" (logior (getvar "osmode") 16384))
)

(defun UtF3on ()
  (setvar "osmode" (logand (getvar "osmode") 16383))
)

;;; Gestion des erreurs

(defun UtInitError ()
  (setq $OldError *error*)
  (setq *error* UtLocalError)
)

(defun UtRestError ()
  (setq *error* $OldError)
  (setq $OldError nil)
  (setq $ErrorList nil)
)

(defun UtLocalError (ch)

  (cond	((= ch "Fonction annulée") (prompt "Arrêt Utilisateur"))
	((wcmatch ch "*quit*") (princ))
	(t (alert ch) (exit))
  )
  (UtRestVar)
  (UtRestError)
  (princ)
)

(defun UtDispError (Msg)
  (alert Msg)
  (exit)
)

(defun c:ErOff ()
  (defun UtInitError () (setq *error* nil))
)
(defun c:ErOn ()
  (defun UtInitError ()
    (setq $OldError *error*)
    (setq *error* UtLocalError)
  )
)

;;; Noms de fichiers et de repertoire

;; Extraction nom du fichier d'un chemin complet
(defun UtFname (FileName /)
  (while (wcmatch FileName "*/*,*\\*")
    (setq FileName (substr FileName 2))
  )
  (UtReturn FileName)
)

;; Extraction du repertoire d'un nom complet
(defun UtRep (FileName / x y)
  (setq x (strlen (UtFname FileName)))
  (setq y (strlen FileName))
  (substr Filename 1 (- y x))
)

;;Pour valider un nom de fichier DOS sur 8 + 3 caractères. Retourne T ou Nil
(defun UtDosFileName (FileName / i c Nom Ext)
  (setq	Nom ""
	Ext ""
  )
  (if (wcmatch Filename "*.*")
    (progn
      (setq i 1)
      (while
	(/= (setq c (substr FileName i 1)) ".")
	 (setq Nom (strcat Nom c))
	 (setq i (1+ i))
      )
      (setq Ext (substr FileName (1+ i)))
    )
    (setq Nom FileName)
  )
  (if
    (and
      (> (strlen Nom) 0)
      (< (strlen Nom) 9)
      (< (strlen Ext) 4)
      (not
	(wcmatch
	  (strcat Nom Ext)
	  (strcat
	    "* *,"     "*`?*,"	  "*\"*,"    "*`\\*,"	"*/*,"
	    "*<*,"     "*>*,"	  "*`**,"    "*|*,"	"*:*,"
	    "*(*,"     "*)*,"	  "*;*,"     "*+*,"	"*`,*,"
	    "*=*,"     "*`[*,"	  "*`]*,"    "*.*"
	   )
	)
      )
    )
     (UtReturn (not nil))
     (UtReturn nil)
  )
)


;;; Fonction de debugage pour pister une variable

(defun UtVisu (StrVar / Var Val)
  (setq Var (read StrVar))
  (setq Val (eval Var))
  (princ (strcat "\n" StrVar " : "))
  (princ Val)
  (if (and (= 'STR (type Val)) (= 8 (getvar "CMDACTIVE")))
    (alert (strcat "\n" StrVar " : " Val))
  )
  (princ)
)

;;; Fonction pour qu'un programme AutoLisp puisse être annulé comme une seul commande

(defun UtUndoInit ()
  (setvar "CMDECHO" 0)
  (command "_UNDO" "_BE")
  (setvar "CMDECHO" 1)
)
(defun UtUndoEnd ()
  (setvar "CMDECHO" 0)
  (command "_UNDO" "_E")
  (setvar "CMDECHO" 1)
)


;;; Gestion de listes

(defun UtSupNieme (Liste Nieme / i len Result)
  ;; supprime le 'Nième' élément de la liste 'Liste'.
  ;; La numérotation commence à Zéro!!!
  (setq len (length Liste))
  (cond
    ((or (>= Nieme len) (minusp Nieme)) (setq Result Liste))
    (t
     (setq i 0)
     (while (< i len)
       (if (/= i Nieme)
	 (setq Result (append Result (list (nth i Liste))))
       )
       (setq i (1+ i))
     )
    )
  )
  (UtReturn Result)
)
(defun UtAddNieme (Liste Nieme NewElt / i len Result)
  ;; Insere 'NewElt' dans 'Liste' à la 'Nième' position, ou à la fin
  ;; si 'Nieme' est trop grand. La numérotation commence à Zéro!!!
  (setq len (length Liste))
  (cond
    ((>= Nieme Len) (setq Result (append Liste (list NewElt))))
    ((minusp Nieme) (setq Result Liste))
    (t
     (setq i 0)
     (while (< i len)
       (if (= i Nieme)
	 (setq Result (append Result (list NewElt)))
       )
       (setq Result (append Result (list (nth i Liste))))
       (setq i (1+ i))
     )
    )
  )
  (UtReturn Result)
)
(defun UtModNieme (Liste Nieme NewElt / i len Result)
  ;; Remplace le 'Nième' élément de 'Liste' par 'NewElt' 
  ;; Si 'Nieme' est trop grand il ne se passe rien
  ;; La numérotation commence à Zéro!!!
  (setq len (length Liste))
  (cond
    ((>= Nieme Len) (setq Result Liste))
    ((minusp Nieme) (setq Result Liste))
    (t
     (setq i 0)
     (while (< i len)
       (if (= i Nieme)
	 (setq Result (append Result (list NewElt)))
	 (setq Result (append Result (list (nth i Liste))))
       )
       (setq i (1+ i))
     )
    )
  )
  (UtReturn Result)
)
(defun UtAvanceNieme (Liste Nieme / len x y)
  ;; Déplace le Nième élément d'un cran vers l'avant de la liste
  (setq len (length Liste))
  (cond
    ((>= Nieme Len) (setq Result Liste))
    ((minusp Nieme) (setq Result Liste))
    ((zerop Nieme) (setq Result Liste))
    (t
     (setq x (nth Nieme Liste))
     (setq y (nth (1- Nieme) Liste))
     (setq Result (UtModNieme Liste (1- Nieme) x))
     (setq Result (UtModNieme Result Nieme y))
    )
  )
  (UtReturn Result)
)
(defun UtReculeNieme (Liste Nieme / len x y)
  ;; Déplace le Nième élément d'un cran vers l'arrière de la liste
  (setq len (length Liste))
  (cond
    ((>= Nieme Len) (setq Result Liste))
    ((minusp Nieme) (setq Result Liste))
    (t
     (setq x (nth Nieme Liste))
     (setq y (nth (1+ Nieme) Liste))
     (setq Result (UtModNieme Liste (1+ Nieme) x))
     (setq Result (UtModNieme Result Nieme y))
    )
  )
  (UtReturn Result)
)
(defun UtSupEltListe (Elt Liste fuzz / nnn Result)
  ;; Supprime toute occurence de Elt dans Liste
  (if (/= (type fuzz) 'REAL)
    (setq fuzz 0)
  )
  (foreach nnn Liste
    (if
      (not (equal nnn Elt fuzz))
       (setq Result (append Result (list nnn)))
    )
  )
  (UtReturn Result)
)

(defun UtSupDoublonsList (Liste / Result)
  ;; Supprime les doublons d'une liste
  (foreach nnn Liste
    (if	(not (member nnn Result))
      (setq Result (append Result (list nnn)))
    )
  )
  (UtReturn Result)
)

(defun UtPosiEltInList (Elt Liste / l1 l2)
  ;; Retourne la position d'un élément dans une liste à partir de zéro
  (setq l1 (length Liste))
  (setq l2 (length (member Elt Liste)))
  (- l1 l2)
)

(defun UtMilieu	(p1 p2 /)
  ;; Retourne le milieu de 2 points
  (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2)
)

(defun UtDiag (a c / m d pi/2 ag)
  ;; Retourne les points b et d d'un carré a b c d
  ;; sous la forme (b d)
  (setq m (milieu a c))
  (setq d (distance a m))
  (setq pi/2 (/ pi 2.0))
  (setq ag (+ (angle a c) pi/2))
  (list
    (polar m ag d)
    (polar m ag (- d))
  )
)

(defun UtNumList (Debut Nbelt Inc / i Out)
  ;; retourne une liste de Nbelt nombres à partir de Debut de Inc en Inc
  (setq i Debut)
  (repeat Nbelt
    (setq Out (cons i Out))
    (setq i (+ i Inc))
  )
  (reverse Out)
)

(defun UtInterList (a b / r nnn)
  (foreach nnn a
    (if	(member nnn b)
      (setq r (cons nnn r))
    )
  )
  (UtReturn r)
)

(defun UtFiltreList (test Ori / New nnn)
  (foreach nnn Ori
    (if	((eval test) nnn)
      (setq New (cons nnn New))
    )
  )
  (set test nil)
  (reverse New)
)

(defun UtCouples (L / R nnn)
  ;; retourne la liste des couples de L (A B C) renvoie ((A B) (A C) (B C))
  (apply 'append
	 (mapcar
	   '(lambda (x)
	      (mapcar '(lambda (z) (cons x z)) (cdr (member x L)))
	    )
	   L
	 )
  )
)

(defun UtSortNum (L / R S x ok)
  ;; trie une liste de nombres
  (while L
    (setq x (apply 'max L))
    (setq R (cons x R))
    (setq S nil)
    (setq ok nil)
    (foreach nnn L
      (cond
	(
	 (or ok (/= nnn x))
	 (setq S (cons nnn S))
	)
	(t (setq ok T))
      )
    )
    (setq L S)
  )
  (Utreturn R)
)

;;; Fonctions numériques
(defun deg2rad (a)
  (/ (* pi a) 180.0)
)

(defun rad2deg (a)
  (/ (* 180 a) pi)
)

;;; Fonctions de lecture et d'écriture de XDATA

;; Pour tester :
;; 1)dessiner un objet AutoCAD
;; 2)taper	(UtWriteXd (entlast) '("1" "Deux" "3") "ApplicTest")
;; 3 puis	(UtReadXd (entlast) "ApplicTest")

;;==========================================================================================================================
(defun UtWriteXd (ne liste app / xd ene x)

  ;;Création éventuelle de l'application app
  (regapp app)

  ;;Ecriture de la liste des xdata
  (setq	xd
	 (mapcar
	   '(lambda (z)
	      (cond
		((= 'STR (type z)) (cons 1000 z))
		((= 'REAL (type z)) (cons 1040 z))
		((= 'INT (type z)) (cons 1070 z))
		(
		 (and (= 'ENAME (type z)) (entget z))
		 (cons 1005 (cdr (assoc 5 (entget z))))
		)
		(
		 (not z)
		 (cons 1005 "0")
		)
		(
		 (and (= 'ENAME (type z)) (not (entget z)))
		 (exit)
		)
	      )
	    )
	   liste
	 )
  )
  (setq xd (cons '(1002 . "{") xd))
  (setq xd (append xd '((1002 . "}"))))
  (setq xd (cons app xd))
  (setq xd (list (cons -3 (list xd))))

  ;;Mise à jour de l'entité
  (setq ene (entget ne))
  (setq x (entmod (append ene xd)))
  (UtReturn x)
)

;;==========================================================================================================================
(defun UtReadXd	(ne app / xd)
  (if (and (= 'ENAME (type ne)) (= 'STR (type app)))
    (progn
      (setq xd (cdr (assoc -3 (entget ne (list app)))))
      (if xd
	(progn
	  (setq xd (reverse (cdr (reverse (cddar xd)))))
	  (mapcar
	    '(lambda (z)
	       (if (= 1005 (car z))
		 (handent (cdr z))
		 (cdr z)
	       )
	     )
	    xd
	  )
	)
      )
    )
  )
)

;;==========================================================================================================================
(defun UtEraseXd (ne app / xd ene x)
  (if (and
	(= 'ENAME (type ne))
	(= 'STR (type app))
	(tblsearch "APPID" app)
      )
    (progn
      (setq ene (entget ne))
      (setq xd (list (list app)))
      (setq xd (list (cons -3 xd)))
      (setq x (entmod (append ene xd)))
    )
  )
  (UtReturn x)
)

(defun UtReturn (x) (setq x x))


;;Edition des XData par une Boîte de dialogue

(defun c:UtEditXd (/ App Obj)

  ;; Saisie d'une application
  (setq App (getstring "\nNom de l'application <AutoGraph> : "))
  (if (= App "")
    (setq App "AUTOGRAPH")
  )

  ;; Saisie d'un objet
  (setq Obj (car (entsel)))
  (if (not Obj)
    (exit)
  )

  ;; Extraction des Xdata dans une liste
  (setq OriDataList (UtReadXd Obj App))
  (setq DataList OriDataList)

  ;; Edition de la liste par boîte de dialogue

  ;; Chargement du dialogue
  (if (minusp (setq DclId (load_dialog "AUTOGRPH")))
    (exit)
  )
  (if (not (new_dialog "EditXd" DclId "" '(700 110)))
    (exit)
  )

  ;; Initialisations
  (UtEditXdInit)

  ;; actions
  (action_tile "DATALIST" "(UtEditXdInit)")
  (action_tile
    "DELETE"
    (strcat
      "(setq DataList (UtSupNieme DataList (atoi (get_tile \"DATALIST\"))))"
      "(UtEditXdInit)"
    )
  )
  (action_tile
    "ADD"
    (strcat
      "(setq DataList "		"(UtAddNieme "
      "DataList "		"(atoi (get_tile \"DATALIST\")) "
      "(get_tile \"DATA\")"	")"
      ")"			"(UtEditXdInit)"
     )
  )
  (action_tile
    "MODIFY"
    (strcat
      "(setq DataList "		"(UtModNieme "
      "DataList "		"(atoi (get_tile \"DATALIST\")) "
      "(get_tile \"DATA\")"	")"
      ")"			"(UtEditXdInit)"
     )
  )
  (action_tile
    "MONTER"
    (strcat
      "(setq DataList "
      "(UtAvanceNieme "
      "DataList "
      "(atoi (get_tile \"DATALIST\")) "
      ")"
      ")"
      "(set_tile \"DATALIST\" (itoa (1- (atoi (get_tile \"DATALIST\")))))"
      "(UtEditXdInit)"
     )
  )
  (action_tile
    "DESCENDRE"
    (strcat
      "(setq DataList "
      "(UtReculeNieme "
      "DataList "
      "(atoi (get_tile \"DATALIST\")) "
      ")"
      ")"
      "(set_tile \"DATALIST\" (itoa (1+ (atoi (get_tile \"DATALIST\")))))"
      "(UtEditXdInit)"
     )
  )
  (action_tile
    "cancel"
    "(setq DataList OriDataList)(done_dialog)"
  )
  (action_tile "accept" "(done_dialog)")

  ;; début du dialogue
  (start_dialog)

  ;; Fin du dialogue
  (unload_dialog DclId)


  ;; Ecriture des nouveau Xdata en sortie ou effacement
  (if DataList
    (UtWriteXd Obj DataList App)
    (UtEraseXd Obj App)
  )

  ;; Sortie propre
  (princ)
)

(defun UtEditXdInit (/ x y)

  (setq x (get_tile "DATALIST"))
  (if (= x "")
    (setq x "0")
  )

  ;; Mise à jour de la liste
  (start_list "DATALIST")
  (mapcar 'add_list (append DataList '(" ")))
  (end_list)

  ;; Sélection dans la liste
  (set_tile "DATALIST" x)

  (setq y (atoi x))
  (setq len (length DATALIST))

  (if (= y len)
    (progn
      (mode_tile "DELETE" 1)
      (mode_tile "MODIFY" 1)
      (set_tile "DATA" "New")
      (mode_tile "DATA" 2)
    )
    (progn
      (mode_tile "DELETE" 0)
      (mode_tile "MODIFY" 0)
      (set_tile "DATA" (nth y (append DataList '(" "))))
      (mode_tile "DATA" 2)
    )
  )
  (if (= y 0)
    (mode_tile "MONTER" 1)
    (mode_tile "MONTER" 0)
  )
  (if (= y (1- len))
    (mode_tile "DESCENDRE" 1)
    (mode_tile "DESCENDRE" 0)
  )
  (if (> y (1- len))
    (progn (mode_tile "DESCENDRE" 1) (mode_tile "MONTER" 1))
  )
)

;;; Gestion de chaînes de caractères

;; UtCaseStrLst -> passe les chaînes de caractère dans une liste en majuscule
(defun UtCaseStrLst (lst / l1 nnn)
  (foreach nnn lst
    (setq nnn (strcase nnn))
    (setq l1 (cons nnn l1))
  )
  (setq l1 (reverse l1))
)

;; Les n derniers caractères d'une chaîne
(defun UtRightStr (ch n /)
  (setq len (strlen ch))
  (cond
    ((>= n len) ch)
    (t (substr ch (- len n -1)))
  )
)
;; Suppression des espaces à Gauche
(defun UtSupLeftSpaces (x /)
  (while (wcmatch x " *") (setq x (substr x 2)))
  (UtReturn x)
)

;; Suppression des espaces à Droite
(defun UtSupRightSpaces	(x /)
  (while (wcmatch x "* ")
    (setq x (substr x 1 (1- (strlen x))))
  )
  (UtReturn x)
)

;; Suppression de certains caractères d'une chaîne
(defun UtSupSpaces (x match / c xx)
  (setq xx "")
  (while (/= (setq c (substr x 1 1)) "")
    (if	(not (wcmatch c match))
      (setq xx (strcat xx c))
    )
    (setq x (substr x 2))
  )
  (UtReturn xx)
)
;; n caractères de S comblé avec des espaces à gauche
(defun UtFormat	(S n /)
  (while (<= (strlen S) n) (setq S (strcat " " S)))
  (UtRightStr S n)
)
;;------------------------------------------
;; UtFillRight
;; -----------
;;		Complète la Chaine par des Blancs
;;
;;------------------------------------------
(defun UtFillRight (S n)
  (while (< (strlen S) n) (setq S (strcat S " ")))
  (UtReturn S)
)

;; Complète la chaine avec n espaces à gauche 

(defun UtFillLeft (S n)
  (while (< (strlen S) n) (setq S (strcat " " S)))
  (UtReturn S)
)


;;; Saucissonne une chaine en n morceaux. (UtSaucisse "ABCDEF" 2) -> ("AB" "CD" "EF")

(defun UtSaucisse (s n / ls)
  (while (/= s "")
    (setq ls (cons (substr s 1 n) ls))
    (setq s (substr s (1+ n)))
  )
  (reverse ls)
)

;; transforme une chaine en liste, selon le séparateur spécifié
(defun UtStr2lst (str sep / ch i len lst)
  (setq	x   ""
	i   1
	len (strlen str)
  )
  (while (<= i len)
    (setq ch (substr str i 1))
    (setq i (1+ i))
    (cond
      ((= ch sep)
       (setq lst (cons x lst)
	     x	 ""
       )
      )
      (t (setq x (strcat x ch)))
    )
  )
  (setq lst (reverse (cons x lst)))
  (if (equal lst '(""))
    (setq lst nil)
    (UtReturn Lst)
  )
)

;; et la réciproque, si la liste contient des chaînes
(defun UtLst2Str (Lst Sep /)
  (setq Str (apply 'strcat (mapcar '(lambda (z) (strcat z sep)) Lst)))
  (if (/= Str "")
    (substr Str 1 (1- (strlen Str)))
    Str
  )
)

;; remplace \n par \r\n dans ch

(defun UtAddCR (ch / i Len x y z New)
  (setq Len (strlen ch))
  (setq i 1)
  (setq New "")
  (while (<= i Len)
    (setq x (substr ch i 1))
    (setq y (substr ch i 2))
    (cond
      ((= y "\r\n") (setq z y) (setq i (+ i 2)))
      ((/= x "\n") (setq z x) (setq i (1+ i)))
      (t (setq z "\r\n") (setq i (1+ i)))
    )
    (setq New (strcat New z))
  )
  (UtReturn New)
)

;; recherche et remplace toutes les occurences de Str1 par Str2 dans Str
(defun UtStrReplace (Str1 Str2 Str / R l l1 l2 i)
  (setq l (strlen Str))
  (setq l1 (strlen Str1))
  (setq l2 (strlen Str2))
  (setq i 1)
  (setq R "")
  (while (/= STR "")
    (cond
      ((wcmatch (substr Str 1 l1) Str1)
       (setq R (strcat R Str2))
       (setq Str (substr Str l1))
       (setq i (+ i l1))
      )
      (t
       (setq R (strcat R (substr Str 1 1)))
      )
    )
    (setq i (1+ i))
    (setq str (substr str 2))
  )
  (utreturn R)
)

(defun UtSuppAccents (Str / LoStr n LfStr)
  (setq LoStr (reverse (UtSaucisse Str 1)))
  (foreach nnn LoStr
    (cond
      ((wcmatch nnn "é,è,ê,ë") (setq LfStr (cons "e" LfStr)))
      ((wcmatch nnn "à,â,ä,á") (setq LfStr (cons "a" LfStr)))
      ((wcmatch nnn "î,ï,í") (setq LfStr (cons "i" LfStr)))
      ((wcmatch nnn "ô,ö") (setq LfStr (cons "o" LfStr)))
      ((wcmatch nnn "ù,û,ü") (setq LfStr (cons "u" LfStr)))
      ((wcmatch nnn "ñ") (setq LfStr (cons "n" LfStr)))
      (t (setq LfStr (cons nnn LfStr)))
    )
  )
  (apply 'strcat LfStr)
)

;;; Concernant les jeux de sélection

(defun UtSs2List (ss / i x y)
  (setq i 0)
  (if
    (not ss)
     (setq y nil)
     (while (setq x (ssname ss i))
       (setq y (cons x y))
       (setq i (1+ i))
     )
  )
  (UtReturn y)
)

(defun UtEntselC (Iget message d / e p j)
  (initget Iget)
  (setq e (entsel message))
  (cond
    ((eq 'LIST (type e))
     (setq p (cadr e))
     (setq j (ssget "_C"
		    (polar p (/ (* -3 pi) 4) d)
		    (polar p (/ pi 4) d)
	     )
     )
    )
    ((eq 'STR (type e))
     (setq j e)
    )
    ((not e) (setq j Iget))
  )
  (UtReturn j)
)

(defun UtSsFrom	(Ori / j x nx nx0)
  (setq	x Ori
	j (ssadd)
  )
  (while (setq x (entnext x))
    (setq nx (entget x))
    (setq nx0 (cdr (assoc 0 nx)))
    (if	(not (wcmatch nx0 "ATTRIB,SEQEND,VERTEX"))
      (ssadd x j)
    )
  )
  (if (> (sslength j) 0)
    j
    nil
  )
)

;;; Pour jouer sur les intersections de polygones

;; Cette fonction renvoie T si 2 contours sont disjoints, nil s'ils intersectent
(defun UtDisjoint (C1 C2 /)
  (command "_COPY" C1 "" '(0 0 0) '(0 0 0))
  (command "_REGION" (entlast) "")
  (setq C1 (entlast))
  (command "_COPY" C2 "" '(0 0 0) '(0 0 0))
  (command "_REGION" (entlast) "")
  (setq C2 (entlast))
  (command "_INTERSECT" C1 C2 "")
  (if (or (entdel C1) (entdel C2))
    nil
    T
  )
)
;; Généralisation de cette fonction à un Jeu de selection de contours : comparaisons 2 à 2.
(defun UtDisjoints (Jspoly / L Lc x)
  (setq L (UtSs2List JsPoly))
  (setq Lc (UtCouples L))
  (setq x (mapcar '(lambda (z) (UtDisjoint (car z) (cdr z))) lc))
  (apply 'and x)
)



;;; Fonctions arithmétiques

(defun UtArrond	(x mult / reste Val)
  (setq Val (fix (/ x mult)))
  (setq rest (rem x mult))
  (if (>= rest (/ mult 2.0))
    (setq Val (1+ Val))
  )
  (* Val mult)
)

(defun UtMemberR (r ListeR fuzz / ok nnn)
  (foreach nnn ListeR
    (if	(equal r nnn fuzz)
      (setq ok T)
    )
  )
  (UtReturn ok)
)

;;------------------------------------------
;; UtFieldPos
;; ----------
;;       Recherche la Position du Champs dans une Liste de Champs
;;		exemple : (setq Pos (FieldPos (DbfFields nh) "GAMME_ID"))
;;
;; Retour : La Position (0 à n)
;;
;;------------------------------------------
(defun UtFieldPos (ListeFields Field / l1 i OneField bNotFound)

  (setq l1 (length ListeFields))
  (setq i 0)
  (setq bNotFound 1)
  (while (and (< i l1) bNotFound)
    (setq OneField (nth i ListeFields))
    (if	(= (car OneField) Field)
      (setq bNotFound Nil)
      (setq i (1+ i))
    )
  )
  (UtReturn i)
)

;;; Conversions d'angles

(defun UtDeg2Rad (a) (/ (* pi a) 180))
(defun UtRad2Deg (a) (/ (* 180 a) pi))

;;; Fonctions Géométriques

;; Calcul de l'angle entre 3 points 2D (Sommet P1 P2)
(defun UtAng (S P1 P2 /)
  (cond
    ((equal P1 P2 0.0001) 0)
    ((equal S P1 0.0001) 0)
    ((equal S P2 0.0001) 0)
    (t (cal "ang(S,P1,P2)"))
  )
)

;; Similitude impropre (rotation + facteurs d'échelle en x y x)
(defun UtSimi (ori p ang fx fy / x y xx yy xxx yyy a b)
  (setq
    x (- (car p) (car Ori))
    y (- (cadr p) (cadr Ori))
  )
  (setq	xx (* x fx)
	yy (* y fy)
  )
  (setq	a (cos ang)
	b (sin ang)
  )
  (setq xxx (- (* a xx) (* b yy)))
  (setq yyy (+ (* b xx) (* a yy)))
  (mapcar '+ Ori (list xxx yyy 0))
)

;; Position relative d'un bloc inséré par rapport à un autre
;; retourne la distance, la direction, la rotation relative et le produit miroir
;;  sous la forme (D A R M)
;; Remarque : Si Fy < 0, alors on mettra la valeur absolue + rotation de 180°
;; Remarque : On suppose ici des facteurs d'echelle homogènes

(defun UtPosRelat (e f / nE nF Ep Er Ex Ey Fp Fr Fx Fy)
  (setq nE (entget e))
  (setq Ep (cdr (assoc 10 nE)))
  (setq Er (cdr (assoc 50 nE)))
  (setq Ex (cdr (assoc 41 nE)))
  (setq Ey (cdr (assoc 42 nE)))
  (if (minusp Ex)
    (setq Er (+ pi Er))
  )
  (if (minusp Ey)
    (setq Ex (- Ex)
	  Ey (- Ey)
	  Er (rem (+ Er pi) (* pi 2))
    )
  )
  (setq nF (entget f))
  (setq Fp (cdr (assoc 10 nF)))
  (setq Fr (cdr (assoc 50 nF)))
  (setq Fx (cdr (assoc 41 nF)))
  (setq Fy (cdr (assoc 42 nF)))
  (if (minusp Fx)
    (setq Fr (+ pi Fr))
  )
  (if (minusp Fy)
    (setq Fx (- Fx)
	  Fy (- Fy)
	  Fr (rem (+ Fr pi) (* pi 2))
    )
  )
  (setq Ex (UtArrond (/ Ex (abs Ex)) 1))
  (setq Fx (UtArrond (/ Fx (abs Fx)) 1))
  (setq Ey (UtArrond (/ Ey (abs Ey)) 1))
  (setq Fy (UtArrond (/ Fy (abs Fy)) 1))
  (if
    (and
      (= "INSERT" (cdr (assoc 0 nE)))
      (= "INSERT" (cdr (assoc 0 nF)))
      (= (abs Ex) (abs Ey) (abs Fx) (abs Fy))
    )
     (list
       (UtArrond (/ (distance Ep Fp) (abs (cdr (assoc 41 nF))))
		 0.0001
       )
       (rem (UtArrond (+ 360 (UtAng Ep (polar Ep Er 1) Fp)) 1) 360)
       (rem (UtArrond (+ 360 (UtRad2Deg (- Er Fr))) 1) 360)
					;(UtArrond (/ (* Ex Fx) (abs Fx)(abs Fy)) 1)
       (cond
	 ((and (= Ex 1) (= Fx 1)) 1)
	 ((and (= Ex 1) (= Fx -1)) 2)
	 ((and (= Ex -1) (= Fx 1)) 3)
	 ((and (= Ex -1) (= Fx -1)) 4)
       )
     )
  )
)
(defun c:PosRel	(/ j x y)
  (setq j (ssget))
  (setq	x (ssname j 0)
	y (ssname j 1)
  )
  (append
    (list
      (car (UtReadXd x "AutoGrph01"))
      (car (UtReadXd x "AutoGrph04"))
      (car (UtReadXd y "AutoGrph01"))
      (car (UtReadXd y "AutoGrph04"))
    )
    (UtPosRelat x y)
  )
)

;; Calcul de l'angle absolu SCG à partir d'un angle issu du SCU via (getorient)
(defun UtAngleScu2Scg (a / p1 p2 p delta)
  (setq p1 (trans '(1 0) 1 0))
  (setq p2 (trans '(0 0) 1 0))
  (setq p (mapcar '- p1 p2))
  (setq delta (atan (/ (cadr p) (car p))))
  (print a)
  (print delta)
  (+ a delta)
)


;;; Gestion du temps

(defun InitTime	(/)
  (setq $Time (getvar "CDATE"))
  (print
    (setq $Time (fix (* 100 100 100 (- $Time (fix $Time)))))
  )
)
(defun EndTime (/ x)
  (setq x $Time)
  (setq $Time (getvar "CDATE"))
  (print
    (setq $Time (fix (* 100 100 100 (- $Time (fix $Time)))))
  )
  (- $Time x)
)


;; Gel d'une entité

(defun Freeze (Ent / nEnt)
  (setq nEnt (entget Ent))
  (if (assoc 60 nEnt)
    (entmod (subst '(60 . 1) (assoc 60 nEnt) nEnt))
    (entmod (append nEnt '((60 . 1))))
  )
)

;; Libèration d'une entité

(defun UnFreeze	(Ent / nEnt)
  (setq nEnt (entget Ent))
  (if (assoc 60 nEnt)
    (entmod (subst '(60 . 0) (assoc 60 nEnt) nEnt))
  )
)

;; Gel d'un jeu de selection

(defun ssFreeze	(j / i x)
  (setq i 0)
  (while (setq x (ssname j i))
    (setq i (1+ i))
    (Freeze x)
  )
)

;; Libération d'un jeu de sélection

(defun ssUnFreeze (j / i x)
  (setq i 0)
  (while (setq x (ssname j i))
    (setq i (1+ i))
    (UnFreeze x)
  )
)

;;; Chaînes en paires pointées et réciproquement       "A";"B";"C";"D" <--> (("A" . "B") ("C" . "D"))

(defun UtStr2pp	(Str Sep / L Ret)
  (setq L (UtStr2lst Str Sep))
  (if (equal L '(""))
    (setq L nil)
  )
  (while L
    (setq Ret (cons (cons (car L) (cadr L)) Ret))
    (setq L (cddr L))
  )
  (reverse Ret)
)

(defun UtPp2Str	(PP Sep / S Ret x)
  (setq Ret "")
  (while PP
    (setq x (car PP))
    (setq Ret (strcat Ret (car x) Sep (cdr x)))
    (setq PP (cdr PP))
    (if	PP
      (setq Ret (strcat Ret Sep))
    )
  )
  (UtReturn Ret)
)

;;; Fonctions géométriques
;; Retourne la liste des points d'une POLYLIGNE dans le SCU courant
(defun UtLpPoly	(e / ne p lp)
  (while (= "VERTEX"
	    (cdr (assoc 0 (setq ne (entget (setq e (entnext e))))))
	 )
    (setq
      p	 (trans (cdr (assoc 10 ne)) e 1)
      lp (append lp (list p))
    )
  )
)

;; Retourne la liste des points d'une LWPOLYLIGNE dans le SCU courant
(defun UtLpLwPoly (e / ne nnn lp)
  (setq ne (entget e))
  (foreach nnn ne
    (if	(= 10 (car nnn))
      (setq lp (append lp (list (trans (cdr nnn) 0 1))))
    )
  )
  (UtReturn lp)
)

;;retourne le centre de la boîte mini englobant la polyligne ou LWpolyligne dont le nom d'entité est "e"
(defun UtPolyCentre (e / ty lp lx ly ne)

  (setq ty (cdr (assoc 0 (entget e))))
  (cond
    ((= "POLYLINE" ty) (setq lp (UtLpPoly e)))
    ((= "LWPOLYLINE" ty) (setq lp (UtLpLwPoly e)))
  )
  (setq lx (mapcar 'car lp))
  (setq ly (mapcar 'cadr lp))

  (list
    (/ (+ (apply 'min lx) (apply 'max lx)) 2)
    (/ (+ (apply 'min ly) (apply 'max ly)) 2)
  )
)

;;retourne le point Haut Gauche de la boîte mini englobant la polyligne ou LWpolyligne dont le nom d'entité est "e"
(defun UtPolyHautGauche	(e / ty lp lx ly ne)

  (setq ty (cdr (assoc 0 (entget e))))
  (cond
    ((= "POLYLINE" ty) (setq lp (UtLpPoly e)))
    ((= "LWPOLYLINE" ty) (setq lp (UtLpLwPoly e)))
  )
  (setq lx (mapcar 'car lp))
  (setq ly (mapcar 'cadr lp))

  (list
    (apply 'min lx)
    (apply 'max ly)
  )
)

;; Retourne la liste des points d'une Polyligne ou d'une LWpolyligne
(defun UtLpXpoly (e / ty lp)
  (setq ty (cdr (assoc 0 (entget e))))
  (cond
    ((= "POLYLINE" ty) (setq lp (UtLpPoly e)))
    ((= "LWPOLYLINE" ty) (setq lp (UtLpLwPoly e)))
  )
  (UtReturn Lp)
)

;;Retourne une liste de 2 objets correspondant au décalages Intérieur et Extérieur d'une polyligne fermée de Offset
(defun UtDecalInOut (e offset / Lp P1 P2 P3 a21 a23 a pA pB aA aB)
  (setq Lp (UtLpXpoly e))
  (setq	P1 (car Lp)
	P2 (cadr Lp)
	P3 (caddr Lp)
  )
  (setq	a21 (angle P2 P1)
	a23 (angle p2 P3)
  )
  (setq a (/ (+ a23 a21) 2))
  (setq	pA (polar p2 a offset)
	pB (polar p2 (+ a pi) offset)
  )
  (command "_OFFSET" offset e "_NONE" pA "")
  (setq eA (entlast))
  (command "_OFFSET" offset e "_NONE" pB "")
  (setq eB (entlast))
  (command "_AREA" "_OB" eA)
  (setq aA (getvar "AREA"))
  (command "_AREA" "_OB" eB)
  (setq aB (getvar "AREA"))
  (if (> aA aB)
    (list eB eA)
    (list eA eB)
  )
)
;;Retourne le décalage Intérieur
(defun UtDecalIn (e Offset / x)
  (setq x (UtDecalInOut e offset))
  (entdel (cadr x))
  (Utreturn (car x))
)

;;Retourne le décalage Exterieur
(defun UtDecalOut (e Offset / x)
  (setq x (UtDecalInOut e offset))
  (entdel (car x))
  (Utreturn (cadr x))
)



;;; Commandes pour faciliter la saisie de nombres, de distance ou d'angle
(defun Utgetint	(msg Valdef mini maxi / ok x)
  (while (not ok)
    (setq x (getint (strcat "\n" msg " <" (itoa valdef) "> : ")))
    (if	(not x)
      (setq x valdef)
    )
    (if	(and (>= x mini) (<= x maxi))
      (setq ok T)
      (alert "Valeur incorrecte !")
    )
  )
  (Utreturn x)
)

(defun Utgetdist (Ori msg Valdef mini maxi Init / ok x)
  (while (not ok)
    (initget 128 Init)
    (setq x (getpoint Ori (strcat "\n" msg " <" (rtos valdef) "> : ")))
    (print x)
    (print init)
    (cond
      ((not x) (setq ok T) (setq x valdef))
      (
       (and (= (type x) 'STR) (or (= x "2P") (= x "3P")))
       (setq ok T)
      )
      ((= (type x) 'STR)
       (setq x (atof x))
       (if (and (>= x mini) (<= x maxi))
	 (setq ok T)
	 (alert "Valeur incorrecte !")
       )
      )
      ((numberp x)
       (if (and (>= x mini) (<= x maxi))
	 (setq ok T)
	 (alert "Valeur incorrecte !")
       )
      )
      (t
       (setq x (distance (list (car Ori) (cadr Ori))
			 (list (car x) (cadr x))
	       )
       )
       (if (and (>= x mini) (<= x maxi))
	 (setq ok T)
	 (alert "Valeur incorrecte !")
       )
      )
    )
  )
  (Utreturn x)
)

(defun Utgetorient (Ori msg Valdef / ok x)
  (setq	x (getorient Ori
		     (strcat "\n" msg " <" (angtos valdef) "> : ")
	  )
  )
  (if (not x)
    (setq x valdef)
  )
  (Utreturn x)
)


;;; Commandes de zoom...

(defun c:- () (command "_ZOOM" "8/10x"))
(defun c:-- () (command "_ZOOM" "64/100x"))
(defun c:+ () (command "_ZOOM" "10/8x"))
(defun c:++ () (command "_ZOOM" "100/64x"))


;;; Accès à un fichier Excel

(defun UtReadXlsTable (Env Xls Table Clé / EnvDsc SqlStm ObjDsc	Cols
		       CsrDsc x	Lx ok)
  (print (setq EnvDsc (asi_connect Env)))
  (print
    (setq SqlStm (strcat "SELECT * FROM " "\"" Xls "\"." Table))
  )
  (print (setq ObjDsc (asi_prepare EnvDsc SqlStm)))
  (print (setq Cols (asi_coldsc ObjDsc)))
  (setq Cols (mapcar '(lambda (z) (cdr (assoc 1 z))) Cols))
  (setq CsrDsc (asi_alloc ObjDsc "CUR"))
  (asi_open CsrDsc)
  (while (not ok)
    (setq x (asi_fetch CsrDsc))
    (if	x
      (progn
	(setq x (mapcar 'cons Cols x))
	(if (cdr (assoc Clé x))
	  (progn
	    (setq x
		   (cons
		     (strcase (cdr (assoc Clé x)))
		     x
		   )
	    )
	    (setq Lx (cons x Lx))
	  )
	  (setq ok T)
	)
      )
      (setq ok T)
    )
  )
  (asi_close CsrDsc)
  (asi_disconnect EnvDsc)
  (Reverse Lx)
)

;;; Transformation d'une LWPOLYLINE en POLYLINE

(defun UtLw2Pl (Lw / nLw Const1 Const2 z Option x)
  (setq nLw (entget Lw))
  (if (= "LWPOLYLINE" (cdr (assoc 0 nLw)))
    (progn

      (foreach nnn '(6 8 62 67 48 60)
	(if (assoc nnn nLw)
	  (setq Const1 (append Const1 (list (assoc nnn nLw))))
	)
      )
      (foreach nnn '(39 210)
	(if (assoc nnn nLw)
	  (setq Const2 (append Const2 (list (assoc nnn nLw))))
	)
      )
      (if (setq z (assoc 38 nLw))
	(setq z (cdr z))
	(setq z 0)
      )
      (entmake
	(append
	  '((0 . "POLYLINE"))
	  Const1
	  Const2
	  (list (assoc 70 nLw))
	  (list (list 10 0 0 z))
	)
      )
      (setq nLw (member (assoc 10 nLw) nLw))
      (while (assoc 10 nLw)
	(foreach nnn '(10 40 41 42)
	  (if (assoc nnn nLw)
	    (setq Option (append Option (list (assoc nnn nLw))))
	  )
	)
	(entmake
	  (append
	    '((0 . "VERTEX"))
	    Const1
	    Option
	  )
	)
	(setq nLw (member (assoc 10 (cdr nLw)) nLw))
      )
      (if (entmake '((0 . "SEQEND")))
	(setq x (entlast))
      )
      (if x
	(entdel Lw)
      )
      (UtReturn x)
    )
  )
)

(defun UtTreeBlk
       (InsObj / Z Ok nnn Der nDer NatObj Bname Y SousObj mmm)
  (setq Z (list (list InsObj)))
  (while (not Ok)
    (setq Ok T)
    (foreach nnn Z
      (setq Der (last nnn))
      (setq nDer (entget Der))
      (setq NatObj (cdr (assoc 0 nDer)))
      (setq Bname (cdr (assoc 2 nDer)))
      (if
	(/= NatObj "INSERT")
	 (setq Y (cons nnn Y))
	 (progn
	   (setq Ok nil)
	   (setq SousObj (UtTreeBlkSousObj Bname))
	   (foreach mmm	SousObj
	     (setq Y (cons (append nnn (list mmm)) Y))
	   )
	 )
      )
    )
    (setq Z Y
	  Y nil
    )
  )
  (UtReturn Z)
)

(defun UtTreeBlkSousObj	(Nom / Block x Ret)
  (setq Block (tblsearch "BLOCK" Nom))
  (setq x (cdr (assoc -2 Block)))
  (while x
    (setq Ret (cons x Ret))
    (setq x (entnext x))
  )
  (UtReturn Ret)
)


;;; Utilitaires de base de données DBF

;; Mise en liste de la table
(defun UtDbf2List (Table / DbfId Fields Data Lst)
  (if (wcmatch (strcase Table) "*.DBF")
    (setq Table (substr Table 1 (- (strlen Table) 4)))
  )
  (if (findfile (strcat table ".DBF"))
    (progn
      (setq DbfId (DbfOpen Table))
      (setq Fields (mapcar 'car (DbfFields DbfId)))
      (setq Data (DbfFindAll DbfId (car Fields) ""))
      (setq Lst (mapcar '(lambda (z) (mapcar 'list Fields z)) Data))
      (DbfClose DbfId)
    )
  )
  (UtReturn Lst)
)

;;; Utilitaires divers

;;; Liste des calques utilisés par un bloc (imbrications comprises)

(defun UtLayersBlk (Objet / x nObjet BlkName)
  (setq nObjet (entget Objet))
  (if (= "INSERT" (cdr (assoc 0 nObjet)))
    (progn
      (setq BlkName (cdr (assoc 2 nObjet)))
      (setq x (list (cons "BLOCK" BlkName)))
      (while (assoc "BLOCK" x)
	(setq x (mapcar 'UtLayersBlk1 x))
	(setq x (apply 'append x))
	(setq x (UtSupDoublonsList x))
      )
      (mapcar 'cdr x)
    )
  )
)
(defun UtLayersBlk1 (Element / L Obj nObj Ty Ly Na)
  (cond
    ((= "LAYER" (car Element)) (setq L (list Element)))
    ((= "BLOCK" (car Element))
     (setq Obj (tblsearch "BLOCK" (cdr Element)))
     (setq Obj (cdr (assoc -2 Obj)))
     (while Obj
       (setq nObj (entget Obj))
       (setq Ty (cdr (assoc 0 nObj)))
       (setq Ly (cdr (assoc 8 nObj)))
       (setq Ly (cons "LAYER" Ly))
       (if (not (member Ly L))
	 (setq L (cons Ly L))
       )
       (if (= Ty "INSERT")
	 (progn
	   (setq Na (cons "BLOCK" (cdr (assoc 2 nObj))))
	   (if (not (member Na L))
	     (setq L (cons Na L))
	   )
	 )
       )
       (setq Obj (entnext Obj))
     )
    )
  )
  (UtReturn L)
)

;;; Lit CDATE & formate la valeur lue en chaîne de caractères

(defun UtDate(/ Ti ToDay)
  (setq Ti (substr(rtos(getvar "CDATE")) 1 8))
  (setq ToDay (strcat (substr Ti 7 2) "/" (substr Ti 5 2) "/" (substr Ti 1 4)))
  (UtReturn ToDay)
)

;;; Chargement propre

(princ)



 ;|«Visual LISP© Format Options» (72 2 40 2 nil "end of " 60 9 0 0 0 T T nil T)
 ***Don't add text below the comment!***|;

