;;; Bar_Data.lsp
;;;
;;; Liste de données des propriètès des objets créés sous Barbacar
;;; Note : pour personnaliser Barbacar, ajouter/modifier/supprimer les valeurs dans les
;;; listes ci-dessous.
;;; Ces listes sont utilisées dans la boite de dialogue Barbacar

;; définition liste des échelles

(setq $BARSCALE '(("25" . "1/25") ("50" . "1/50") ("100" . "1/100") ("200" . "1/200") ("500" . "1/500") ("1000" . "1/1000e") ("211" . "1/211e")))

;; définition liste de calques - (<pointeur> . "<nom de calque>")
;; <pointeur> : valeur entier numérique
;; <nom de calque> : chaine de caractères

(setq $BARLAYER '((1 . "0") (2 . "21_alt1-car1k") (3 . "22_alt1-car500") (4 . "23_alt1-car200")))

;; définition type de ligne des calques - (<pointeur> . "<type de ligne>")
;; <pointeur> : valeur entier numérique
;; <type de ligne> : chaine de caractères

(setq $BARLAYERTL '((1 . "continuous") (2 . "continuous") (3 . "continuous") (4 . "continuous")))

;; définition couleur des calques - (<pointeur> . <numéro de couleur>)
;; <pointeur> : entier numérique
;; <numéro de couleur> : entier numérique

(setq $BARLAYERCL '((1 . "7") (2 . "7") (3 . "7") (4 . "7")))

;; définition liste de bloc symbole "flèche NORD"

(setq $BARBLOCK '("NORD01"))

;; Sortie sans bruit

(princ)
