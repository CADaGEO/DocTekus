;;Copyright 2015, Jean-Christophe MICHELIN
;;
;;This file is part of DocTekus.
;;
;;    DocTekus is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    DocTekus is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with Foobar.  If not, see <http://www.gnu.org/licenses/>
;;
;;
;; ci dessous permet decharger une ortho uiml = Unload IMage
(defun c:uim ()
(command "_.-image" "_unload" (vla-get-name (vlax-ename->vla-object (car (entsel)))))
  )

;; ci dessous permet de decharger toutes les orthos uimt = Unload Image Tout
(defun c:uimt ()
(command "_.-image" "_unload" "*")
  )

;; ci dessous permet charger une ortho rim = Reload IMage
(defun c:rim ()
(command "_.-image" "_reload" (vla-get-name (vlax-ename->vla-object (car (entsel)))))
  )