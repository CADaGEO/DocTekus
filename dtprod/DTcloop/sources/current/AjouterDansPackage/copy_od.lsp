;;;---------------------------------------------------------------------------;
;;;
;;;    COPY_OD.LSP
;;;
;;;    (C) Copyright 1998 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;    July 1996
;;; 
;;;---------------------------------------------------------------------------;
;;;
;;;    DESCRIPTION
;;;
;;;    Copy object data from one object to a set of other objects. If the
;;;    object data to be copied already exists on the target object the
;;;    options to not copy, copy once and copy all are presented.
;;;
;;;    Careful, it is possible to corrupt existing topological data
;;;    using this routine.
;;;
;;;---------------------------------------------------------------------------;

;;;****************************************************************************
;;; Function: C:COPY_OD
;;;
;;; Main routine for copying object data from an object to
;;; a selection set of object. 
;;;
;;;
(defun C:COPY_OD ( 
  /
  source_obj                 ; source object
  target_obj                 ; target object
  target_ss                  ; target selection set
  ct                         ; count
  len                        ; length
  overwrite                  ; overwrite flag
  error                      ; old error function
  )
  
  (setq error *error*)
  ;;
  ;; Define error handler
  ;;
  (defun *error* (msg)
    (alert msg)
    (setq *error* error)
    (exit)
  )
  
  ;;
  ;; Input the source object to copy data from
  ;;
  (princ "\nSelect SOURCE object: ")
  (setq source_obj (car (entsel)))
  (if (null source_obj)
    (prompt "\nNo source object selected.")
    (progn
      ;;
      ;; If the object has object data attached process it
      ;;
      (if (null (ade_odgettables source_obj))
        (princ "\nSelected object contains no object data.")
        (progn
          (princ "\n\nSelect TARGET objects: ")
          (setq target_ss (ssget))
          (if (null target_ss)
            (prompt "\nNo target object selected.")
            (progn
              (setq len (sslength target_ss))
              (setq ct 0)
              (princ "\nCopying object data...")
              (while (< ct len)
                (setq target_obj (ssname target_ss ct))
                (redraw target_obj 3)
                (setq ct (+ ct 1))
                (setq overwrite (COPY_DATA source_obj target_obj overwrite))
                (redraw target_obj 4)
              )
            )
          );if
        )
      );if
    )
  );if   
  
  (setq *error* error)                                ;restore error handler
  
  (prompt "\nProcessing completed.")
  (princ)
  
);C:COPY_OD

;;;****************************************************************************
;;; Function: C:COPY_DATA
;;;
;;; Copy object data from the source object to the target object.
;;;
;;; If the data is already found to exist on a target object, the 
;;; user is prompted what to do. Either to replace it only on the 
;;; target, for all objects in  the selection set, or to skip it.
;;;
;;;
(defun COPY_DATA (
  source_obj
  target_obj
  overwrite                   ; overwrite flag
  / 
  ct        ct2 
  cttemp    fld 
  fldnme    fldnamelist 
  fldtyp    fldtypelist
  len       numrec 
  OK        tbl 
  tbllist   tbldef 
  tblstr    val 
  vallist 
  )

  ;; 
  ;; access all OD tables from source object
  ;;
  (if (setq tbllist (ade_odgettables source_obj))
    (progn
      ;;
      ;; for each table on source object
      ;;
      (foreach tbl tbllist
         (prompt (strcat "\nProcessing source table " tbl "."))
         ;;
         ;; determine if target object has object
         ;; data records for current table
         ;;
         (setq OK nil)
         (setq numrec (ade_odrecordqty target_obj tbl))
         ;;
         ;; If the table is found on object ask what to do
         ;;
         (if (and (> numrec 0) (/= overwrite "All"))
           (progn
             (initget "All Yes No")
             (setq overwrite (getkword "\nOverwrite existing record(s) on target? (All/Yes/No) <All>: "))
             (if (null overwrite)
               (setq overwrite "All")
             )
           )
         )
         (if (or (= overwrite "All") 
                 (= overwrite "Yes")
                 (= numrec 0)
             )
            (setq OK T)
         )
         ;;
         ;; delete all existing records on target 
         ;; object if overwrite flag is set
         ;;
         (if (and (> numrec 0)
                  (or (= overwrite "Yes")(= overwrite "All"))
             )
           (progn
             (setq ct 0)
             (while (< ct numrec)
               (ade_oddelrecord target_obj tbl ct)
               (setq ct (+ ct 1))
             )
           ) 
         )
         (if OK
           (progn
            ;;
            ;; build list of field names
            ;;
            (setq tbldef (ade_odtabledefn tbl))
            (setq tblstr (cdr (nth 2 tbldef)))
            (setq fldnamelist ())
            (setq fldtypelist ())
            (foreach fld tblstr
              (setq fldnme (cdr (nth 0 fld)))
              (setq fldtyp (cdr (nth 2 fld)))
              (setq fldnamelist (append fldnamelist (list fldnme)))
              (setq fldtypelist (append fldtypelist (list fldtyp)))
            )
            ;;
            ;; for each record on source object 
            ;;
            (setq numrec (ade_odrecordqty source_obj tbl))
            (setq ct 0)
            (while (< ct numrec)
              ;;
              ;; build list of values
              ;;
              (setq cttemp 0)
              (setq vallist ())
              (foreach fld fldnamelist
                (setq typ (nth cttemp fldtypelist))
                (setq cttemp (+ cttemp 1))
                (setq val (ade_odgetfield source_obj tbl fld ct))
                (if (= typ "Integer")(setq val (fix val)))
                (setq vallist (append vallist (list val)))
              )
              ;;
              ;; add a record to target object
              ;;
              (ade_odaddrecord target_obj tbl)
              ;;
              ;; populate target record with values from source record
              ;;
              (setq ct2 0)
              (while (< ct2 (length vallist))
                (setq val (nth ct2 vallist))
                (setq fld (nth ct2 fldnamelist))
                (setq ct2 (+ ct2 1))
                (ade_odsetfield target_obj tbl fld ct val)
              )
              (setq ct (+ ct 1))
            );while
          )
        );if
      );foreach
    )
  );if
  
  ;;
  ;; Return overwrite status so it can 
  ;; be passed back in for the next object.
  ;; 
  overwrite
   
);COPY_DATA

(prompt "\nType: COPY_OD to copy object data.")
(princ)
