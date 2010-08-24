;;; -*- encoding: utf-8 -*-
;;;
;;; io.lisp
;;; Input/Output
;;; gamallo, February 11, 2007
;;;

;; Copyright 2007, 2008, 2009 Manuel Felipe Gamallo Rivero.

;;     This file is part of Miguedrez.

;;     Miguedrez is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.

;;     Miguedrez  is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with Miguedrez.  If not, see <http://www.gnu.org/licenses/>.

;;; Hash table containing the descriptions of the pieces
(in-package :miguedrez)
(defvar *pieces* (make-hash-table))
(setf (gethash 'TN *pieces*) "BR")
(setf (gethash 'TB *pieces*) "WR")
(setf (gethash 'CN *pieces*) "BN")
(setf (gethash 'CB *pieces*) "WN")
(setf (gethash 'AN *pieces*) "BB")
(setf (gethash 'AB *pieces*) "WB")
(setf (gethash 'RN *pieces*) "BK")
(setf (gethash 'RB *pieces*) "WK")
(setf (gethash 'DN *pieces*) "BQ")
(setf (gethash 'DB *pieces*) "WQ")
(setf (gethash 'PN *pieces*) "BP")
(setf (gethash 'PB *pieces*) "WP")
(setf (gethash 'VV *pieces*) "..")


;;; Show board to the user
(defun show-board (board)
  (format t "~%     A    B    C    D    E    F    G    H~%~%")
  (dotimes (row 8)
    (format t " ~A  " (- 8 row))
    (dotimes (column 8)
      (format t "~A   " (gethash (aref (board-board board) row column)
                 *pieces*)))
    (format t "~%~%")))


;;; Ask the user for a move
(defun read-move ()
  (let (row-from column-from row-to column-to)
    (loop
       (when (and (not (null row-from))
          (not (null column-from))
          (not (null row-to))
          (not (null column-to)))
     (return))
       (format t "Movimiento: ")
       (let ((mov1 (read))
         (mov2 (read)))
     (ignore-errors
       (when (= 2 (length (string mov1)))
         (setf row-from (digit-char-p (aref (string mov1) 1)))
         (setf column-from (position (aref (string mov1) 0) "ABCDEFGH"
                       :test (function string-equal))))
       (when (= 2 (length (string mov2)))
         (setf row-to (digit-char-p (aref (string mov2) 1)))
         (setf column-to (position (aref (string mov2) 0) "ABCDEFGH"
                       :test (function string-equal)))))))
;;    (format t "~A ~A ~A ~A" row-from column-from row-to column-to)
    (make-move :from (make-pos :row (- 8 row-from)
                       :col column-from)
             :to (make-pos :row (- 8 row-to)
                       :col column-to))))
