;;; -*- encoding: utf-8 -*-
;;;
;;; data.lisp
;;; Data definitions and manipulation functions
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

;;; Main data type
(in-package :miguedrez)
(defstruct board
  (board)
  (whites)
  (blacks)
  (whites-enpass)
  (blacks-enpass)
  (whites-initial-pos)
  (white-king-pos)
  (black-king-pos)
  (white-king-moved)
  (black-king-moved))


;;; Player
(defstruct player
  (type) ;; 'auto, 'manual
  (color)) ;; 'white, 'black


(defun copy-board (board)
  (make-board :board (copy-board-array (board-board board))
		:whites (copy-board-array (board-whites board))
		:blacks (copy-board-array (board-blacks board))
		:whites-enpass (copy-board-row
				 (board-whites-enpass board))
		:blacks-enpass (copy-board-row
				(board-blacks-enpass board))
		:whites-initial-pos
		(board-whites-initial-pos board)
		:white-king-pos
		(copy-pos (board-white-king-pos board))
		:black-king-pos
		(copy-pos (board-black-king-pos board))
		:white-king-moved (board-white-king-moved board)
		:black-king-moved (board-black-king-moved board)))

(defun copy-board-array (board-array)
  (let ((new-board-array (make-array '(8 8))))
    (dotimes (row 8)
      (dotimes (col 8)
	(setf (aref new-board-array row col)
	      (aref board-array row col))))
    new-board-array))

(defun copy-board-row (row-array)
  (let ((new-row-array (make-array 8)))
    (dotimes (i 8 new-row-array)
      (setf (aref new-row-array i)
	    (aref row-array i)))))


(defun invert-color (color)
  (if (eq color 'white) 'black 'white))

(defun whitep (color)
  (eq color 'white))

;;; Is a given position occupied by a color?
(defun color-occupied-p (board-color row col)
  (eq (aref board-color row col) '1))

;;; Position
(defstruct pos
  (row)
  (col))

(defun equal-pos (pos1 pos2)
  (and (eq (pos-row pos1) (pos-row pos2))
       (eq (pos-col pos1) (pos-col pos2))))


;;; Creates a new pos given row and column and adds to the list
(defmacro add-pos (pos-list row col)
  `(setf ,pos-list (cons (make-pos :row ,row :col ,col) ,pos-list)))


;;; A movement
(defstruct move
  (from)
  (to))

(defun equal-moves (mov1 mov2)
  (and (equal-pos (move-from mov1) (move-from mov2))
       (equal-pos (move-to mov1) (move-to mov2))))


;;; A very used construction: a cell from a 8x8 vector
(defmacro board-pos (board pos)
  `(aref ,board (pos-row ,pos) (pos-col ,pos)))



;;; Creates an initial board with the white pieces at the bottom
(defun create-initial-board-white-bottom ()
  (make-board :board (make-array '(8 8)
				     :initial-contents
				     '((TN CN AN DN RN AN CN TN)
				       (PN PN PN PN PN PN PN PN)
				       (VV VV VV VV VV VV VV VV)
				       (VV VV VV VV VV VV VV VV)
				       (VV VV VV VV VV VV VV VV)
				       (VV VV VV VV VV VV VV VV)
				       (PB PB PB PB PB PB PB PB)
				       (TB CB AB DB RB AB CB TB)))
		:whites (make-array '(8 8)
				     :initial-contents
				     '((0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (1 1 1 1 1 1 1 1)
				       (1 1 1 1 1 1 1 1)))
		:blacks (make-array '(8 8)
				     :initial-contents
				     '((1 1 1 1 1 1 1 1)
				       (1 1 1 1 1 1 1 1)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)))
		:whites-enpass (make-array
				 8
				 :initial-contents '(0 0 0 0 0 0 0 0))
		:blacks-enpass (make-array
				 8
				 :initial-contents '(0 0 0 0 0 0 0 0))
		:whites-initial-pos 'bottom
		:white-king-pos (make-pos :row 7 :col 4)
		:black-king-pos (make-pos :row 0 :col 4)
		:white-king-moved nil
		:black-king-moved nil))


;;; Creates an initial board with the white pieces at the top
(defun create-initial-board-white-top ()
  (make-board :board (make-array '(8 8)
				    :initial-contents
				    '((TB CB AB RB DB AB CB TB)
				      (PB PB PB PB PB PB PB PB)
				      (VV VV VV VV VV VV VV VV)
				      (VV VV VV VV VV VV VV VV)
				      (VV VV VV VV VV VV VV VV)
				      (VV VV VV VV VV VV VV VV)
				      (PN PN PN PN PN PN PN PN)
				      (TN CN AN RN DN AN CN TN)))
		:whites (make-array '(8 8)
				     :initial-contents
				     '((1 1 1 1 1 1 1 1)
				       (1 1 1 1 1 1 1 1)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)))
		:blacks (make-array '(8 8)
				     :initial-contents
				     '((0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (1 1 1 1 1 1 1 1)
				       (1 1 1 1 1 1 1 1)))
		:whites-enpass (make-array
				 8
				 :initial-contents '(0 0 0 0 0 0 0 0))
		:blacks-enpass (make-array
				 8
				 :initial-contents '(0 0 0 0 0 0 0 0))
		:whites-initial-pos 'top
		:white-king-pos (make-pos :row 0 :col 3)
		:black-king-pos (make-pos :row 7 :col 3)
		:white-king-moved nil
		:black-king-moved nil))


;;; Creates an initial test board
(defun create-initial-board-tests ()
  (make-board :board (make-array '(8 8)
				     :initial-contents
				     '((VV VV VV VV VV VV RN VV)
				       (VV VV DB VV VV VV VV VV)
				       (VV VV VV VV RB VV VV VV)
				       (VV VV VV VV VV VV VV PN)
				       (VV VV VV VV VV VV VV PB)
				       (VV VV VV VV VV VV VV VV)
				       (VV VV VV VV VV PB VV VV)
				       (VV VV VV VV VV VV VV VV)))
		:whites (make-array '(8 8)
				     :initial-contents
				     '((0 0 0 0 0 0 0 0)
				       (0 0 1 0 0 0 0 0)
				       (0 0 0 0 1 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 1)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 1 0 0)
				       (0 0 0 0 0 0 0 0)))
		:blacks (make-array '(8 8)
				     :initial-contents
				     '((0 0 0 0 0 0 1 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 1)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)
				       (0 0 0 0 0 0 0 0)))
		:whites-enpass (make-array
				 8
				 :initial-contents '(0 0 0 0 0 0 0 0))
		:blacks-enpass (make-array
				 8
				 :initial-contents '(0 0 0 0 0 0 0 0))
		:whites-initial-pos 'bottom
		:white-king-pos (make-pos :row 2 :col 4)
		:black-king-pos (make-pos :row 0 :col 6)
		:white-king-moved t
		:black-king-moved t))
