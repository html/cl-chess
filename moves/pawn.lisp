;;; -*- encoding: utf-8 -*-
;;;
;;; pawn.lisp
;;; Pawn movements
;;; gamallo, March 22, 2007
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


(in-package :miguedrez)

;;; Places to which a pawn can move whitout atacking
(defun free-pos-pawn (board pos)
  (let ((positions '()))
    ;; White pieces at the bottom
    (if (eq (board-whites-initial-pos board) 'bottom)
	;; The pawn is white
	(if (eq (board-pos (board-board board) pos) 'PB)
	    (progn
	      ;; We are NOT at the upper border
	      (when (not (= (pos-row pos) 0))
		(add-pos positions
		    (1- (pos-row pos))
		    (pos-col pos)))
	      ;; We are in the initial pos: we can advance two
	      ;; cells if way is clear
	      (when (and (= (pos-row pos) 6)
			 (eq (aref (board-board board)
				   5
				   (pos-col pos))
			     'VV))
		(add-pos positions 4 (pos-col pos))))
	    ;; The pawn is black
	    (progn
	      ;; We are NOT at the lower border
	      (when (not (= (pos-row pos) 7))
		(add-pos positions
		    (1+ (pos-row pos))
		    (pos-col pos)))
	      ;; We are int the initial pos: we can advance two
	      ;; cells if the way is clear
	      (when (and (= (pos-row pos) 1)
			 (eq (aref (board-board board)
				   2
				   (pos-col pos))
			     'VV))
		(add-pos positions 3 (pos-col pos)))))
	;; White pieces at the top
	;; Pawn is white
	(if (eq (board-pos (board-board board) pos) 'PB)
	    (progn
	      ;; We are not at the lower border
	      (when (not (= (pos-row pos) 7))
		(add-pos positions
		    (1+ (pos-row pos))
		    (pos-col pos)))
	      ;; We are in the initial pos: we can advance two
	      ;; cells if way is clear
	      (when (and (= (pos-row pos) 1)
			 (eq (aref (board-board board)
				   2
				   (pos-col pos))
			     'VV))
		(add-pos positions 3 (pos-col pos))))
	    ;; Pawn is black
	    (progn
	      ;; We are not at the upper border
	      (when (not (= (pos-row pos) 0))
		(add-pos positions
		    (1- (pos-row pos))
		    (pos-col pos)))
	      ;; We are in the initial pos: we can advance two
	      ;; cells if way is clear
	      (when (and (= (pos-row pos) 6)
			 (eq (aref (board-board board)
				   5
				   (pos-col pos))
			     'VV))
		(add-pos positions 4 (pos-col pos))))))
    positions))




;;; Places an ataccking pawn can move to
(defun attack-pos-pawn (board pos)
  (let ((positions '()))
    ;; White pieces at the bottom
    (if (eq (board-whites-initial-pos board) 'bottom)
	;; Pawn is white
	(if (eq (board-pos (board-board board) pos) 'PB)
	    ;; We are not at the upper border
	    (when (not (= (pos-row pos) 0))
	      ;; We are not at the left border
	      (when (not (= (pos-col pos) 0))
		(add-pos positions
		    (1- (pos-row pos))
		    (1- (pos-col pos))))
	      ;; We are not at the right border
	      (when (not (= (pos-col pos) 7))
		(add-pos positions
		    (1- (pos-row pos))
		    (1+ (pos-col pos)))))
	    ;; The pawn is black
	    ;; We are not at the lower border
	    (when (not (= (pos-row pos) 7))
	      ;; We are not at the left border
	      (when (not (= (pos-col pos) 0))
		(add-pos positions
		    (1+ (pos-row pos))
		    (1- (pos-col pos))))
	      ;; We are not at the right border
	      (when (not (= (pos-col pos) 7))
		(add-pos positions
		    (1+ (pos-row pos))
		    (1+ (pos-col pos))))))
	;; White pieces at the top
	;; Pawn is white
	(if (eq (board-pos (board-board board) pos) 'PB)
	    ;; We are not at the lower border
	    (when (not (= (pos-row pos) 7))
	      ;; We are not at the left border
	      (when (not (= (pos-col pos) 0))
		(add-pos positions
		    (1+ (pos-row pos))
		    (1- (pos-col pos))))
	      ;; We are not at the right border
	      (when (not (= (pos-col pos) 7))
		(add-pos positions
		    (1+ (pos-row pos))
		    (1+ (pos-col pos)))))
	    ;; Pawn is black
	    ;; We are not at the upper border
	    (when (not (= (pos-row pos) 0))
	      ;; We are not at the left border
	      (when (not (= (pos-col pos) 0))
		(add-pos positions
		    (1- (pos-row pos))
		    (1- (pos-col pos))))
	      ;; We are not at the right border
	      (when (not (= (pos-col pos) 7))
		(add-pos positions
		    (1- (pos-row pos))
		    (1+ (pos-col pos)))))))
    positions))




;;; Places to which a pawn can move.
(defun possible-free-pawn (board pos positions)
  (dolist (a-pos (free-pos-pawn board pos))
    (when (eq (board-pos (board-board board) a-pos) 'VV)
      (setf positions (cons a-pos positions))))
  positions)




;;; Places to which an attacking pawn can move.
;;; NOTE: We take into account enpases
(defun possible-attack-pawn (board pos positions)
  (if (eq (board-pos (board-board board) pos) 'PB)
      ;; Pawn is white
      (dolist (a-pos (attack-pos-pawn board pos))
	(cond ((color-occupied-p (board-blacks board)
				(pos-row a-pos)
				(pos-col a-pos))
	       ;; Dest pos is occupied by a black piece
	       (setf positions (cons a-pos positions)))
	      ((and (eq (pos-row a-pos) 2)
		    (eq (board-whites-initial-pos board) 'bottom)
		    (eq (aref (board-blacks-enpass board)
			      (pos-col a-pos))
			'1))
	       ;; White pieces are at the bottom, the pawn moves to the row
	       ;; of enpass captures and, yes, there is a pawn that can be
	       ;; enpass-captured
	       (setf positions (cons a-pos positions)))
	      ((and (eq (pos-row a-pos) 5)
		    (eq (board-whites-initial-pos board) 'top)
		    (eq (aref (board-blacks-enpass board)
			      (pos-col a-pos))
			'1))
	       ;; White pieces are at the top, the pawn moves to the row
	       ;; of enpass captures and, yes, there is a pawn that can be
	       ;; enpass-captured
	       (setf positions (cons a-pos positions)))))
      
      ;; Pawn is black
      (dolist (a-pos (attack-pos-pawn board pos))
	(cond ((color-occupied-p (board-whites board)
				(pos-row a-pos)
				(pos-col a-pos))
	       ;; Dest pos is occupied by a white piece
	       (setf positions (cons a-pos positions)))
	      ((and (eq (pos-row a-pos) 5)
		    (eq (board-whites-initial-pos board) 'bottom)
		    (eq (aref (board-whites-enpass board)
			      (pos-col a-pos))
			'1))
	       ;; White pieces are at the bottom, the pawn moves to the row
	       ;; of enpass captures and, yes, there is a pawn that can be
	       ;; enpass-captured
	       (setf positions (cons a-pos positions)))
	      ((and (eq (pos-row a-pos) 2)
		    (eq (board-whites-initial-pos board) 'top)
		    (eq (aref (board-whites-enpass board)
			      (pos-col a-pos))
			'1))
	       ;; White pieces are at the top, the pawn moves to the row
	       ;; of enpass captures and, yes, there is a pawn that can be
	       ;; enpass-captured
	       (setf positions (cons a-pos positions))))))
  positions)





(defun possible-pawn (board pos)
  (incf *calls*)
  (let ((positions '()))
    (setf positions (possible-attack-pawn board pos positions))
    (setf positions (possible-free-pawn board pos positions))
    positions))
