;;; -*- encoding: utf-8 -*-
;;;
;;; king.lisp
;;; Movements of kings
;;; gamallo, July 25, 2007
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

(defun maybe-pos-king (pos)
  (let ((row (pos-row pos))
	(col (pos-col pos))
	(maybe-positions '()))
    (when (> row 0) ; Twelve o'clock
      (add-pos maybe-positions (- row 1) col))
    (when (and (> row 0) (< col 7)) ; Half-past one
      (add-pos maybe-positions (- row 1) (1+ col)))
    (when (< col 7) ; Three o'clock
      (add-pos maybe-positions row (1+ col)))
    (when (and (< row 7) (< col 7)) ; Half-past four
      (add-pos maybe-positions (1+ row) (1+ col)))
    (when (< row 7) ; Six o'clock
      (add-pos maybe-positions (1+ row) col))
    (when (and (< row 7) (> col 0)) ; Half-past seven
      (add-pos maybe-positions (1+ row) (- col 1)))
    (when (> col 0) ; nine o'clock
      (add-pos maybe-positions row (- col 1)))
    (when (and (> row 0) (> col 0)) ; Half-past ten
      (add-pos maybe-positions (- row 1) (- col 1)))
    maybe-positions))





(defun empty-pos (board pos)
  (eq (aref (board-board board)
	    (pos-row pos)
	    (pos-col pos))
      'VV))

(defun occupied-pos (board pos funcion)
  (eq (aref (funcall funcion board)
	    (pos-row pos)
	    (pos-col pos))
      '1))



(defun possible-king-without-castling (board pos)
  (let ((opponent-function (if (eq (aref (board-board board)
					  (pos-row pos)
					  (pos-col pos))
				    'RB)
				#'board-blacks
			      #'board-whites))
	(color (if (eq (aref (board-board board)
			     (pos-row pos)
			     (pos-col pos))
		       'RB)
		   'white
		   'black))
	(maybe-positions
	 (maybe-pos-king pos))
	(positions '()))
    (dolist (p maybe-positions)
      (if (or (empty-pos board p)
 	      (occupied-pos board p opponent-function))
 	  (setf positions (cons p positions))))
    positions))


(defun possible-king (board pos)
  (let ((color (if (eq (aref (board-board board)
			     (pos-row pos)
			     (pos-col pos))
		       'RB)
		   'white
		   'black))
	(positions (possible-king-without-castling board pos)))
    (setf positions (append positions (short-castle-if-possible board positions color)))
    (setf positions (append positions (long-castle-if-possible board positions color)))
    positions))



(defun short-castle-if-possible (board a-list color)
  (cond ((and (eq color 'white)
 	      (eq (board-whites-initial-pos board) 'bottom))
 	 (when (and (not (board-white-king-moved board))
 		    (empty-pos board
 				    (make-pos :row 7 :col 5))
 		    (empty-pos board
 				    (make-pos :row 7 :col 6))
 		    (eq (board-pos (board-board board)
 					  (make-pos :row 7
 							 :col 7))
 			'TB)
 		    (not (king-threatened board color)))
 	   (add-pos a-list 7 6)))
	((and (eq color 'white)
	      (eq (board-whites-initial-pos board) 'top))
	 (when (and (not (board-white-king-moved board))
		    (empty-pos board
				    (make-pos :row 0 :col 2))
		    (empty-pos board
				    (make-pos :row 0 :col 1))
		    (eq (board-pos (board-board board)
					  (make-pos :row 0
							 :col 0))
			'TB)
		    (not (king-threatened board color)))
	   (add-pos a-list 0 1)))
	((and (eq color 'black)
	      (eq (board-whites-initial-pos board) 'bottom))
	 (when (and (not (board-black-king-moved board))
		    (empty-pos board
				    (make-pos :row 0 :col 5))
		    (empty-pos board
				    (make-pos :row 0 :col 6))
		    (eq (board-pos (board-board board)
					  (make-pos :row 0
							 :col 7))
			'TN)
		    (not (king-threatened board color)))
	   (add-pos a-list 0 6)))
	((and (eq color 'black)
	      (eq (board-whites-initial-pos board) 'top))
	 (when (and (not (board-black-king-moved board))
		    (empty-pos board
				    (make-pos :row 7 :col 2))
		    (empty-pos board
				    (make-pos :row 7 :col 1))
		    (eq (board-pos (board-board board)
					  (make-pos :row 7
							 :col 0))
			'TN)
		    (not (king-threatened board color)))
	   (add-pos a-list 7 1)))))


(defun long-castle-if-possible (board a-list color)
  (cond ((and (eq color 'white)
 	      (eq (board-whites-initial-pos board) 'bottom))
 	 (when (and (not (board-white-king-moved board))
 		    (empty-pos board
 				    (make-pos :row 7 :col 2))
 		    (empty-pos board
 				    (make-pos :row 7 :col 3))
 		    (eq (board-pos (board-board board)
 					  (make-pos :row 7
 							 :col 0))
 			'TB)
 		    (not (king-threatened board color)))
 	   (add-pos a-list 7 2)))
	((and (eq color 'white)
	      (eq (board-whites-initial-pos board) 'top))
	 (when (and (not (board-white-king-moved board))
		    (empty-pos board
				    (make-pos :row 0 :col 5))
		    (empty-pos board
				    (make-pos :row 0 :col 4))
		    (eq (board-pos (board-board board)
					  (make-pos :row 0
							 :col 7))
			'TB)
		    (not (king-threatened board color)))
	   (add-pos a-list 0 5)))
	((and (eq color 'black)
	      (eq (board-whites-initial-pos board) 'bottom))
	 (when (and (not (board-black-king-moved board))
		    (empty-pos board
				    (make-pos :row 0 :col 2))
		    (empty-pos board
				    (make-pos :row 0 :col 3))
		    (eq (board-pos (board-board board)
					  (make-pos :row 0
							 :col 0))
			'TN)
		    (not (king-threatened board color)))
	   (add-pos a-list 0 2)))
	((and (eq color 'black)
	      (eq (board-whites-initial-pos board) 'top))
	 (when (and (not (board-black-king-moved board))
		    (empty-pos board
				    (make-pos :row 7 :col 5))
		    (empty-pos board
				    (make-pos :row 7 :col 4))
		    (eq (board-pos (board-board board)
					  (make-pos :row 7
							 :col 7))
			'TN)
		    (not (king-threatened board color)))
	   (add-pos a-list 7 5)))))
