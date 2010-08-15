;;; -*- encoding: utf-8 -*-
;;;
;;; ai.lisp
;;; Artificial Intelligence
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


(in-package :miguedrez)
;; Pieces weight more or less depending on their proximity to the
;; center of the board.

(defun choose-move (board color)
  (second (multiple-value-list
	   (alpha-beta board 3 -1000000 1000000 color))))





(defun alpha-beta (board depth alpha beta color)
  (if (eq depth 0)
      (fev board)
      (let ((move)
	    (children (children board color)))
	(dolist (a-move children)
	  (when (king-valid board a-move color)
	    (when (null move) (setf move a-move))
	    (let* ((new-board (execute-move (copy-board board)
						      a-move))
		   (val (alpha-beta new-board (- depth 1) alpha beta (invert-color color))))
	      (if (eq color 'white)
		  (progn
		    (when (> val alpha)
		      (setf alpha val)
		      (setf move a-move))
		    (when (> alpha beta)
		      (values alpha move)))
		  (progn
		    (when (< val beta)
		      (setf beta val)
		      (setf move a-move))
		    (when (> alpha beta)
		      (values beta move)))))))
	(if (eq color 'white)
	    (values alpha move)
	    (values beta move)))))




(defun fev (board)
  (let ((fev 0))
    (dotimes (i 8)
      (dotimes (j 8)
	(let ((piece (aref (board-board board) i j)))
	  (cond ((eq piece 'PB)
		 (setf fev (+ fev
			      100
			      (if (eq
				   (board-whites-initial-pos board)
				   'bottom)
				  (aref +pawn-pos-v-weigh+ (- 7 i))
				  (aref +pawn-pos-v-weigh+ i))
			      (aref +pawn-pos-h-weigh+ j))))
		((eq piece 'PN)
		 (setf fev (- fev
			      100
			      (if (eq
				   (board-whites-initial-pos board)
				   'bottom)
				  (aref +pawn-pos-v-weigh+ i)
				  (aref +pawn-pos-v-weigh+ (- 7 i)))
			      (aref +pawn-pos-h-weigh+ j))))
		((eq piece 'TB)
		 (setf fev (+ fev
			      500
			      (aref +rook-pos-weigh+ i)
			      (aref +rook-pos-weigh+ j))))
		((eq piece 'TN)
		 (setf fev (- fev
			      500
			      (aref +rook-pos-weigh+ i)
			      (aref +rook-pos-weigh+ j))))
		((eq piece 'AB)
		 (setf fev (+ fev
			      300
			      (aref +knight-bishop-pos-weigh+ i)
			      (aref +knight-bishop-pos-weigh+ j))))
		((eq piece 'AN)
		 (setf fev (- fev
			      300
			      (aref +knight-bishop-pos-weigh+ i)
			      (aref +knight-bishop-pos-weigh+ j))))
		((eq piece 'CB)
		 (setf fev (+ fev
			      300
			      (aref +knight-bishop-pos-weigh+ i)
			      (aref +knight-bishop-pos-weigh+ j))))
		((eq piece 'CN)
		 (setf fev (- fev
			      300
			      (aref +knight-bishop-pos-weigh+ i)
			      (aref +knight-bishop-pos-weigh+ j))))
		((eq piece 'RB)
		 (setf fev (+ fev 1000)))
		((eq piece 'RN)
		 (setf fev (- fev 1000)))
		((eq piece 'DB)
		 (setf fev (+ fev 900)))
		((eq piece 'DN)
		 (setf fev (- fev 900)))))))
    fev))
