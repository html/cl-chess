;;; -*- encoding: utf-8 -*-
;;;
;;; moves.lisp
;;; Movements of the pieces
;;; gamallo, March 8, 2007
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

;;; This function executes a move on a board
;;; It DOES NOT CHECK that the dest pos is void, that is job for
;;; check-move.
(defun execute-move (board move)
  (execute-move-enpass board move)
  (execute-move-board board move)
  (if (eq (board-pos (board-whites board)
			    (move-from move))
	  '1)
      (progn
	(execute-move-white board move)
	(when (equal-pos (board-white-king-pos board)
				  (move-from move))
	  (setf (board-white-king-pos board)
		(move-to move))
	  (when (not (board-white-king-moved board))
	    (setf (board-white-king-moved board) t)
	    ;; We check if it's a castling and, in that case,
	    ;; we move the corresponding rook
	    (if (eq (board-whites-initial-pos board) 'bottom)
		(cond ((equal-pos
			(move-to move)
			(make-pos :row 7 :col 6))
		       (execute-move
			board
			(make-move
			 :from (make-pos :row 7 :col 7)
			 :to (make-pos :row 7 :col 5))))
		      ((equal-pos
			(move-to move)
			(make-pos :row 7 :col 2))
		       (execute-move
			board
			(make-move
			 :from (make-pos :row 7 :col 0)
			 :to (make-pos :row 7 :col 3)))))
		(cond ((equal-pos
			(move-to move)
			(make-pos :row 0 :col 5))
		       (execute-move
			board
			(make-move
			 :from (make-pos :row 0 :col 7)
			 :to (make-pos :row 0 :col 4))))
		      ((equal-pos
			(move-to move)
			(make-pos :row 0 :col 1))
		       (execute-move
			board
			(make-move
			 :from (make-pos :row 0 :col 0)
			 :to (make-pos :row 0 :col 2))))))))
	  (queen-if-promotion board move))
      (progn
	(execute-move-black board move)
	(when (equal-pos (board-black-king-pos board)
				  (move-from move))
	  (setf (board-black-king-pos board)
		(move-to move))
	  (when (not (board-black-king-moved board))
	    (setf (board-black-king-moved board) t)
	    ;; We check if it's a castling and, in that case,
	    ;; move the corresponding rook
	    (if (eq (board-whites-initial-pos board) 'top)
		(cond ((equal-pos
			(move-to move)
			(make-pos :row 7 :col 5))
		       (execute-move
			board
			(make-move
			 :from (make-pos :row 7 :col 7)
			 :to (make-pos :row 7 :col 4))))
		      ((equal-pos
			(move-to move)
			(make-pos :row 7 :col 1))
		       (execute-move
			board
			(make-move
			 :from (make-pos :row 7 :col 0)
			 :to (make-pos :row 7 :col 2)))))
		(cond ((equal-pos
			(move-to move)
			(make-pos :row 0 :col 6))
		       (execute-move
			board
			(make-move
			 :from (make-pos :row 0 :col 7)
			 :to (make-pos :row 0 :col 5))))
		      ((equal-pos
			(move-to move)
			(make-pos :row 0 :col 2))
		       (execute-move
			board
			(make-move
			 :from (make-pos :row 0 :col 0)
			 :to (make-pos :row 0 :col 1))))))))
	(queen-if-promotion board move)))
      board)





;;; Function to move a piece on the main board
(defun execute-move-board (board move)
  (setf (board-pos (board-board board) (move-to move))
	(board-pos (board-board board) (move-from move)))
  (setf (board-pos (board-board board) (move-from move))
	'VV))





;;; Function to move a piece in the white pieces board
(defun execute-move-white (board move)
  (setf (board-pos (board-whites board) (move-to move)) '1)
  (setf (board-pos (board-whites board) (move-from move)) '0)
  (when (eq (board-pos (board-blacks board) (move-to move)) '1)
    (setf (board-pos (board-blacks board) (move-to move)) '0)))





;;; Function to move a piece in the black pieces board
(defun execute-move-black (board move)
  (setf (board-pos (board-blacks board)
			  (move-to move))
	'1)
  (setf (board-pos (board-blacks board)
			  (move-from move))
	'0)
  (when (eq (board-pos (board-whites board)
			      (move-to move))
	    '1)
    (setf (board-pos (board-whites board)
			    (move-to move))
	  '0)))




(defun queen-if-promotion (board move)
  (cond ((and (or (eq (pos-row (move-to move)) '7)
		  (eq (pos-row (move-to move)) '0))
	      (eq (board-pos (board-board board) (move-to move)) 'PB))
	 (setf (board-pos (board-board board) (move-to move)) 'DB))
	((and (or (eq (pos-row (move-to move)) '7)
		  (eq (pos-row (move-to move)) '0))
	      (eq (board-pos (board-board board) (move-to move)) 'PN))
	 (setf (board-pos (board-board board) (move-to move)) 'DN))))





;;; Function that takes care of the enpass arrays when moving
(defun execute-move-enpass (board move)
  (if (eq (board-whites-initial-pos board) 'bottom)
      ;; White pieces at the bottom

      (let ((a-board (board-board board))
	    (from (move-from move))
	    (to (move-to move)))
	(cond ((and (eq (board-pos a-board from) 'PN)
		    (eq (pos-row to) 5)
		    (eq (aref (board-whites-enpass board) (pos-col to)) '1))
	       ;; We are capturing a white pawn in the enpass position
	       (setf (aref a-board 4 (pos-col to)) 'VV)
	       (setf (aref (board-whites board) 4 (pos-col to)) '0))
	      ((and (eq (board-pos a-board from) 'PB)
		    (eq (pos-row to) 2)
		    (eq (aref (board-blacks-enpass board) (pos-col to))	'1))
	       ;; We are capturing a black pawn in the enpass position
	       (setf (aref a-board 3 (pos-col to)) 'VV)
	       (setf (aref (board-blacks board) 3 (pos-col to)) '0)))

	(setf (board-whites-enpass board)
	      (make-array 8 :initial-contents '(0 0 0 0 0 0 0 0)))
	(setf (board-blacks-enpass board)
	      (make-array 8 :initial-contents '(0 0 0 0 0 0 0 0)))

	(cond ((eq (board-pos a-board from) 'PB)
	       ;; The piece we are moving is a white pawn
	       (if (and (eq (pos-row from) '6)
			(eq (pos-row to) '4))
		   ;; The pawn is in its initial position and we want to
		   ;; advance two cells
		   (setf (aref (board-whites-enpass board) (pos-col to)) '1)))
	      
	      ((eq (board-pos a-board from) 'PN)
	       ;; The piece we are moving is a black pawn
	       (if (and (eq (pos-row from) '1)
			(eq (pos-row to) '3))
		   ;; The pawn is in its initial position and we want to
		   ;; advance two cells
		   (setf (aref (board-blacks-enpass board) (pos-col to)) '1)))))


      ;; White pieces at the top
      (let ((a-board (board-board board))
	    (from (move-from move))
	    (to (move-to move)))
	
	(cond ((and (eq (board-pos a-board from) 'PN)
		    (eq (pos-row to) 2)
		    (eq (aref (board-whites-enpass board) (pos-col to)) '1))
	       ;; We are capturing a white pawn in the enpass position
	       (setf (aref a-board 3 (pos-col to)) 'VV)
	       (setf (aref (board-whites board) 3 (pos-col to)) '0))
	      ((and (eq (board-pos a-board from) 'PB)
		    (eq (pos-row to) 5)
		    (eq (aref (board-blacks-enpass board) (pos-col to)) '1))
	       ;; We are capturing a black pawn in the enpass position
	       (setf (aref a-board 4 (pos-col to)) 'VV)
	       (setf (aref (board-blacks board) 4 (pos-col to)) '0)))

	(setf (board-whites-enpass board)
	      (make-array 8 :initial-contents '(0 0 0 0 0 0 0 0)))
	(setf (board-blacks-enpass board)
	      (make-array 8 :initial-contents '(0 0 0 0 0 0 0 0)))


	(cond ((eq (board-pos a-board from) 'PB)
	       ;; We are about to move a white pawn
	       (if (and (eq (pos-row from) '1)
			(eq (pos-row to) '3))
		   ;; The pawn is in its initial position, and we want to
		   ;; advance two cells
		   (setf (aref (board-whites-enpass board) (pos-col to)) '1)))
	    
	      ((eq (aref a-board (pos-row from) (pos-col from)) 'PN)
	       ;; We are about to move a black pawn
	       (if (and (eq (pos-row from) '6)
			(eq (pos-row to) '4))
		   ;; The pawn is in its initial position and we want to
		   ;; advance two cells
		   (setf (aref (board-blacks-enpass board) (pos-col to)) '1)))))))




(defun children-piece (board pos &key castle)
  (let ((children '())
	(piece (board-pos (board-board board) pos)))
    (cond ((or (eq piece 'PB) (eq piece 'PN))
	   (setf children (possible-pawn board pos)))
	  ((or (eq piece 'CB) (eq piece 'CN))
	   (setf children (possible-knight board pos)))
	  ((or (eq piece 'TB) (eq piece 'TN))
	   (setf children (possible-rook board pos)))
	  ((or (eq piece 'AB) (eq piece 'AN))
	   (setf children (possible-bishop board pos)))
	  ((or (eq piece 'DB) (eq piece 'DN))
	   (setf children (possible-queen board pos)))
	  ((or (eq piece 'RB) (eq piece 'RN))
	   (setf children
		 (if (eq castle 'without-castling)
		     (possible-king-without-castling board pos)
		     (possible-king board pos)))))
    (mapcar #'(lambda (to)
		(make-move :from pos :to to)) children)))



(defun children (board color &key castle)
  (let ((board-color (if (whitep color)
			   (board-whites board)
			   (board-blacks board)))
	(children '()))
    (dotimes (row 8 children)
      (dotimes (col 8)
	(when (color-occupied-p board-color row col)
	  (let ((current-pos (make-pos :row row
					   :col col)))
	    (setf children (nconc children
				   (children-piece
				    board
				    current-pos
				    :castle castle)))))))))




;;; Is this position threatened by this color?
(defun pos-threatened (a-board a-pos a-color)
  (member a-pos
	  (mapcar #'move-to
		  (children a-board
			     (invert-color a-color)
			     :castle 'without-castling))
	  :test #'equal-pos))

(defun valid (board move color)
  (let ((piece (if (eq color 'white) 'RB 'RN)))
    (and (member move (children board color)
		 :test #'equal-moves)
	 (king-valid board move color))))

(defun king-valid (board move color)
  (let ((new-board (copy-board board)))
    (execute-move new-board move)
    (null (king-threatened new-board color))))

(defun king-threatened (board color)
  (pos-threatened board
		      (if (eq color 'white)
			  (board-white-king-pos board)
			  (board-black-king-pos board))
		      color))

(defun checkmatep (a-board a-player)
  (and
   ;; The king is threatened
   (king-threatened a-board (player-color a-player))
   ;; For all of the children for the king, he stays threatened
   (let ((a-pos (if (eq (player-color a-player) 'white)
			  (board-white-king-pos a-board)
			  (board-black-king-pos a-board)))
	 (some-move-allowed))
     (dolist (a-move
	       (children a-board (player-color a-player) :castle 'without-castling)
	      (not some-move-allowed))
       ;; If some move is possible, we set some-move-allowed to T
       (let* ((new-board (execute-move (copy-board a-board)
					     a-move))
	      (new-king-pos (if (eq (player-color a-player) 'white)
				      (board-white-king-pos new-board)
				      (board-black-king-pos new-board))))
	 (when (not (pos-threatened new-board
					new-king-pos
					(player-color a-player)))
	   (setf some-move-allowed t)))))))
