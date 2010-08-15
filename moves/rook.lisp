;;; -*- encoding: utf-8 -*-
;;;
;;; rook.lisp
;;; Movements of the rooks
;;; gamallo, April 1, 2007
;;;

;; Copyright 2007, 2008, 2009 Manuel Felipe Gamallo Rivero.

;;     This file is part of Foobar.

;;     Foobar is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.

;;     Foobar is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with Foobar.  If not, see <http://www.gnu.org/licenses/>.




(defun possible-white-rook (board pos)
  (let ((positions '()))
    ;; Twelve o'clock
   (dotimes (row (pos-row pos))
      (cond ((eq (aref (board-board board)
		       (- (pos-row pos) (1+ row))
		       (pos-col pos))
		 'VV)
	     (setf positions
		   (cons (make-pos
			  :row (- (pos-row pos) (1+ row))
			  :col (pos-col pos))
			 positions)))
	    ((eq (aref (board-blacks board)
		       (- (pos-row pos) (1+ row))
		       (pos-col pos))
		 '1)
	     (setf positions
		   (cons (make-pos
			  :row (- (pos-row pos) (1+ row))
			  :col (pos-col pos))
			 positions))
	     (return nil))
	    (t (return nil))))
    ;; Three o'clock
    (dotimes (col (- 7 (pos-col pos)))
      (cond ((eq (aref (board-board board)
		       (pos-row pos)
		       (+ (pos-col pos) col 1))
		 'VV)
	     (setf positions
		   (cons (make-pos
			  :row (pos-row pos)
			  :col (+ (pos-col pos) col 1))
			 positions)))
	    ((eq (aref (board-blacks board)
		       (pos-row pos)
		       (+ (pos-col pos) col 1))
		 '1)
	     (setf positions
		   (cons (make-pos
			  :row (pos-row pos)
			  :col (+ (pos-col pos) col 1))
			 positions))
	     (return nil))
	    (t (return nil))))
    ;; Six o'clock
    (dotimes (row (- 7 (pos-row pos)))
      (cond ((eq (aref (board-board board)
		       (+ (pos-row pos) row 1)
		       (pos-col pos))
		 'VV)
	     (setf positions
		   (cons (make-pos
			  :row (+ (pos-row pos) row 1)
			  :col (pos-col pos))
			 positions)))
	    ((eq (aref (board-blacks board)
		       (+ (pos-row pos) row 1)
		       (pos-col pos))
		 '1)
	     (setf positions
		   (cons (make-pos
			  :row (+ (pos-row pos) row 1)
			  :col (pos-col pos))
			 positions))
	     (return nil))
	    (t (return nil))))
    ;; Nine o'clock
    (dotimes (col (pos-col pos))
      (cond ((eq (aref (board-board board)
		       (pos-row pos)
		       (- (pos-col pos) (1+ col)))
		 'VV)
	     (setf positions
		   (cons (make-pos
			  :row (pos-row pos)
			  :col (- (pos-col pos)
				      (1+ col)))
			 positions)))
	    ((eq (aref (board-blacks board)
		       (pos-row pos)
		       (- (pos-col pos) (1+ col)))
		 '1)
	     (setf positions
		   (cons (make-pos
			  :row (pos-row pos)
			  :col (- (pos-col pos)
				      (1+ col)))
			 positions))
	     (return nil))
	    (t (return nil))))
    positions))


(defun possible-back-rook (board pos)
  (let ((positions '()))
    ;; Twelve o'clock
    (dotimes (row (pos-row pos))
      (cond ((eq (aref (board-board board)
		       (- (pos-row pos) (1+ row))
		       (pos-col pos))
		 'VV)
	     (setf positions
		   (cons (make-pos
			  :row (- (pos-row pos) (1+ row))
			  :col (pos-col pos))
			 positions)))
	    ((eq (aref (board-whites board)
		       (- (pos-row pos) (1+ row))
		       (pos-col pos))
		 '1)
	     (setf positions
		   (cons (make-pos
			  :row (- (pos-row pos) (1+ row))
			  :col (pos-col pos))
			 positions))
	     (return nil))
	    (t (return nil))))
    ;; Three o'clock
    (dotimes (col (- 7 (pos-col pos)))
      (cond ((eq (aref (board-board board)
		       (pos-row pos)
		       (+ (pos-col pos) col 1))
		 'VV)
	     (setf positions
		   (cons (make-pos
			  :row (pos-row pos)
			  :col (+ (pos-col pos) col 1))
			 positions)))
	    ((eq (aref (board-whites board)
		       (pos-row pos)
		       (+ (pos-col pos) col 1))
		 '1)
	     (setf positions
		   (cons (make-pos
			  :row (pos-row pos)
			  :col (+ (pos-col pos) col 1))
			 positions))
	     (return nil))
	    (t (return nil))))
    ;; Six o'clock
    (dotimes (row (- 7 (pos-row pos)))
      (cond ((eq (aref (board-board board)
		       (+ (pos-row pos) row 1)
		       (pos-col pos))
		 'VV)
	     (setf positions
		   (cons (make-pos
			  :row (+ (pos-row pos) row 1)
			  :col (pos-col pos))
			 positions)))
	    ((eq (aref (board-whites board)
		       (+ (pos-row pos) row 1)
		       (pos-col pos))
		 '1)
	     (setf positions
		   (cons (make-pos
			  :row (+ (pos-row pos) row 1)
			  :col (pos-col pos))
			 positions))
	     (return nil))
	    (t (return nil))))
    ;; Nine o'clock
    (dotimes (col (pos-col pos))
      (cond ((eq (aref (board-board board)
		       (pos-row pos)
		       (- (pos-col pos) (1+ col)))
		 'VV)
	     (setf positions
		   (cons (make-pos
			  :row (pos-row pos)
			  :col (- (pos-col pos)
				      (1+ col)))
			 positions)))
	    ((eq (aref (board-whites board)
		       (pos-row pos)
		       (- (pos-col pos) (1+ col)))
		 '1)
	     (setf positions
		   (cons (make-pos
			  :row (pos-row pos)
			  :col (- (pos-col pos)
				      (1+ col)))
			 positions))
	     (return nil))
	    (t (return nil))))
    positions))





(defun possible-rook (board pos)
  (if (eq (aref (board-board board)
		(pos-row pos)
		(pos-col pos))
	  'TB)
      ;; The rook is white
      (possible-white-rook board pos)
    ;; The rook is black
    (possible-back-rook board pos)))
