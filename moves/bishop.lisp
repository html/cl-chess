;;; -*- encoding: utf-8 -*-
;;;
;;; bishop.lisp
;;; Movements of the bishops
;;; gamallo, April 2, 2007
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



(defun possible-white-move (board pos)
  (eq (aref (board-board board)
	    (pos-row pos)
	    (pos-col pos)) 'VV))

(defun possible-white-capture (board pos)
  (eq (aref (board-blacks board)
	    (pos-row pos)
	    (pos-col pos)) '1))

(defun possible-black-move (board pos)
  (eq (aref (board-board board)
	    (pos-row pos)
	    (pos-col pos)) 'VV))

(defun possible-black-capture (board pos)
  (eq (aref (board-whites board)
	    (pos-row pos)
	    (pos-col pos)) '1))

(defun diagonal-move (pos f c)
  (make-pos :row (+ (pos-row pos) f)
		 :col (+ (pos-col pos) c)))







(defun possible-white-bishop (board pos)
  (let ((positions '()))
    ;; NE
    (dotimes (c (min (pos-row pos)
		     (- 7 (pos-col pos))))
      (let ((new-pos
	     (diagonal-move pos (- (1+ c)) (1+ c))))
	(cond ((possible-white-move board new-pos)
	       (setf positions (cons new-pos positions)))
	      ((possible-white-capture board new-pos)
	       (setf positions (cons new-pos positions))
	       (return nil))
	      (t (return nil)))))

    ;; SE
    (dotimes (c (min (- 7 (pos-row pos))
		     (- 7 (pos-col pos))))
      (let ((new-pos
	     (diagonal-move pos (1+ c) (1+ c))))
	(cond ((possible-white-move board new-pos)
	       (setf positions (cons new-pos positions)))
	      ((possible-white-capture board new-pos)
	       (setf positions (cons new-pos positions))
	       (return nil))
	      (t (return nil)))))

    ;; SW
    (dotimes (c (min (- 7 (pos-row pos))
		     (pos-col pos)))
      (let ((new-pos
	     (diagonal-move pos (1+ c) (- (1+ c)))))
	(cond ((possible-white-move board new-pos)
	       (setf positions (cons new-pos positions)))
	      ((possible-white-capture board new-pos)
	       (setf positions (cons new-pos positions))
	       (return nil))
	      (t (return nil)))))

    ;; NW
    (dotimes (c (min (pos-row pos)
		     (pos-col pos)))
      (let ((new-pos
	     (diagonal-move pos (- (1+ c)) (- (1+ c)))))
	(cond ((possible-white-move board new-pos)
	       (setf positions (cons new-pos positions)))
	      ((possible-white-capture board new-pos)
	       (setf positions (cons new-pos positions))
	       (return nil))
	      (t (return nil)))))
    positions))





(defun possible-black-bishop (board pos)
  (let ((positions '()))
    ;; NE
    (dotimes (c (min (pos-row pos)
		     (- 7 (pos-col pos))))
      (let ((new-pos
	     (diagonal-move pos (- (1+ c)) (1+ c))))
	(cond ((possible-black-move board new-pos)
	       (setf positions (cons new-pos positions)))
	      ((possible-black-capture board new-pos)
	       (setf positions (cons new-pos positions))
	       (return nil))
	      (t (return nil)))))

    ;; SE
    (dotimes (c (min (- 7 (pos-row pos))
		     (- 7 (pos-col pos))))
      (let ((new-pos
	     (diagonal-move pos (1+ c) (1+ c))))
	(cond ((possible-black-move board new-pos)
	       (setf positions (cons new-pos positions)))
	      ((possible-black-capture board new-pos)
	       (setf positions (cons new-pos positions))
	       (return nil))
	      (t (return nil)))))

    ;; SW
    (dotimes (c (min (- 7 (pos-row pos))
		     (pos-col pos)))
      (let ((new-pos
	     (diagonal-move pos (1+ c) (- (1+ c)))))
	(cond ((possible-black-move board new-pos)
	       (setf positions (cons new-pos positions)))
	      ((possible-black-capture board new-pos)
	       (setf positions (cons new-pos positions))
	       (return nil))
	      (t (return nil)))))

    ;; NW
    (dotimes (c (min (pos-row pos)
		     (pos-col pos)))
      (let ((new-pos
	     (diagonal-move pos (- (1+ c)) (- (1+ c)))))
	(cond ((possible-black-move board new-pos)
	       (setf positions (cons new-pos positions)))
	      ((possible-black-capture board new-pos)
	       (setf positions (cons new-pos positions))
	       (return nil))
	      (t (return nil)))))
    positions))





(defun possible-bishop (board pos)
  (if (eq (aref (board-board board)
		(pos-row pos)
		(pos-col pos))
	  'AB)
      ;; Bishop is white
      (possible-white-bishop board pos)
    ;; Bishop is black
    (possible-black-bishop board pos)))
