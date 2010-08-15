;;; -*- encoding: utf-8 -*-
;;;
;;; knight.lisp
;;; Movements of the knights
;;; gamallo, April 1, 2007
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

;;; Places threatened by a knight
(defun pos-knight (board pos)
  (let ((positions '()))
    ;; One o'clock
    (if (and (> (pos-row pos) 1)
	     (< (pos-col pos) 7))
	(setf positions (cons (make-pos
			       :row (- (pos-row pos) 2)
				:col (1+ (pos-col pos)))
			       positions)))
    ;; Two o'clock
    (if (and (> (pos-row pos) 0)
	     (< (pos-col pos) 6))
	(setf positions (cons (make-pos
				:row (1- (pos-row pos))
				:col (+ (pos-col pos) 2))
			       positions)))
    ;; Four o'clock
    (if (and (< (pos-row pos) 7)
	     (< (pos-col pos) 6))
	(setf positions (cons (make-pos
				:row (1+ (pos-row pos))
				:col (+ (pos-col pos) 2))
			       positions)))
    ;; Five o'clock
    (if (and (< (pos-row pos) 6)
	     (< (pos-col pos) 7))
	(setf positions (cons (make-pos
				:row (+ (pos-row pos) 2)
				:col (1+ (pos-col pos)))
			       positions)))
    ;; Seven o'clock
    (if (and (< (pos-row pos) 6)
	     (> (pos-col pos) 0))
	(setf positions (cons (make-pos
				:row (+ (pos-row pos) 2)
				:col (1- (pos-col pos)))
			       positions)))
    ;; Eight o'clock
    (if (and (< (pos-row pos) 7)
	     (> (pos-col pos) 1))
	(setf positions (cons (make-pos
				:row (1+ (pos-row pos))
				:col (- (pos-col pos) 2))
			       positions)))
    ;; Ten o'clock
    (if (and (> (pos-row pos) 0)
	     (> (pos-col pos) 1))
	(setf positions (cons (make-pos
				:row (1- (pos-row pos))
				:col (- (pos-col pos) 2))
			       positions)))
    ;; Eleven o'clock
    (if (and (> (pos-row pos) 1)
	     (> (pos-col pos) 0))
	(setf positions (cons (make-pos
				:row (- (pos-row pos) 2)
				:col (1- (pos-col pos)))
			       positions)))
    positions))





(defun possible-knight (board pos)
  (let ((positions '()))
    (dolist (a-pos (pos-knight board pos))
      (if (eq (aref (board-board board)
		    (pos-row pos)
		    (pos-col pos))
	      'CB)
	  ;; Knight is white
	  (if (eq (aref (board-whites board)
			(pos-row a-pos)
			(pos-col a-pos))
		  '0)
	      (setf positions (cons a-pos positions)))
	;; Knight is black
	(if (eq (aref (board-blacks board)
		      (pos-row a-pos)
		      (pos-col a-pos))
		'0)
	    (setf positions (cons a-pos positions)))))
    positions))
