;;; -*- encoding: utf-8 -*-
;;;
;;; queen.lisp
;;; Movements of the queens
;;; gamallo, July 9, 2007
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

(defun possible-queen (board pos)
  (let* ((rook-functions '((white possible-white-rook)
			    (black possible-back-rook)))
	 (bishop-functions '((white possible-white-bishop)
			    (black possible-black-bishop)))
	 (color (if (eq (aref (board-board board)
			      (pos-row pos)
			      (pos-col pos))
			'DB)
		    'white
		  'black))
	 (rook-function (second (assoc color rook-functions)))
	 (bishop-function (second (assoc color bishop-functions)))
	 (positions '()))

    (setf positions (funcall rook-function board pos))
    (setf positions (append positions
			     (funcall bishop-function board pos)))))
