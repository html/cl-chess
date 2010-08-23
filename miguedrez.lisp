;;; -*- encoding: utf-8 -*-
;;;
;;; miguedrez.lisp
;;; Main
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

(defpackage :miguedrez
  (:use :cl :cl-user)
  (:export :ajz :make-move :make-pos :*pieces* :make-player))

(in-package :miguedrez)

(defvar *calls* 0)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))

(define-constant +knight-bishop-pos-weigh+
  (make-array 8 :initial-contents '(10 10 30 60 60 30 10 10)))

(define-constant +rook-pos-weigh+
  (make-array 8 :initial-contents '(60 30 30 10 10 30 30 60)))

(define-constant +pawn-pos-v-weigh+
  (make-array 8 :initial-contents '(0 0 5 10 30 100 250 900)))

(define-constant +pawn-pos-h-weigh+
  (make-array 8 :initial-contents '(2 5 10 20 20 10 5 2)))

(defun ajz ()

  ;; initialize
  (let* ((player1 (make-player :type 'manual :color 'white))
     (player2 (make-player :type 'auto :color 'black))
;;   (table (create-initial-board-tests))
     (table (create-initial-board-white-bottom))
     (current-player)
     (current-move))

    (setf current-player player1)

    (loop
       
       (show-board table)
       (setf *calls* 0)
       ;; checkmate?
       (if (checkmatep table current-player)
       ;; Yes -> end
       (progn
         (format t "Pierde el color ~A.~%"
             (player-color current-player))
         (return))
       ;; No -> load move
       (setf current-move (load-move table current-player)))
       
       ;; check move - including castlings
       (if (valid table current-move (player-color current-player))

       ;; valid -> do movement
       (progn
         (execute-move table current-move)
         (if (eq current-player player1)
         (setf current-player player2)
         (setf current-player player1)))

       ;; invalid -> automatic?
       (if (eq (player-type current-player) 'auto)
           ;; yes -> error
           (progn
         (print "Esto es una mierda...")
         (return))
           ;; not -> ask again
           (print "Movimiento de mierda. Otro, anda..."))))))

(defun load-move (table player)
  (if (eq (player-type player) 'manual)
      (read-move)
      (choose-move table (player-color player))))
