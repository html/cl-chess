(defpackage #:cl-chess-asd
  (:use :cl :asdf))

(in-package :cl-chess-asd)

(defsystem cl-chess
    :name "miguedrez"
    :version "0.9.5"
    :maintainer "Olexiy Zamkoviy"
    :author "Manuel Felipe Gamallo Rivero"
    :licence "GPL v3"
    :description "Chess library"
    :components (
                 (:file "miguedrez")
                 (:file "data" :depends-on ("miguedrez"))
                 (:file "io" :depends-on ("miguedrez"))
                 (:file "ai" :depends-on ("miguedrez"))
                 (:module "moves"
                      :components (
                         (:file "moves")
                         (:file "bishop" :depends-on ("moves"))
                         (:file "king" :depends-on ("moves"))
                         (:file "knight" :depends-on ("moves"))
                         (:file "pawn" :depends-on ("moves"))
                         (:file "queen" :depends-on ("moves"))
                         (:file "rook"))
                       :depends-on ("miguedrez"))))
