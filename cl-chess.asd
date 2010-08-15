(defpackage #:cl-chess-asd
  (:use :cl :asdf))

(in-package :cl-chess-asd)

(defsystem cl-chess
    :name "cl-chess"
    :version "0.9.5"
    :maintainer "Olexiy Zamkoviy"
    :author "Manuel Gamallo"
    :licence "GPL v3"
    :description "Chess library"
    :components (
                 (:file "miguedrez")
                 (:file "data")
                 (:file "io")
                 (:file "ai")
                 (:module "moves"
                          :components (
                 (:file "bishop")
                 (:file "king")
                 (:file "knight")
                 (:file "moves")
                 (:file "pawn")
                 (:file "queen")
                 (:file "rook")))))
