
(asdf:defsystem #:twitch-websockets
  :description "A WebSockets client interface to Twitch chat.  You have been warned."
  :author "Clint Moore <clint@ivy.io>"
  :license  "MIT" ;; MIT or BSD3?
  :version "0.0.1"
  :serial t

  :depends-on (:alexandria
               :websocket-driver
               :cl-ppcre)

  :components ((:file "twitch-websockets")))
