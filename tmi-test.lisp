


(defpackage :tmi-test
  (:use :cl))


(in-package :tmi-test)


(defun message-handler (message ws-connection)
  (declare (ignore ws-connection))
  (log:info "thingy!"))

(defparameter *connection*
  (tmi:make-connection "chatpollbot"

                       ""
                       ;;
                       ;; Your twitch oauth password thing.
                       ;; The easiest place to get this is
                       ;; https://twitchapps.com/tmi/
                       ;;
                       
                       'message-handler))


