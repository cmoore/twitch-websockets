

(ql:quickload :twitch-websockets)
(ql:quickload :alexandria)
(ql:quickload :yason)


(defpackage :tmi-test
  (:use :cl))

(in-package :tmi-test)

(defun message-handler (message ws-connection)
  (declare (ignore ws-connection))

  (typecase message
    (tmi:clearchat (log:info "~a was banned from ~a for ~a seconds."
                             (tmi:clearchat-banned-user message)
                             (tmi:clearchat-channel message)
                             (tmi:clearchat-ban-duration message)))
    
    (tmi:resubscribe (log:info "~a resubscribed to ~a"
                               (tmi:resubscribe-user message)
                               (tmi:resubscribe-channel message)))
    (tmi:clearmsg nil)

    (tmi:privmsg nil ;; This gets really spammy really quick.
       ;; (log:info "~a on ~a said: ~a"
       ;;       (tmi:user-display-name message)
       ;;       (tmi:privmsg-channel message)
       ;;       (tmi:privmsg-message message))
       )
    
    (t (log:info "Unhandled: ~a" (type-of message)))))

(defparameter *connection* nil)

(defun setup ()
  (setf *connection*
        (tmi:make-connection
         "chatpollbot"
         ;;
         ;; Your twitch oauth password thing.  The easiest place to get this
         ;; is https://twitchapps.com/tmi/ In this case, I'm just pulling it
         ;; from the config of another project.  It'll look something like
         ;; "oauth:102930129310asdklwakd"
         ;;

         ;; This just grabs my password out of another project.
         (gethash "irc-oauth"
                  (yason:parse
                   (alexandria:read-file-into-string
                    "/home/cmoore/quicklisp/local-projects/tscore/config.json")))
     
     
         'message-handler))


  ;; 
  ;; At this point, to test things out it's probably best to join a bunch of channels.
  ;; for experimentation, what I do is go to
  ;; https://www.twitch.tv/directory/all  which lists all channels in descending order
  ;; by viewer count and join the ones with the most spam.
  ;; 
  
  (tmi:join *connection* "sodapoppin"))


