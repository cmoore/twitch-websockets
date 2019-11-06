

(ql:quickload '(:twitch-websockets
                :alexandria
                :yason))

(declaim (optimize (speed 3) (safety 1)))

(defpackage :tmi-test
  (:use :cl))

(in-package :tmi-test)

(defun message-handler (message ws-connection)
  (declare (ignore ws-connection))
  (typecase message

    (tmi:notice (log:info "NOTICE: ~a" (tmi:notice-message message)))
    
    (tmi:roomstate (log:info "ROOMSTATE: ~a ~a"
                             (tmi:roomstate-room-id message)
                             (tmi:roomstate-subs-only message)))
    
    (tmi:part
       (log:info "~a left ~a"
                 (tmi:user-display-name message)
                 (tmi:part-channel message)))

    (tmi:join
       (log:info "~a joined ~a"
                 (tmi:user-display-name message)
                 (tmi:join-channel message)))
    
    (tmi:addmod
       (log:info "~a was modded in ~a"
                 (tmi:user-display-name message)
                 (tmi:addmod-channel message)))
       
    (tmi:unmod
       (log:info "~a was un-modded in ~a"
                 (tmi:user-display-name message)
                 (tmi:unmod-channel message)))
    
    (tmi:clearchat
       (log:info "~a was banned from ~a for ~a seconds."
                 (tmi:clearchat-banned-user message)
                 (tmi:clearchat-channel message)
                 (tmi:clearchat-ban-duration message)))
    
    (tmi:resubscribe
       (log:info "~a resubscribed to ~a"
                 (tmi:resubscribe-user message)
                 (tmi:resubscribe-channel message)))
    
    (tmi:clearmsg
       (log:info "~a clearmsg ~a messageid: ~a"
                 (tmi:clearmsg-login message)
                 (tmi:clearmsg-channel message)
                 (tmi:clearmsg-message-id message)))

    (tmi:privmsg  nil) 
    ;; (log:info "~a on ~a said ~a"
    ;;    (tmi:user-display-name message)
    ;;    (tmi:privmsg-channel message)
    ;;    (tmi:privmsg-message message))
    
    (t (log:info "Unhandled: ~a" message)))) 

(defparameter *connection* nil)

(defun setup ()
  (setf *connection*
        (tmi:make-connection "chatpollbot"
                             ;;
                             ;; Your twitch oauth password
                             ;; The easiest place to get this is
                             ;; https://twitchapps.com/tmi/
                             ;; In this case, I'm just pulling it
                             ;; from the config of another project.
                             ;; It'll look something like
                             ;; "oauth:102930129310asdklwakd"
                             ;;

                             (gethash "irc-oauth"
                                      (yason:parse
                                       (alexandria:read-file-into-string
                                        (asdf:system-relative-pathname :chatpoll "config.json"))))
         
                             'message-handler
                             :verify nil))


  ;; 
  ;; At this point, to test things out it's probably best to join a
  ;; bunch of channels.  for experimentation, what I do is go to
  ;; https://www.twitch.tv/directory/all which lists all channels in
  ;; descending order by viewer count and join the ones with the most
  ;; spam.
  ;; 
  
  (tmi:join *connection* "sodapoppin"))
