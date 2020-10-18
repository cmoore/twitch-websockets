

;;
;; A note about twitch and websockets: If you open a single connection
;; and send a message to a channel, that message won't come back as a
;; PRIVMSG event.  If you're thinking of using this for a client,
;; you'll want to use two connections, using one for sending messages
;; and the other for receiving them.
;;

(ql:quickload '(:twitch-websockets
                :alexandria
                :yason))

(defpackage :tmi-test
  (:use :cl))

(in-package :tmi-test)

(defun message-handler (ws-connection message)
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

    (tmi:privmsg
     (log:info "~a on ~a said ~a"
               (tmi:user-display-name message)
               (tmi:privmsg-channel message)
               (tmi:privmsg-message message)))

    (t (log:info "Unhandled: ~a ~a" ws-connection message))))

(defvar *connection* nil)

(defun setup ()
  (unless (and *connection*
               (eq :open
                   (wsd:ready-state
                    (tmi:connection-websocket *connection*))))
    (setf *connection*
          (make-instance 'tmi:connection
                         :nick "chatpollbot"
                         ;;
                         ;; Your twitch oauth password
                         ;; The easiest place to get this is
                         ;; https://twitchapps.com/tmi/
                         ;; In this case, I'm just pulling it
                         ;; from the config of another project.
                         ;; It'll look something like
                         ;; "oauth:102930129310asdklwakd"
                         ;;
                         :auth (gethash
                                "irc-oauth"
                                (yason:parse
                                 (alexandria:read-file-into-string
                                  (asdf:system-relative-pathname
                                   :chatpoll
                                   "config.json"))))))
    (tmi:start-connection *connection* 'message-handler :verify nil))



  ;;
  ;; At this point, to test things out it's probably best to join a
  ;; bunch of channels.  for experimentation, what I do is go to
  ;; https://www.twitch.tv/directory/all which lists all channels in
  ;; descending order by viewer count and join the ones with the most
  ;; spam.
  ;;



  ;; Don't forget the # when joining channels.
  ;; (tmi:join *connection* "#channel-name")
  )
