;;
;; sbcl --load "tmi-test.lisp"
;;
;; Broken stuff
;;
;; - subscribe and resubscribe events come back with "display-name" as the username of the person who resubscribed.
;;
;; - USERNOTICE message that *aren't* sub or resub events aren't parsed correctly.
;;
;; - wsd doesn't start pinging and I haven't looked into it yet.
;;   You'll notice this as you'll get disconnected after about 10
;;   minutes or so.
;;

;;
;; A note about twitch and websockets: If you open a single connection
;; and send a message to a channel, that message won't come back as a
;; PRIVMSG event.  If you're thinking of using this for a client,
;; you'll want to use two connections, using one for sending messages
;; and the other for receiving them.
;;

(ql:quickload '(:twitch-websockets
                :log4cl))


(defpackage :tmi-test
  (:use :cl))

(in-package :tmi-test)

(setf tmi::*debug* t)

(defmethod handle-message ((connection tmi:connection) (message tmi:notice))
  (log:info "NOTICE: ~a: ~a"
                   (tmi:notice-channel message)
                   (tmi:notice-channel message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:roomstate))
  (log:info "ROOMSTATE: ~a" message))

(defmethod handle-message ((connection tmi:connection) (message tmi:part))
  (log:info "~a left ~a"
                   (tmi:user-display-name message)
                   (tmi:part-channel-name message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:join))
  (log:info "~a joined ~a"
                   (tmi:user-display-name message)
                   (tmi:join-channel-name message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:addmod))
  (log:info "~a was modded in ~a"
            (tmi:user-display-name message)
            (tmi:addmod-channel message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:unmod))
  (log:info "~a was un-modded in ~a"
            (tmi:user-display-name message)
            (tmi:unmod-channel message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:clearchat))
  (log:info "~a was banned from ~a for ~a seconds."
            (tmi:clearchat-banned-user message)
            (tmi:clearchat-channel message)
            (tmi:clearchat-ban-duration message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:subscribe))
  (log:info "~a resubscribed to ~a"
            (tmi:user-display-name message)
            (tmi:subscribe-channel message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:resubscribe))
  (log:info "~a resubscribed to ~a"
            (tmi:user-display-name message)
            (tmi:resubscribe-channel message))
  )

(defmethod handle-message ((connection tmi:connection) (message tmi:clearmsg))
  (log:info "~a clearmsg ~a messageid: ~a"
            (tmi:clearmsg-login message)
            (tmi:clearmsg-channel message)
            (tmi:clearmsg-message-id message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:ws-close))
  (log:info "Connection closed: ~a ~a"
            (tmi:ws-close-code message)
            (tmi:ws-close-reason message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:ws-open))
  (log:info "WS Connection opened."))

(defmethod handle-message ((connection tmi:connection) (message tmi:privmsg))
  ;; (log:info "~a ~a >>> ~a"
  ;;           (tmi:privmsg-channel message)
  ;;           (tmi:user-display-name message)
  ;;           (tmi:privmsg-message message))
  )

(defmethod handle-message ((connection tmi:connection) message)
  (log:info "Unhandled message: ~a" message))



(defvar *connection* nil)

;; TMI hands your function straight to websockets-client, so if you
;; want to handle any errors, you'll want to wrap it in something.
(defun handle-message-wrapper (connection message)
  (handler-case
      (handle-message connection message)
    (error (condition)
      (log:info "Error in one of our handlers: ~a" condition))))


(defun setup ()
  (unless (and *connection*
               (eq (tmi:ready-state *connection*) :open))
    (setf *connection*
          (make-instance 'tmi:connection
                         :nick "chatpollbot"
                         :handler 'handle-message-wrapper

                         ;; Your twitch oauth password
                         ;; The easiest place to get this is
                         ;; https://twitchapps.com/tmi/
                         ;; It'll look something like
                         ;; "oauth:102930129310asdklwakd"
                         ;;
                         :auth "your oauth string")))

  (tmi:connect *connection*)

  ;;
  ;; At this point, to test things out it's probably best to join a
  ;; bunch of channels.  for experimentation, what I do is go to
  ;; https://www.twitch.tv/directory/all which lists all channels in
  ;; descending order by viewer count and join the ones with the most
  ;; spam.
  ;;
  ;; (tmi:join-channel *connection* "#channel-name")
  )


(log:config :sane :this-console :pretty)
(setup)
(tmi:join-channel *connection* "#quin69")
(tmi:join-channel *connection* "#asmongold")
(tmi:join-channel *connection* "#starladder5")
(tmi:join-channel *connection* "#ranboolive")
(tmi:join-channel *connection* "#gamesdonequick")
