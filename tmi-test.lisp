;;
;; sbcl --load tmi-test.lisp
;;
;; Beware, if you don't edit the channel list at the bottom of this
;; file, your repl might well start filling up with obscenities or
;; worse, when you run the script as above.
;;
;;
;; Broken stuff
;;
;; - subscribe and resubscribe events come back with "display-name" as the username of the person who resubscribed.
;;
;; - USERNOTICE message that *aren't* sub or resub events aren't parsed correctly.

;;
;; A note about twitch and websockets: If you open a single connection
;; and send a message to a channel, that message won't come back as a
;; PRIVMSG event.  If you're thinking of using this for a client,
;; you'll want to use two connections, using one for sending messages
;; and the other for receiving them.
;;

;; (ql:quickload '(:twitch-websockets
;;                 :log4cl))


(defpackage :tmi-test
  (:use :cl))

(in-package :tmi-test)

(setf tmi::*debug* t)


(defmethod handle-message ((connection tmi:connection) (message tmi:hosting))
  (log:info "~a is now hosting ~a"
            (tmi:hosting-who message)
            (tmi:hosting-target message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:addmod))
  (log:info "~a was modded on ~a"
            (tmi:user-display-name message)
            (tmi:addmod-channel message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:addmod))
  (log:info "~a was UN-modded on ~a"
            (tmi:user-display-name message)
            (tmi:unmod-channel message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:whisper))
  (log:info "~a whispered: ~a"
            (tmi:user-display-name message)
            (tmi:whisper-message message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:notice))
  (log:info "NOTICE: ~a: ~a"
                   (tmi:notice-channel message)
                   (tmi:notice-channel message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:roomstate))
  ;;(log:info "ROOMSTATE: ~a" message)
  )

(defmethod handle-message ((connection tmi:connection) (message tmi:part))
  (log:info "~a left ~a"
                   (tmi:user-display-name message)
                   (tmi:part-channel-name message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:join))
  (log:info "~a joined ~a"
                   (tmi:user-display-name message)
                   (tmi:join-channel-name message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:clearchat))
  (log:info "~a was banned from ~a for ~a seconds."
            (tmi:clearchat-banned-user message)
            (tmi:clearchat-channel message)
            (tmi:clearchat-ban-duration message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:subscribe))
  (log:info (tmi:subscribe-message message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:resubscribe))
  (log:info (tmi:resubscribe-message message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:clearmsg))
  (log:info "~a clearmsg ~a messageid: ~a"
            (tmi:clearmsg-login message)
            (tmi:clearmsg-channel message)
            (tmi:clearmsg-message-id message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:ws-close))
  (log:info "Connection closed: ~a ~a"
            (tmi:ws-close-code message)
            (tmi:ws-close-reason message)))

(defmethod handle-message ((connection tmi:connection) (message tmi:reconnect))
  (log:info "***************** RECEIEVED RECONNECT *****************************"))

(defmethod handle-message ((connection tmi:connection) (message tmi:ws-open))
  (log:info "WS Connection opened."))

(defmethod handle-message ((connection tmi:connection) (message tmi:privmsg))
  (log:info "~a ~a >>> ~a"
            (tmi:privmsg-channel message)
            (tmi:user-display-name message)
            (tmi:privmsg-message message))
  )

(defmethod handle-message ((connection tmi:connection) message)
  (log:info "Unhandled message: ~a" message))



(defvar *connection* nil)


(defvar *message-log* nil)
(defun setup ()
  (unless (and *connection*
               (eq (tmi:ready-state *connection*) :open))
    (setf *connection*
          (make-instance 'tmi:connection
                         :nick "chatpollbot"
                         :handler #'(lambda (connection message)
                                      ;;(push message *message-log*)
                                      (handler-case
                                          (handle-message connection message)
                                        (error (condition)
                                          (log:info "Error in our handlers: ~a" condition))))

                         ;; Your twitch oauth password
                         ;; The easiest place to get this is
                         ;; https://twitchapps.com/tmi/
                         ;; It'll look something like
                         ;; "oauth:102930129310asdklwakd"
                         ;;
                         :auth "oauth:klzf0khuss0mgkrx9qclkqoqmbq6hf")))

  (tmi:connect *connection*)
  (bt:make-thread (lambda ()
                    (block oof
                      (loop
                        (unless (eq :open (tmi:ready-state *connection*))
                          (log:info "Not ponging a closed connection")
                          (return-from oof))
                        (wsd:send (tmi:connection-websocket *connection*)
                                  #.(make-array 0 :element-type '(unsigned-byte 8))
                                  :type :pong)
                        (sleep 10)))))
  ;;
  ;; At this point, to test things out it's probably best to join a
  ;; bunch of channels.  for experimentation, what I do is go to
  ;; https://www.twitch.tv/directory/all which lists all channels in
  ;; descending order by viewer count and join the ones with the most
  ;; spam.
  ;;
  ;; (tmi:join-channel *connection* "#channel-name")
  )

(defun start ()
  (setup)

  ;; (tmi:join-channel *connection* "#moonmoon")
  ;; (tmi:join-channel *connection* "#faker")
  ;; (tmi:join-channel *connection* "#trainwreckstv")
  ;; (tmi:join-channel *connection* "#timthetatman")
  ;; (tmi:join-channel *connection* "#iiTzTimmy")
  ;; (tmi:join-channel *connection* "#ThisIsNotGeorgeNotFound")
  ;; (tmi:join-channel *connection* "#gamesdonequick")
  ;; (tmi:join-channel *connection* "#sodapoppin")
  ;; (tmi:join-channel *connection* "#quin69")

  ;; (tmi:join-channel *connection* "#ppy")
  (tmi:join-channel *connection* "#fart_simulator")
  )



(defun fart-omg ()
  (dotimes (x 10)
    (tmi:send-message *connection* "#fart_simulator" "! fart a")
    (sleep 1)
    (tmi:send-message *connection* "#fart_simulator" "!fart b")
    (sleep 1))
  (tmi:send-message *connection* "#fart_simulator" "!match"))
