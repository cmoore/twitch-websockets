
(defpackage #:twitch-websockets
  (:use #:cl)
  (:nicknames "tmi" "TMI")
  (:import-from :alexandria
                #:when-let
                #:switch
                #:alist-hash-table)

  (:export #:connection
           #:connection-nick
           #:connection-auth
           #:connection-websocket
           #:connection-handler

           #:user
           #:user-username
           #:user-display-name
           #:user-user-info
           #:user-userid

           #:whisper
           #:whisper-message

           #:clearchat
           #:clearchat-channel
           #:clearchat-banned-user
           #:clearchat-ban-duration

           #:clearmsg
           #:clearmsg-channel
           #:clearmsg-login
           #:clearmsg-message-id

           #:privmsg
           #:privmsg-channel
           #:privmsg-message
           #:privmsg-tags
           #:privmsg-raw
           #:privmsg-subscribed
           #:privmsg-mod

           #:subscribe
           #:subscribe-twitch-id
           #:subscribe-user
           #:subscribe-plan
           #:subscribe-turbo
           #:subscribe-months
           #:subscribe-premium
           #:subscribe-color
           #:subscribe-channel
           #:subscribe-message
           #:subscribe-tags

           #:resubscribe
           #:resubscribe-twitch-id
           #:resubscribe-user
           #:resubscribe-plan
           #:resubscribe-turbo
           #:resubscribe-months
           #:resubscribe-premium
           #:resubscribe-color
           #:resubscribe-channel
           #:resubscribe-message
           #:resubscribe-tags

           #:unmod
           #:unmod-channel

           #:addmod
           #:addmod-channel

           #:part
           #:part-channel-name

           #:join
           #:join-channel-name

           #:notice
           #:notice-message
           #:notice-channel

           #:hosting
           #:hosting-who
           #:hostring-target

           #:roomstate
           #:roomstate-emote-only
           #:roomstate-followers-only
           #:roomstate-r9k
           #:roomstate-rituals
           #:roomstate-room-id
           #:roomstate-slow
           #:roomstate-subs-only

           #:disconnected
           #:connected

           #:ws-error
           #:ws-error-message

           #:ws-close
           #:ws-close-code
           #:ws-close-reason

           #:ws-open

           #:reconnect
           #:action

           #:with-wsd
           #:ready-state
           #:close-connection
           #:join-channel
           #:part-channel
           #:send-message
           #:connect))

(in-package #:twitch-websockets)

(defvar *debug* nil)

(defmacro mdebug (&rest message)
  `(when *debug*
     (funcall #'(lambda ()
                  (log:info ,@message)))))

(defclass connection ()
  ((nick :initarg :nick
         :accessor connection-nick)
   (auth :initarg :auth
         :accessor connection-auth)
   (websocket :initarg :websocket
              :reader connection-websocket)
   (handler :initarg :handler
            :initform (error "You must supply a handler")
            :accessor connection-handler)))

(defclass user ()
  ((username :initarg :username
             :initform "username"
             :reader user-username)
   (display-name :initarg :display-name
                 :initform "display-name"
                 :reader user-display-name)
   (user-info :initarg :user-info
              :initform "user-info"
              :reader user-user-info)
   (userid :initarg :user-id
           :initform "userid"
           :reader user-userid)))

(defmethod print-object ((user user) out)
  (with-slots (display-name username) user
    (print-unreadable-object (user out :type t)
      (format out "~a ~a" display-name username))))


(defclass whisper (user)
  ((message :initarg :message
            :initform "no message?"
            :reader whisper-message)))

(defclass clearchat ()
  ((channel :initarg :channel
            :reader clearchat-channel)
   (banned-user :initarg :banned-user
                :reader clearchat-banned-user)
   (ban-duration :initarg :ban-duration
                 :reader clearchat-ban-duration)))

(defclass clearmsg ()
  ((channel :initarg :channel
            :reader clearmsg-channel)
   (login :initarg :login
          :reader clearmsg-login)
   (message-id :initarg :message-id
               :reader clearmsg-message-id)))

(defclass privmsg (user)
  ((channel :initarg :channel
            :reader privmsg-channel)
   (message :initarg :message
            :reader privmsg-message)
   (tags :initarg :tags
         :reader privmsg-tags)
   (raw :initarg :raw
        :reader privmsg-raw)
   (subscribed :initarg :subscribed
               :reader privmsg-subscribed)
   (mod :initarg :mod
        :reader privmsg-mod)))

(defclass subscribe (user)
  ((twitch-id :initarg :twitch-id
              :reader subscribe-twitch-id)
   (user :initarg :user
         :reader subscribe-user)
   (plan :initarg :plan
         :reader subscribe-plan)
   (turbo :initarg :turbo
          :reader subscribe-turbo)
   (months :initarg :months
           :reader subscribe-months)
   (premium :initarg :premium
            :reader subscribe-premium)
   (color :initarg :color
          :reader subscribe-color)
   (channel :initarg :channel
            :reader subscribe-channel)
   (message :initarg :message
            :reader subscribe-message)
   (tags :initarg :tags
         :reader subscribe-tags)))

(defclass resubscribe (user)
  ((twitch-id :initarg :twitch-id
              :reader resubscribe-twitch-id)
   (user :initarg :user
         :reader resubscribe-user)
   (plan :initarg :plan
         :reader resubscribe-plan)
   (turbo :initarg :turbo
          :reader resubscribe-turbo)
   (months :initarg :months
           :reader resubscribe-months)
   (premium :initarg :premium
            :reader resubscribe-premium)
   (color :initarg :color
          :reader resubscribe-color)
   (channel :initarg :channel
            :reader resubscribe-channel)
   (message :initarg :message
            :reader resubscribe-message)
   (tags :initarg :tags
         :reader resubscribe-tags)))

(defclass unmod (user)
  ((channel :initarg :channel
            :reader unmod-channel)))

(defclass addmod (user)
  ((channel :initarg :channel
            :reader addmod-channel)))

(defclass part (user)
  ((channel :initarg :channel
            :reader part-channel-name)))

(defclass join (user)
  ((channel :initarg :channel
            :reader join-channel-name)))

(defclass notice ()
  ((message :initarg :message
            :reader notice-message)
   (channel :initarg :channel
            :reader notice-channel)))

(defclass hosting ()
  ((who :initarg :who
        :reader hosting-who)
   (target :initarg :target
           :reader hosting-target)))

(defclass roomstate ()
  ((emote-only :initarg :emote-only
               :reader roomstate-emote-only)
   (followers-only :initarg :followers-only
                   :reader roomstate-followers-only)
   (r9k :initarg :r9k
        :reader roomstate-r9k)
   (rituals :initarg :rituals
            :reader roomstate-rituals)
   (room-id :initarg :room-id
            :reader roomstate-room-id)
   (slow :initarg :slow
         :reader roomstate-slow)
   (subs-only :initarg :subs-only
              :reader roomstate-subs-only)))

(defmethod print-object ((roomstate roomstate) out)
  (with-slots (emote-only followers-only r9k rituals
               room-id slow subs-only) roomstate
    (print-unreadable-object (user out :type t)
      (format out "emote: ~a followers: ~a r9k: ~a rituals: ~a room-id ~a slow: ~a subs-only: ~a"
              emote-only followers-only r9k rituals room-id slow subs-only))))

(defclass disconnected ()
  nil)

(defclass connected ()
  nil)

(defclass ws-error ()
  ((message :initarg :message
            :reader ws-error-message)))

(defclass ws-close ()
  ((code :initarg :code
         :reader ws-close-code)
   (reason :initarg :reason
           :reader ws-close-reason)))

(defclass ws-open () ())

(defclass reconnect () ())

(defclass action (privmsg) ())



;; Parser... it's ugly, just warning you...

(defun parse-user-tags (info-line)
  (let ((entries (ppcre:split ";" info-line))
        (result (make-hash-table :test #'equalp)))
    (map nil (lambda (entry)
               (let ((kv (ppcre:split "=" entry)))
                 (setf (gethash (car kv) result) (cadr kv))))
         entries)
    result))

(defun parse-user-info (bundle)
  (let ((pairs (ppcre:split ";" bundle)))
    (mapcar (lambda (pair)
                 (ppcre:split "=" pair))
               pairs)))

(defun drop-colon (string)
  (ppcre:regex-replace "^:" string ""))

(defun drop-hash (string)
  (ppcre:regex-replace "^#" string ""))

(defun scrub-message (message)
  (cl-ppcre:regex-replace-all "" message ""))

(defun split-irc-message (message)
  (ppcre:split " " (ppcre:regex-replace-all "\\r\\n$" message "")))

(defun parse-irc-name (string)
  (ppcre:regex-replace "^:"
                       (car (ppcre:split "!" string))
                       ""))

(defun parse-message (websocket raw-message)
  (let* ((split-message (split-irc-message raw-message))
         ;; This is not the message type.  It's to tell things like
         ;; PINGs etc. from PRIVMSG and friends.
         (command (car split-message)))
    (when (string= command "PING")
      (return-from parse-message nil))
    ;; Check for a reconnect message.
    (when (and (cadr split-message)
               (string= (cadr split-message) "RECONNECT"))
      (return-from parse-message (make-instance 'reconnect)))

    (destructuring-bind (user-info user message-type &rest message)
        split-message
      (unless (every #'upper-case-p message-type)
        (mdebug "TMI: message-type: ~a: ~a" message-type raw-message))
      (alexandria:switch (message-type :test #'equal)

        ;; Ignoring these for now.
        ("USERSTATE" nil)

        ("RECONNECT"
         (websocket-driver.ws.base:close-connection websocket)
         (make-instance 'disconnected))

        ("WHISPER"
         (make-instance 'whisper
                        :display-name (or (gethash "display-name"
                                                   (parse-user-tags user-info))
                                          "Something's fucky.")
                        :username user
                        :user-info user-info
                        :message (drop-colon
                                  (format nil "~{~A ~}" (cdr message)))))

        ("PRIVMSG"
         (let* ((user-tags (parse-user-tags user-info))
                (message-text (scrub-message
                               (drop-colon
                                (format nil "~{~A ~}" (cdr message)))))
                (channel (car message)))
           (if (ppcre:scan "^\\s?ACTION " message-text)
               (make-instance 'action
                              :message message-text
                              :channel channel
                              :user-info user-info
                              :display-name (gethash "display-name" user-tags)
                              :username user ;;(gethash "login" user-tags)
                              :user-id (gethash "user-id" user-tags)
                              :mod (gethash "mod" user-tags)
                              :subscribed (gethash "subscriber" user-tags)
                              :tags user-tags
                              :raw raw-message)
               (make-instance 'privmsg
                              :message message-text
                              :channel channel
                              :user-info user-info
                              :display-name (gethash "display-name" user-tags)
                              :username user
                              :user-id (gethash "user-id" user-tags)
                              :mod (gethash "mod" user-tags)
                              :subscribed (gethash "subscriber" user-tags)
                              :tags user-tags
                              :raw raw-message))))

        ("CLEARMSG"
         (let ((user-tags (parse-user-tags user-info)))
           (make-instance 'clearmsg
                          :message-id (gethash "target-msg-id" user-tags)
                          :login (gethash "@login" user-tags)
                          :channel (drop-colon (car message)))))

        ("CLEARCHAT"
         (make-instance 'clearchat
                        :channel (drop-hash (car message))
                        :banned-user (scrub-message (drop-colon (cadr message)))
                        :ban-duration (gethash "@ban-duration"
                                               (parse-user-tags user-info) "0")))

        ("USERNOTICE"
         (let* ((user-tags (parse-user-tags user-info))
                (notice-type (gethash "msg-id" user-tags)))
           (alexandria:switch (notice-type :test #'string=)
             ("resub" (mdebug "TMI: RESUB USER TAGS: ~a /////////// ~a" user-tags raw-message)
                      (make-instance 'resubscribe
                                     :twitch-id (gethash "user-id" user-tags)
                                     :user (gethash "display-name" user-tags)
                                     :color (gethash "color" user-tags)
                                     :premium nil
                                     :plan (gethash "msg-param-sub-plan-name" user-tags)
                                     :channel (car message)
                                     :tags user-tags
                                     :message (ppcre:regex-replace-all
                                               "\\\\s"
                                               (gethash "system-msg" user-tags)
                                               " ")))
             ("sub" (mdebug "TMI: SUB USER TAGS: ~a /////////// ~a" user-tags raw-message)
                    (make-instance 'subscribe
                                   :twitch-id (gethash "user-id" user-tags)
                                   :user (gethash "display-name" user-tags)
                                   :color (gethash "color" user-tags)
                                   :premium nil
                                   :plan (gethash "msg-param-sub-plan-name" user-tags)
                                   :channel (car message)
                                   :tags user-tags
                                   :message (ppcre:regex-replace-all
                                             "\\\\s"
                                             (gethash "system-msg" user-tags)
                                             " ")))
             (t (mdebug "TMI: UNKNOWN USERNOTICE TYPE: ~a" raw-message)))))

        ;; The MODE events (ie. opping/modding a person) are in a
        ;; different format.
        (t
         (labels
             ((make-mode (name channel mode)
                (switch (mode :test #'string=)
                  ("+o" (make-instance 'addmod
                                       :display-name name
                                       :channel channel))
                  ("-o" (make-instance 'unmod
                                       :display-name name
                                       :channel channel))))
              (handle-multimessage (message)
                (cond
                  ((string= (nth 2 message) "NOTICE")
                   (progn
                     (let ((message-text (drop-colon
                                          (format nil "~{~A ~}" (subseq message 4)))))
                       (return-from parse-message
                         (make-instance 'notice :message message-text
                                                :channel (drop-hash (nth 3 message)))))))
                  ((string= (nth 1 message) "HOSTTARGET")
                   (return-from parse-message
                     (make-instance 'hosting
                                    :who (drop-hash (nth 2 message))
                                    :target (drop-colon (nth 3 message)))))
                  ((string= (nth 2 message) "ROOMSTATE")
                   (let ((tags (parse-user-tags (nth 0 message))))
                     (return-from parse-message
                       (make-instance 'roomstate
                                      :subs-only (gethash "subs-only" tags)
                                      :slow (gethash "slow" tags)
                                      :room-id (gethash "room-id" tags)
                                      :rituals (gethash "rituals" tags)
                                      :r9k (gethash "r9k" tags)
                                      :followers-only (gethash "followers-only" tags)
                                      :emote-only (gethash "emote-only" tags)))))

                  ((string= (nth 1 message) "PART")
                   (return-from parse-message
                     (make-instance 'part
                                    :display-name (parse-irc-name (nth 0 message))
                                    :channel (drop-hash (nth 2 message)))))

                  ((string= (nth 1 message) "JOIN")
                   (return-from parse-message
                     (make-instance 'join
                                    :display-name (parse-irc-name (nth 0 message))
                                    :channel (drop-hash (nth 2 message)))))

                  ((string= (nth 1 message) "MODE")
                   (destructuring-bind (jtv mode channel action nick)
                       message
                     (declare (ignore mode jtv))
                     (return-from parse-message
                       (make-mode nick channel action))))

                  ((= 1 (length message)) nil)

                  (t nil))))

           (dolist (single-message (ppcre:split "" raw-message))
             (handle-multimessage
              (split-irc-message single-message)))))))))


;;; Utilities

(defmacro with-wsd (connection &rest body)
  `(with-slots (websocket)
       ,connection
     ,@body))

(defmethod ready-state ((connection connection))
  (with-wsd connection (wsd:ready-state websocket)))

(defmethod close-connection ((connection connection))
  (with-wsd connection (wsd:close-connection websocket)))

(defmethod join-channel ((connection connection) (channel-name string))
  (with-wsd connection
    (wsd:send websocket (format nil "JOIN ~a" channel-name))))

(defmethod part-channel ((connection connection) channel-name)
  (with-wsd connection
    (wsd:send websocket (format nil "PART ~a" channel-name))))

(defmethod send-message ((connection connection) channel-name message)
  (with-wsd connection
    (wsd:send websocket (format nil "PRIVMSG ~a ~a"
                                channel-name message))))

(defmethod connected? ((connection connection))
  (with-wsd connection
    (wsd:ready-state websocket)))

(defmethod connect ((connection connection))
  (with-wsd connection
    (wsd:start-connection websocket))
  connection)

(defmethod initialize-instance :after ((connection connection) &key)
  (with-slots (nick auth websocket handler)
      connection
    (setf websocket (wsd:make-client "wss://irc-ws.chat.twitch.tv:443/irc"))

    (wsd:on :error websocket
            (lambda (error)
              (apply handler  (list connection (make-instance 'ws-error :error error)))))
    (wsd:on :close websocket
            (lambda (&key code reason)
              (apply handler
                     (list connection
                           (make-instance 'ws-close :code code :reason reason)))))
    (wsd:on :open websocket
            (lambda ()
              (wsd:send websocket "CAP REQ :twitch.tv/tags twitch.tv/commands twitch.tv/membership")
              (wsd:send websocket (format nil "PASS ~a" auth))
              (wsd:send websocket (format nil "NICK ~a" nick))
              (apply handler (list connection (make-instance 'ws-open)))))

    (wsd:on :message websocket
            (lambda (message)
              (when-let ((parsed-message (funcall 'parse-message
                                                  websocket message)))
                (apply handler (list connection parsed-message)))))))
