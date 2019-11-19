
(defpackage #:twitch-websockets
  (:use #:cl)
  (:nicknames "tmi" "TMI")
  (:import-from :alexandria
                #:when-let
                #:switch
                #:alist-hash-table)
  
  (:export #:user
           #:reconnect
           #:whisper
           
           ;; events
           #:clearchat
           #:clearmsg
           #:whisper
           #:privmsg
           #:action
           #:resubscribe
           #:addmod
           #:unmod
           #:connected
           #:disconnected
           
           #:make-connection
           #:close-connection
           
           #:join
           #:part
           #:notice
           #:hosting
           #:roomstate

           #:clearchat-channel
           #:clearchat-banned-user
           #:clearchat-ban-duration
           #:clearmsg-channel
           #:clearmsg-login
           #:clearmsg-message-id
           
           #:privmsg-channel
           #:privmsg-message
           #:privmsg-tags
           #:privmsg-raw
           #:privmsg-subscribed
           #:privmsg-mod
           
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
           #:unmod-channel
           #:addmod-channel
           #:part-channel
           #:join-channel
           #:notice-message
           #:notice-channel
           #:hosting-who
           #:hosting-target
           #:roomstate-emote-only
           #:roomstate-followers-only
           #:roomstate-r9k
           #:roomstate-rituals
           #:roomstate-room-id
           #:roomstate-slow
           #:roomstate-subs-only
           #:whisper-message
           
           #:user-info
           #:user-display-name
           #:user-username
           #:user-mod))

(in-package #:twitch-websockets)


(defclass reconnect () ())

(defclass user ()
  ((username :initarg :username
             :accessor user-username)
   (display-name :initarg :display-name
                 :accessor user-display-name)
   (info :initarg :user-info
         :accessor user-info)
   (userid :initarg :user-id
           :accessor user-id)))

(defmethod print-object ((user user) out)
  (print-unreadable-object (user out :type t)
    (format out "~a ~a ~a"
            (user-info user)
            (user-display-name user)
            (user-username user))))

(defclass whisper (user)
  ((message :initarg :message
            :accessor whisper-message)))

(defclass clearchat ()
  ((channel :initarg :channel
            :accessor clearchat-channel)
   (banned-user :initarg :banned-user
                :accessor clearchat-banned-user)
   (ban-duration :initarg :ban-duration
                 :accessor clearchat-ban-duration)))

(defclass clearmsg ()
  ((channel :initarg :channel
            :accessor clearmsg-channel)
   (login :initarg :login
          :accessor clearmsg-login)
   (message-id :initarg :message-id
               :accessor clearmsg-message-id)))

(defclass privmsg (user)
  ((channel :initarg :channel
            :accessor privmsg-channel)
   (message :initarg :message
            :accessor privmsg-message)
   (tags :initarg :tags
         :accessor privmsg-tags)
   (raw :initarg :raw
        :accessor privmsg-raw)
   (subscribed :initarg :subscribed
               :accessor privmsg-subscribed)
   (mod :initarg :mod
        :accessor privmsg-mod)))

(defclass action (privmsg)
  ())

(defclass resubscribe (user)
  ((twitch-id :initarg :twitch-id
              :accessor resubscribe-twitch-id)
   (user :initarg :user
         :accessor resubscribe-user)
   (plan :initarg :plan
         :accessor resubscribe-plan)
   (turbo :initarg :turbo
          :accessor resubscribe-turbo)
   (months :initarg :months
           :accessor resubscribe-months)
   (premium :initarg :premium
            :accessor resubscribe-premium)
   (color :initarg :color
          :accessor resubscribe-color)
   (channel :initarg :channel
            :accessor resubscribe-channel)
   (message :initarg :message
            :accessor resubscribe-message)
   (tags :initarg :tags
         :accessor resubscribe-tags)))

(defclass unmod (user)
  ((channel :initarg :channel
            :accessor unmod-channel)))

(defclass addmod (user)
  ((channel :initarg :channel
            :accessor addmod-channel)))

(defclass part (user)
  ((channel :initarg :channel
            :accessor part-channel)))

(defclass join (user)
  ((channel :initarg :channel
            :accessor join-channel)))

(defclass notice ()
  ((message :initarg :message
            :accessor notice-message)
   (channel :initarg :channel
            :accessor notice-channel)))

(defclass hosting ()
  ((who :initarg :who
        :accessor hosting-who)
   (target :initarg :target
           :accessor hosting-target)))

(defclass roomstate ()
  ((emote-only :initarg :emote-only
               :accessor roomstate-emote-only)
   (followers-only :initarg :followers-only
                   :accessor roomstate-followers-only)
   (r9k :initarg :r9k
        :accessor roomstate-r9k)
   (rituals :initarg :rituals
            :accessor roomstate-rituals)
   (room-id :initarg :room-id
            :accessor roomstate-room-id)
   (slow :initarg :slow
         :accessor roomstate-slow)
   (subs-only :initarg :subs-only
              :accessor roomstate-subs-only)))

(defclass disconnected ()
  nil)

(defclass connected ()
  nil)

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

(defun parse-message (connection raw-message)
  (let* ((split-message (split-irc-message raw-message))
         ;; This is not the message type.  It's to tell things like
         ;; PINGs etc. from PRIVMSG and friends.
         (command (car split-message)))
    
    (cond ((string= command "PONG") (return-from parse-message nil))
          ((string= command "PING")
           ;; Not sure why we're getting pings.  The twitch docs
           ;; say that we're supposed to SEND pings, not receive
           ;; them, but ok, we're flexible.
           (wsd:send connection "PONG")
           (return-from parse-message nil)))

    ;; Check for a reconnect message.
    (when (and (cadr split-message)
               (string= (cadr split-message) "RECONNECT"))
      (return-from parse-message (make-instance 'reconnect)))
    
    (destructuring-bind (user-info user message-type &rest message)
        split-message
      (alexandria:switch (message-type :test #'equal)

        ;; Ignoring these for now.
        ("USERSTATE" (log:info "USERSTATE: ~a" raw-message))

        ("RECONNECT"
         (log:info "GOT RECONNECT EVENT!!!")
         (websocket-driver.ws.base:close-connection connection)
         (make-instance 'disconnected))

        ("WHISPER"
         (make-instance 'whisper
                        :display-name (gethash "display-name"
                                               (parse-user-tags user-info))
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
           ;;(log:info "~a" (alexandria:hash-table-keys user-tags))
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
           ;;(log:info "~a" (alexandria:hash-table-keys user-tags))
           (cond ((or (string= notice-type "resub")
                      (string= notice-type "sub"))
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
                                           " "))))))

        ;; The MODE events (ie. opping/modding a person) are in a
        ;; different format.
        (t
         ;;(log:info "Message type: ~a" message-type)
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
                    
                  (t nil ;;(log:info "FELL THROUGH: ~a" message)
                   ))))
           
           (dolist (single-message (ppcre:split "" raw-message))
             (handle-multimessage
              (split-irc-message single-message)))))))))

(defun make-connection (nick pass handler &key (verify t))
  (let ((connection (wsd:make-client "wss://irc-ws.chat.twitch.tv:443/irc")))

    (wsd:on :error connection
            (lambda (error)
              (log:info error)))

    (wsd:on :close connection
            #'(lambda (&key code reason)
                (declare (ignore code reason))
                (apply handler (list (make-instance 'disconnected) connection))))

    (wsd:on :open connection
            (lambda ()
              (wsd:send
               connection
               "CAP REQ :twitch.tv/tags twitch.tv/commands twitch.tv/membership")
              (wsd:send connection (format nil "PASS ~a" pass))
              (wsd:send connection (format nil "NICK ~a" nick))
              (apply handler (list (make-instance 'connected) connection))))

    (wsd:on :message connection
            #'(lambda (message)
                (when-let ((parsed-message (funcall 'parse-message
                                                    connection
                                                    message)))
                  (apply handler (list parsed-message connection)))))
    (wsd:start-connection connection :verify verify)
    connection))

(defun close-connection (connection)
  (wsd:close-connection connection))

(defun join (connection channel-name)
  (wsd:send connection (format nil "JOIN #~a" channel-name)))

(defun part (connection channel-name)
  (wsd:send connection (format nil "PART #~a" channel-name)))
