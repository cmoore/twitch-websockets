
(defpackage #:twitch-websockets
  (:use #:cl)
  (:nicknames "tmi" "TMI")
  (:import-from :alexandria
                #:when-let
                #:switch
                #:alist-hash-table)
  
  (:export #:user
           #:whisper
           
           ;; events
           #:clearchat
           #:whisper
           #:privmsg
           #:resubscribe

           
           #:make-connection
           #:close-connection
           
           #:join
           #:part))

(in-package #:twitch-websockets)

(defclass user ()
  ((username :initarg :username
             :accessor user-username)
   (display-name :initarg :display-name
                 :accessor user-display-name)
   (info :initarg :user-info
         :accessor user-info)))
(export 'user-info)
(export 'user-display-name)
(export 'user-username)


(defclass whisper (user)
  ((message :initarg :message
            :accessor whisper-message)))
(export 'whisper-message)


(defclass clearchat ()
  ((channel :initarg :channel
            :accessor clearchat-channel)
   (banned-user :initarg :banned-user
                :accessor clearchat-banned-user)
   (ban-duration :initarg :ban-duration
                 :accessor clearchat-ban-duration)))
(export 'clearchat-channel)
(export 'clearchat-banned-user)
(export 'clearchat-ban-duration)

(defclass privmsg (user)
  ((channel :initarg :channel
            :accessor privmsg-channel)
   (message :initarg :message
            :accessor privmsg-message)))
(export 'privmsg-channel)
(export 'privmsg-message)

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
            :accessor resubscribe-message)))
(export 'resubscribe-twitch-id)
(export 'resubscribe-user)
(export 'resubscribe-plan)
(export 'resubscribe-turbo)
(export 'resubscribe-months)
(export 'resubscribe-premium)
(export 'resubscribe-color)
(export 'resubscribe-channel)
(export 'resubscribe-message)

(defun parse-user-tags (info-line)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let ((entries (ppcre:split ";" info-line))
        (result (make-hash-table :test #'equalp)))
    (map nil (lambda (entry)
               (let ((kv (ppcre:split "=" entry)))
                 (setf (gethash (car kv) result) (cadr kv))))
         entries)
    result))

(defun start-ping-handler (client)
  (let ((ws-client client))
    (bt:make-thread (lambda ()
                      (labels ((real-handler ()
                                 (loop
                                   (sleep 30)
                                   (when (eq :closed (wsd:ready-state ws-client))
                                     (return-from real-handler))
                                   ;; For some reason, the (wsd:send-ping) function
                                   ;; doesn't seem to work with twitch for reasons
                                   ;; I have yet to figure out.
                                   (wsd:send ws-client "PING"))))
                        (real-handler)))
                    :name "wsd ping handler")))

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

(defun parse-message (connection raw-message)
  ;;  (log:info raw-message)
  (let* ((split-message (ppcre:split " " (ppcre:regex-replace "\\r\\n$" raw-message "")))
         ;; This is not the message type.  It's to tell things like PINGs etc. from
         ;; PRIVMSG and friends.
         (command (car split-message)))
    
    (cond ((string= command "PONG") (return-from parse-message nil))
          ((string= command "PING")
           ;; Not sure why we're getting pings.  The twitch docs
           ;; say that we're supposed to SEND pings, not receive
           ;; them, but ok, we're flexible.
           (wsd:send connection "PONG")
           (return-from parse-message nil)))
    
    (destructuring-bind (user-info user message-type &rest message)
        split-message
      (switch (message-type :test #'equal)

        
        ("WHISPER"
         (make-instance 'whisper
                        :display-name (gethash "display-name"
                                               (parse-user-tags user-info))
                        :username user
                        :user-info user-info
                        :message (drop-colon
                                  (format nil "~{~A ~}" (cdr message)))))

        
        ("PRIVMSG"
         (let ((user-tags (parse-user-tags user-info)))
           (make-instance 'privmsg
                          :message (scrub-message
                                    (drop-colon
                                     (format nil
                                             "~{~A ~}"
                                             (cdr message))))
                          :channel (car message)
                          :user-info user-info ;;(parse-user-tags user-info)
                          :display-name (gethash "display-name" user-tags)
                          :username (gethash "login" user-tags))))

        
        ("CLEARMSG"
         (make-instance 'clearchat
                        :channel (drop-hash (car message))
                        :banned-user (drop-colon (cadr message))
                        :ban-duration (gethash "@ban-duration"
                                               (parse-user-tags user-info) "0")))

        
        ("CLEARCHAT"
         (log:info "channel: ~a user: ~a duration: ~a"
                   (drop-hash (car message))
                   (scrub-message (drop-colon (cadr message)))
                   (gethash "@ban-duration"
                            (parse-user-tags user-info) "NOTFOUND"))
         ;; (make-instance 'clearchat
         ;;                :channel (drop-hash (car message))
         ;;                :banned-user (scrub-message (drop-colon (cadr message)))
         ;;                :ban-duration (gethash "@ban-duration"
         ;;                                       (parse-user-tags user-info) "0"))
         )
        
        ("USERNOTICE"
         (let* ((user-tags (parse-user-tags user-info))
                (notice-type (gethash "msg-id" user-tags)))
           (cond ((or (string= notice-type "resub")
                      (string= notice-type "sub"))
                  (make-instance 'resubscribe
                                 :twitch-id (gethash "user-id" user-tags)
                                 :user (gethash "display-name" user-tags)
                                 :color (gethash "color" user-tags)
                                 :premium nil
                                 :plan (gethash "msg-param-sub-plan-name" user-tags)
                                 :channel (car message)
                                 :message (ppcre:regex-replace-all
                                           "\\\\s"
                                           (gethash "system-msg" user-tags)
                                           " "))))))
        (t nil)))))


(defun make-connection (nick pass handler)
  (let ((connection (wsd:make-client "wss://irc-ws.chat.twitch.tv:443/irc")))
    (wsd:on :error connection
            (lambda (error)
              (log:info error)))
    (wsd:on :close connection
            (lambda (&key code reason)
              (log:info "CLOSED ~a ~a" code reason)))
    (wsd:on :open connection
            (lambda ()
              (wsd:send connection
                        "CAP REQ :twitch.tv/tags twitch.tv/commands twitch.tv/membership")
              (wsd:send connection (format nil "PASS ~a" pass))
              (wsd:send connection (format nil "NICK ~a" nick))))
    (wsd:on :message connection
            #'(lambda (message)
                ;; (handler-case)
                (when-let ((parsed-message (parse-message connection message)))
                  (apply handler (list parsed-message connection)))
                ;; (error (condition)
                ;;        (log:info "HANDLER ERROR: ~a" condition)
                ;;        nil)
                ))
    (wsd:start-connection connection)
    (start-ping-handler connection)
    connection))

(defun close-connection (connection)
  (wsd:close-connection connection))

(defun join (connection channel-name)
  (wsd:send connection (format nil "JOIN #~a" channel-name)))

(defun part (connection channel-name)
  (wsd:send connection (format nil "PART #~a" channel-name)))
