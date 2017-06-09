;; $Id: ChatServer.cl,v 2.4 2004/01/16 19:24:29 layer Exp $

(defclass my-server (chat:ChatServerI-servant)
  ((listeners :initform nil :accessor get-listeners)))

(corba:define-method addListener ((this my-server) listener)
  (format t "my-server: Got listener of: ~s~%" listener)
  (force-output)
  (push listener (get-listeners this)))

(corba:define-method sendMessage ((this my-server) message)
  (format t "my-server: sending message of: ~s~%" message)
  (force-output)
  (loop for listener in (get-listeners this)
      do
	(op:messageReceived listener message)))


