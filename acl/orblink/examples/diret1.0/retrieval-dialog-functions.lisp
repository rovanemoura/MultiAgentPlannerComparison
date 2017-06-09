
(in-package :cg-user)

;;; This function needs to be modified to make a call
;;; to the diret client. The call should return a list
;;; of retrieved documents. The list should become
;;; the value of the range slot of the retrieval-list.

(defun show-retrievals ()
   (let* ((rets-window (make-retrieval-window))
          (rets-list (find-component :retrieval-list rets-window)))
      (setf (range rets-list)
            '("Risks of Bonds"
              "Risks of Stocks"
              "How to Manage Cash Effectively"))
      (setf (value rets-list)
            (first (range rets-list)))))

;;; This function needs to be modified to request
;;; the diret client to describe a data item. This
;;; is where your implementation of describeDataItemFully
;;; should be useful to the client.

(defun describe-retrieval (button new old)
   (declare (ignore old))
   (let* ((diret-rw (parent button))
          (item (value (find-component :retrieval-list diret-rw)))
          (rdescrip (find-component :retrieval-description diret-rw)))
      (cond
       ((string= item "Risks of Bonds")
        (setf (value rdescrip)
              "This document discusses some risks of the bond markets."))
       ((string= item "Risks of Stocks")
        (setf (value rdescrip)
              "This document discusses some risks of the stock markets."))
       ((string= item "How to Manage Cash Effectively")
        (setf (value rdescrip)
              "This document discusses effective cash management methods."))
       (t "no description available on this item"))))

;;; This function needs to be modified to request
;;; the diret client to fetch the text of a document from
;;; the remote server. This is where your implementation of
;;; fetchDataItem should be useful to the client.

(defun examine-retrieval (button new old)
   (declare (ignore old))
   (let* ((diret-rw (parent button))
          (item (value (find-component :retrieval-list diret-rw)))
          (examine-win (make-examine-window)))
      (select-window examine-win)
      (setf (value (find-component :examine-retrieval examine-win))
            (concatenate 'string
              "Here is the full text of "
              item))))
                  
;;; end-of-file