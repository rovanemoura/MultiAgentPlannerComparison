;; -=Begin Copyright Notice=-
;; copyright (c) 1986-2013 Franz Inc, Oakland, CA  All rights reserved.
;;
;; All rights reserved.
;;
;; Permission is granted only to any individual or institution which has
;; current Allegro CL license(s) to use, copy, or modify this software,
;; provided any reproduction or distribution of binary versions of this
;; software are compiled with a licensed Allegro CL, and provided
;; that this complete copyright and permission notice is maintained, intact,
;; in all copies and supporting documentation. 
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;; -=End Copyright Notice=-

(in-package :cg-user)

;;; This code demonstrates using the :cg.nodes module to create networks
;;; of graphical node-pictures that are linked by link-lines.  The :cg.nodes
;;; module handles drawing the nodes and links, keeping the links connected
;;; to the nodes, and handling mouse and keyboard events on the nodes and
;;; links.

;;; The :cg.nodes rendering module can be used along with the
;;; :cg.layout module to automatically arrange the node-pictures to make
;;; the layout relatively readable.  Or either module can be uesd alone.
;;; This code demonstrates using the two modules together.

;;; The Gruff browser for AllegroGraph uses the :cg.nodes and :cg.layout
;;; modules.  Gruff defines an agraph-node-pane window class that can be used
;;; to make node-pictures and link-lines represent triple-store triples.
;;; See the Gruff documentation for more information about that specific
;;; use of the :cg.nodes and :cg.layout modules.

;;; Through version 8.1 of Allegro CL, this example implemented the nodes and
;;; links objects from scratch.  Now it uses the nodes and links functionality
;;; that has been moved into Common Graphics for use in applications.

;;; Call (run-nodes-example) to run this example.  That will read and display
;;; the example network at nodes-sample.nod in the same directory as this file.

;;; Features of this demo:

;;; * Right-click the background and select "Add Node" to create a node.
;;; * Left-click a node and drag to move it around.
;;; * Right-click a node for a pop-up menu that allows you to:
;;;     * Change the node's label or background color.
;;;     * Resize the node.
;;;     * Stretch a new link from that node to another node.
;;;     * Delete the node.
;;; * Right-click a link for a pop-up menu that allows you to:
;;;     * Change the line-width or color of the link.
;;;     * Delete the link.
;;; * Left-click a link and drag to reconnect one of its ends.
;;; * Left-click the background and drag to scroll.
;;; * Left-click a node or link to select it.
;;; * Left-click the background to deselect any selected things.

;;; File | Random Network will create and display a random network
;;; of nodes and links, where the above features still apply.

;;; File | Browse Large Random Network will create a larger network
;;; and then display only a portion of its nodes and links.  Clicking
;;; a node will update the subset of nodes and links that are
;;; displayed so that it includes all nodes that are within two links
;;; of the clicked node.  (Clicking the node that is already selected
;;; will still move it to the center as with the other demos.)  This
;;; allows browsing through the network by following a path, always
;;; seeing all nodes that are within two steps of a current node.

;;; ----------------------------------------------------
;;; Creating the Demo Window and Defining Event-Handling

;;; This section defines sublcasses for a window and nodes and links,
;;; along with methods on those subclasses for mouse clicks and so on.

(defclass example-node-window (frame-with-single-child)())

(defclass example-node-pane (node-pane)())

(defmethod default-pane-class ((window example-node-window))
  'example-node-pane)

(defclass example-node-picture (node-picture)())

(defclass example-link-line (link-line)())

(defmethod device-open :after ((window example-node-window) slot-names options)
  
  ;; Whenever a node-window is opened, this will give it a menu-bar.
  (declare (ignore slot-names options))
  (setf (menu window)
    (open-menu
     (list (make-instance 'menu-item
             :title "~File"
             :value (open-menu
                     (list (make-instance 'menu-item
                             :title "~New"
                             :help-string
                             "Creates a new empty network of nodes."
                             :value 'new-network-of-nodes
                             :event-synonym '(control-key #\N))
                           (make-instance 'menu-item
                             :title "~Open"
                             :help-string
                             "Opens a network of nodes that has been saved."
                             :value 'open-network-of-nodes
                             :event-synonym '(control-key #\O))
                           (make-instance 'menu-item
                             :title "~Save"
                             :help-string
                             "Saves the current network of nodes to its file."
                             :value 'save-network-of-nodes
                             :event-synonym '(control-key #\S))
                           (make-instance 'menu-item
                             :title "Save ~As"
                             :help-string
                             "Saves the current network of nodes to a new file."
                             :value 'save-network-of-nodes-as
                             :event-synonym '(control-key #\A))
                           menu-separator
                           (make-instance 'menu-item
                             :title "~Random Network"
                             :help-string
                             "Generates a random network of nodes."
                             :value 'display-random-network-of-nodes
                             :event-synonym '(control-key #\R))
                           (make-instance 'menu-item
                             :title "~Browse large random network"
                             :help-string "Shows a subset of a large network around the selected node."
                             :value 'browse-large-random-network-of-nodes
                             :event-synonym '(control-key #\B))
                           )
                     'pull-down-menu (screen *system*)
                     :on-click 'funcall-menu-item-with-window)))
     'menu-bar (screen *system*))))

(defmethod node-pane-mouse-left-down ((pane example-node-pane) buttons position)
  (declare (ignore buttons position))
  
  ;; Left-click the background and drag to scroll the window.
  (if* (wait-for-drag)
     then (nodes-drag-scroll pane)
          
          ;; Left-click the background (without dragging) to deselect
          ;; the current node and/or link (if any) and to update the
          ;; layout.
     else (let* ((network (network-of-nodes pane)))
            (setf (selected-node-picture network) nil)
            (setf (selected-link-line network) nil)
            (update-layout-in-node-pane
             :max-iterations 60))))

(defmethod node-pane-mouse-right-down ((pane example-node-pane) buttons position)
  (declare (ignore buttons))
  
  ;; Right-click the background to show a global pop-up menu.
  (let* ((network (network-of-nodes pane)))
    (case (pop-up-lettered-menu
           '(:add-node :change-network-name :change-the-background-color)
           :sortp nil
           :on-print 'menu-printer)
      (:add-node (add-node-picture (make-instance 'example-node-picture
                                     :node-box (center-to-box position 40 25))
                                   network))
      (:change-network-name (change-name network pane))
      (:change-the-background-color
       (change-the-background-color pane)))))

(defmethod node-mouse-left-down ((node example-node-picture) node-pane buttons position-in-window)
  (declare (ignore buttons))
  
  ;; Left-click and drag to move a node.
  (if* (wait-for-drag)
     then (drag-node-picture node node-pane :click-position position-in-window)
          
          ;; Left-click a node (without dragging) to select the node and
          ;; cause it to move to the center of the diagram.
     else (let* ((network (network-of-nodes node-pane))
                 (moused-selected-node
                  (eq node (selected-node-picture network)))
                 (show-subset (show-subset node-pane))
                 (old-nodes (node-pictures network)))
            (setf (selected-node-picture network) node)
            (when (and show-subset (not moused-selected-node))
              (include-nearby-nodes network node-pane)
              (set-title-for-network node-pane))
            
            ;; This tries to arrange the new nodes
            ;; around the pre-existing nodes, and then arrange
            ;; all nodes further as needed below.  It may
            ;; make more of a mess for it to straighten out,
            ;; but it leaves the pre-existing nodes nearer
            ;; to where they were than otherwise I think.
            (when (and show-subset
                       (not moused-selected-node))
              (update-layout-in-node-pane
               :max-iterations 12
               :move-to-center nil
               :fixed-nodes old-nodes))
            
            (update-layout-in-node-pane
             :max-iterations 60
             :move-to-center (or (not show-subset)
                                 moused-selected-node)))))

(defmethod node-mouse-right-down ((node example-node-picture) node-pane buttons position-in-window)
  (declare (ignore buttons position-in-window))
  
  ;; Right-click a node to show a pop-up menu for that node.
  (case (pop-up-lettered-menu
         '(:connect-to-node :edit-label :change-name
                            :change-color :resize-node :delete-node)
         :sortp nil
         :on-print 'menu-printer)
    (:edit-label (edit-node-label node node-pane))
    (:change-name (change-name node node-pane))
    (:change-color (change-color node))
    (:resize-node (resize-node-picture node node-pane))
    (:delete-node (delete-node-picture node))
    (:connect-to-node (connect-to-node-picture (make-instance 'example-link-line)
                                               node node-pane))))

(defmethod link-mouse-left-down ((link example-link-line)(node-pane node-pane-mixin)
                                 buttons position-in-window)
  (declare (ignore buttons))
  
  ;; Left-click a link-line and drag to reconnect one end of that link
  ;; to another node.
  (if* (wait-for-drag)
     then (drag-link-line link node-pane
                          :click-position position-in-window
                          :link-line-class 'example-link-line)
          
          ;; Left-click a link-line (without dragging) to select that link.
     else (setf (selected-link-line (network-of-nodes node-pane)) link)))

(defmethod link-mouse-right-down ((link example-link-line) node-pane buttons position-in-window)
  (declare (ignore buttons position-in-window))
  
  ;; Right-click a link to show a pop-up menu for that link.
  (case (pop-up-lettered-menu
         '(:change-line-width 
           :change-color :change-name :delete-link)
         :sortp nil
         :on-print 'menu-printer)
    (:change-line-width (change-line-width link node-pane))
    (:change-color (change-color link))
    (:change-name (change-name link node-pane))
    (:delete-link (delete-link-line link))))

(defun menu-printer (symbol)
  
  ;; Print symbols in a pretty way in menus by changing dashes
  ;; to spaces and capitalizing the resulting words.
  (nsubstitute
   #\space #\-
   (capitalize-object symbol)))

(defmethod change-name (object (pane example-node-pane))
  (multiple-value-bind (string blah bleah accepted?)
      (ask-user-for-string
       (format nil "Enter a new programmatic name for ~s."
         (name object))
       (princ-to-string (name object))
       :~OK :~Cancel
       nil nil "New Name")
    (declare (ignore blah bleah))
    (if* accepted?
       then (setf (name object)
              (intern string :keyword))
            (typecase object
              (network-of-nodes
               (set-title-for-network pane)))
            (window-message pane "New name is ~s." (name object))
       else (window-message pane "Renaming CANCELED."))))
    
(defmethod change-line-width ((link example-link-line)(pane example-node-pane))
  (let* ((new (ask-user-for-choice
               "Select a new line-width."
               :~2 :~3 :~5 :cancel "Link line-width"
               (parent pane))))
    (if* (not (eq new :cancel))
       then (setf (link-line-width link)
              (case new (:~2 2)(:~3 3)(:~5 5)))
            (window-message pane
                "~s is now ~r pixels thick."
              (name link) (link-line-width link))
       else (window-message pane
                "Line-width changing CANCELED."))))

(defmethod change-color ((node example-node-picture))
  (let* ((new (pop-up-menu
               (open-menu
                (list
                 (make-instance 'menu-item
                   :title "Green"
                   :value (make-rgb :red 128 :green 255 :blue 128))
                 (make-instance 'menu-item
                   :title "Yellow"
                   :value yellow)
                 (make-instance 'menu-item
                   :title "White"
                   :value white))
                'pop-up-menu (screen *system*)))))
    (when new
      (setf (color node) new))))

(defmethod change-color ((link example-link-line))
  (let* ((new (pop-up-menu
               (open-menu
                (list
                 (make-instance 'menu-item
                   :title "Blue"
                   :value (make-rgb :blue 200))
                 (make-instance 'menu-item
                   :title "Green"
                   :value (make-rgb :green 170))
                 (make-instance 'menu-item
                   :title "Black"
                   :value black))
                'pop-up-menu (screen *system*)))))
    (when new
      (setf (color link) new))))

(defmethod change-the-background-color ((pane example-node-pane))
  (let* ((color (ask-user-for-color
                 :initial-color (background-color pane)
                 :stream pane)))
    (when color
      (setf (background-color pane) color)
      (invalidate pane))))

(defvar *node-or-link-in-status-bar* nil)

;;; An application can define methods on node-mouse-in, node-mouse-out,
;;; link-mouse-in, and link-mouse-out to implement arbitrary side effects
;;; whenever the mouse cursor moves over or off of nodes and links.

(defmethod node-mouse-in :after ((node example-node-picture)(pane example-node-pane))
  (mention-object-under-mouse-in-status-bar node pane))

(defmethod node-mouse-out :after ((node example-node-picture)(pane example-node-pane))
  (mention-object-under-mouse-in-status-bar nil pane))

(defmethod link-mouse-in :after ((link example-link-line)(pane example-node-pane))
  (mention-object-under-mouse-in-status-bar link pane))

(defmethod link-mouse-out :after ((link example-link-line)(pane example-node-pane))
  (mention-object-under-mouse-in-status-bar nil pane))

(defun mention-object-under-mouse-in-status-bar (object pane)
  (unless (eq object *node-or-link-in-status-bar*)
    (setq *node-or-link-in-status-bar* object)
    (typecase object
      (node-picture (window-message pane "~s with ~r link~:p."
              (name object)(length (link-lines object))))
      (link-line (window-message pane "~s connecting ~s and ~s."
              (name object)(name (node-picture-one object))
              (name (node-picture-two object))))
      (t (window-message pane "")))))

(defmethod display-network-of-nodes :after ((network network-of-nodes)(pane example-node-pane))
  (set-title-for-network pane))

(defun set-title-for-network (pane)
  (let* ((network (network-of-nodes pane)))
    (setf (title (parent pane))
      (format nil "Network ~s - ~a nodes and ~a links"
        (name network)
        (length (node-pictures network))
        (length (link-lines network))))))

;;; ----------------------------------------
;;; Creating and Displaying a Random Network

;;; Code for the File | Random Network command.
;;; This menu command shows how to create a network of nodes and links
;;; programmatically, rather than loading them from a file as
;;; the demo does initially.

(defun display-random-network-of-nodes
    (pane &key (number-of-nodes (+ 12 (random 24)))
          (number-of-links (+ number-of-nodes (random (ceiling number-of-nodes 2)))))
  (setf (show-subset pane) nil)
  
  ;; Create a new empty network object to hold nodes and links.
  (let* ((network (make-instance 'network-of-nodes
                    :name (gensym-sequential-name :network)))
         
         (visible-box (visible-box pane))
         (center-x (box-center-x visible-box))
         (center-y (box-center-y visible-box))
         (font (font pane))
         (font-height (ceiling (font-size font)))
         (nodes nil)
         (label-margin 3)
         (margins-and-borders (* 2 (1+ label-margin)))
         links label node1 node2 node link node-width node-height)
    
    ;; Create a number of random nodes and add them to the network.
    (dotimes (j number-of-nodes)
      (setq label (and (plusp j)(format nil "~:(~r~)" j)))
      (setq node-width (if (zerop j)
                           52
                         
                         ;; Antialiased text is a different size, so we must
                         ;; bind *antialiasing* if the node-pane is using it.
                         (+ (let* ((*antialiasing* (antialias-text pane)))
                              (stream-string-width pane label))
                            
                            margins-and-borders)))
      (setq node-height (if (zerop j)
                            52
                          (+ font-height margins-and-borders)))
      (setq node (make-instance 'example-node-picture
                   :name j
                   :label label
                   
                   ;; In the very first node, show a pixmap of Melvin
                   ;; instead of a label string.
                   :pixmap (and (zerop j)
                                (find-pixmap :melvin))
                   
                   ;; Initially position all node-pictures in the center of
                   ;; the node-pane.
                   :node-box (make-box-relative
                              (- center-x (floor node-width 2))
                              (- center-y (floor node-height 2))
                              node-width node-height)
                   
                   :label-margin label-margin
                   :color (make-rgb :red (+ 128 (random 128))
                                    :green (+ 128 (random 128))
                                    :blue (+ 128 (random 128)))))
      (push node nodes)
      (add-node-picture node network))
    
    ;; Create a number of random links and add them to the network,
    ;; making each link connect a random pair of nodes.
    (dotimes (j number-of-links)
      (setq node1 (nth (random number-of-nodes) nodes))
      (loop (setq node2 (nth (random number-of-nodes) nodes))
            (unless (eq node1 node2)(return)))
      (setq link (make-instance 'example-link-line
                   :name j
                   :color (make-rgb :red (random 128)
                                    :green (random 128)
                                    :blue (random 128))
                   :line-width (1+ (random 3))
                   :line-dashing (if (zerop (random 4)) :dot :solid)))
      (push link links)
      (add-link-line link node1 node2))
      
    ;; Remove any nodes that aren't linked to other nodes, since
    ;; they just get in the way and aren't interesting.
    (setf (node-pictures network)
      (delete-if-not 'link-lines nodes))
    
    ;; Display the network of nodes and links and update the
    ;; layout to arrange them nicely.
    (display-network-of-nodes network pane)
    (update-layout-in-node-pane)
    network))

;;; ---------------------------------
;;; Browsing Parts of a Large Network

;;; Code for the File | Browse Large Random Network command.
;;; This example creates pseudo-triples, similar to those in a
;;; triple-store, but does not use AllegroGraph.  It shows how
;;; to create networks of nodes and links to display from
;;; selected subsets of a master graph of domain objects.
;;; This gets a little more complicated.

(defclass example-triple ()
  ((example-subject :accessor example-subject :initarg :example-subject :initform nil)
   (example-predicate :accessor example-predicate :initarg :example-predicate)
   (example-object :accessor example-object :initarg :example-object :initform nil)))

(defclass example-resource ()
  ((title :accessor title :initarg :title :initform nil)
   (example-value :accessor example-value :initarg :example-value :initform nil)
   (example-triples :accessor example-triples
                    :initarg :example-triples
                    :initform nil)))

(defvar *example-triples* nil)

(defvar *example-resources* nil)

(defvar *example-resources-to-nodes* (make-hash-table :test #'eq :size 500))

(defvar *example-triples-to-links* (make-hash-table :test #'eq :size 500))

(defvar *original-number-of-resources* nil)

(defparameter *example-predicates*
  (list (list :is-a black)
        (list :has-a dark-blue)
        (list :wants-a dark-green)
        (list :likes dark-red)
        (list :knows dark-cyan)
        (list :avoids dark-magenta)
        (list :is-a black)))

(defun browse-large-random-network-of-nodes
    (pane &key (number-of-things (+ 40 (random 200)))
          (number-of-triples (+ number-of-things (random (* 1 number-of-things)))))
  
  ;; This demo displays a subset of a large network at any one time.
  ;; The subset consists of the currently selected node plus all nodes
  ;; and links that are within two links from the selected node.
  ;; Clicking a node makes it the selected node and changes the
  ;; subset of displayed nodes and links accordingly.
  ;; Clicking the node that is already the selected node moves it
  ;; to the center of the window; otherwise the selected node remains fixed.
  
  (setf (show-subset pane) t)
  (setq *original-number-of-resources* number-of-things)
  (clrhash *example-resources-to-nodes*)
  (clrhash *example-triples-to-links*)
  
  ;; Make an arbitrary set of things to browse.
  (setq *example-resources* nil)
  (setq *example-triples* nil)
  (let* (subject object triple)
    (dotimes (j number-of-things)
      (push (make-instance 'example-resource
              :title (format nil "~:(~r~)" j)
              :example-value j)
            *example-resources*))
    
    ;; Make an arbitrary set of triples linking the things.
    (dotimes (j number-of-triples)
      (setq subject (nth (random number-of-things) *example-resources*))
      (loop (setq object (nth (random number-of-things) *example-resources*))
            (unless (eq subject object)(return)))
      (setq triple (make-instance 'example-triple
                     :example-subject subject
                     :example-predicate (first (nth (random (length *example-predicates*))
                                                    *example-predicates*))
                     :example-object object))
      (push triple *example-triples*)
      (push triple (example-triples subject))
      (push triple (example-triples object)))
    (setq *example-resources* (delete-if-not 'example-triples *example-resources*))
    
    ;; Initially display a nodes-and-links network that includes
    ;; all things within two triples from the first thing.
    (let* ((network (make-instance 'network-of-nodes
                      :name (gensym-sequential-name :network)))
           (root-node (node-for-example-resource (first *example-resources*) pane network)))
      (setf (node-pictures network)(list root-node))
      (initialize-example-node-box root-node nil pane)
      (setf (selected-node-picture network) root-node)
      (include-nearby-nodes network pane)
      (display-network-of-nodes network pane)
      (update-layout-in-node-pane)
      
      ;; Overwrite the usual window title of the Nodes and Links example.
      (setf (title (parent pane))
        (format nil "Displaying ~a nodes and ~a links from a network of ~a nodes and ~a links."
          (length (node-pictures network))
          (length (link-lines network))
          (length *example-resources*)
          (length *example-triples*)))
      
      network)))

(defun include-nearby-nodes (network pane &key (levels 2))
  (let* ((root-node (selected-node-picture network))
         (new-nodes nil)
         (new-links nil))
    (declare (special new-nodes new-links))
    (unless root-node (return-from include-nearby-nodes))
    (collect-nodes-and-links
     root-node (node-pictures network) network pane levels)
    (dolist (link new-links)
      (push link (link-lines (node-picture-one link)))
      (push link (link-lines (node-picture-two link))))
    (setf (node-pictures network) new-nodes)
    (setf (link-lines network) new-links)))

(defun collect-nodes-and-links (node old-nodes network pane recursions-left)
  (declare (special new-nodes new-links))
  (pushnew node new-nodes :test #'eq)
  (setf (link-lines node) nil)
  (let* ((thing (represented-object node))
         (triples (example-triples thing))
         link other-thing linked-node subject object thing-is-subject)
    (dolist (triple triples)
      (setq link (link-for-example-triple triple network))
      (unless (member link new-links :test #'eq)
        (setq subject (example-subject triple))
        (setq object (example-object triple))
        (setq thing-is-subject (eq thing subject))
        (setq other-thing (if thing-is-subject object subject))
        (setq linked-node
              (case recursions-left
                (0 (find-if (lambda (other-node)
                              (eq (represented-object other-node)
                                  other-thing))
                            new-nodes))
                (t (node-for-example-resource other-thing pane network))))
        (when linked-node
          (push link new-links)
          
          ;; If this linked node was already being displayed, then
          ;; leave it where it is, and otherwise place it at the
          ;; same place as the node to which it is linked.
          (unless (member linked-node old-nodes :test #'eq)
            (initialize-example-node-box linked-node node pane))
          
          (if* thing-is-subject
             then (setf (node-picture-one link) node)
                  (setf (node-picture-two link) linked-node)
             else (setf (node-picture-one link) linked-node)
                  (setf (node-picture-two link) node))
          (unless (zerop recursions-left)
            (collect-nodes-and-links
             linked-node old-nodes network pane
             (1- recursions-left))))))))
      
(defun initialize-example-node-box (node from-node pane)
        
  ;; If we are adding a node that's linked to a node that's already
  ;; displayed, then start the new node at the same position as its
  ;; linked node.  If there's no linked node, then start the new
  ;; node in the center of the window.
  (let* ((box (node-box node))
         (width (box-width box))
         (height (box-height box))
         x y)
    (if* from-node
       then (let* ((from-box (node-box from-node)))
              (setq x (box-center-x from-box))
              (setq y (box-center-y from-box)))
       else (with-boxes (visible-box)
              (nvisible-box pane visible-box)
              (setq x (box-center-x visible-box))
              (setq y (box-center-y visible-box))))
    (center-to-box-x-y x y width height :box box)))

(defun node-for-example-resource (thing pane network)
  (or (gethash thing *example-resources-to-nodes*)
      (setf (gethash thing *example-resources-to-nodes*)
        (make-node-for-example-resource thing pane network))))

(defun make-node-for-example-resource (thing pane network)
  (let* ((label (title thing))
         (value (example-value thing))
         (label-margin 3) ;; 01jun08
         (margins-and-borders (* 2 (1+ label-margin)))
         (node-width (+ (let* ((*antialiasing* (antialias-text pane)))
                          (stream-string-width pane label))
                        margins-and-borders))
         (node-height (+ (ceiling (font-size (font pane)))
                         margins-and-borders)))
    (make-instance 'example-node-picture
      :name value
      :network-of-nodes network
      :represented-object thing
      :label label
      :node-box (make-box 0 0 node-width node-height)
      :label-margin label-margin
      :color (make-rgb :red (+ 144 (random 112))
                       :green (+ 144 (random 112))
                       :blue (+ 144 (random 112))))))

(defun link-for-example-triple (triple network)
  (or (gethash triple *example-triples-to-links*)
      (setf (gethash triple *example-triples-to-links*)
        (make-link-for-example-triple triple network))))

(defun make-link-for-example-triple (triple network)
  (make-instance 'example-link-line
    :name (gensym-sequential-name :link)
    :network-of-nodes network
    :represented-object triple
    :color (second (assoc (example-predicate triple) *example-predicates*))
    :line-width 1))

(defun escape-was-pressed (canvas)
  (declare (ignore canvas))
  (nth-value 1 (key-is-down-p vk-escape)))

(defparameter net nil) ;; the network; for debugging

;;; chee   23jul09 rename gruff-layout to graph-layout as it becomes a
;;;        CG module independent of the agraph browser now known as gruff
(defun update-layout-in-node-pane
    (&key (max-iterations 100)(move-to-center t) fixed-nodes)
  (let* ((frame (or (find-window :nodes-and-links-example)
                    (error "You must run the Nodes and Links example before using Gruff.")))
         (pane (frame-child frame))
         (network (network-of-nodes pane))
         
         ;; Toggle this to test both general types of layout,
         ;; where true is a "batch" layout for a better graph that takes longer,
         ;; and nil is an interactive layout optimized for a quicker solution.
         (from-scratch nil)
         
         )
    (setq net network) ;; a global variable for debugging
    (escape-was-pressed pane) ;; initialize pressed-since-last-call state
    (with-positions (scroll-pos)
      (nscroll-position pane scroll-pos)
      (select-window (parent pane))
      (cg.layout:graph-layout
       :nodes (node-pictures network)
       :links (link-lines network)
       :links-reader 'link-lines
       :node1-reader 'node-picture-one
       :node2-reader 'node-picture-two
       :center-x-reader 'node-picture-center-x
       :center-y-reader 'node-picture-center-y
       :center-writer 'move-node-picture
       :width-reader 'node-picture-width
       :height-reader 'node-picture-height
       :canvas pane
       :canvas-left (position-x scroll-pos)
       :canvas-top (position-y scroll-pos)
       :canvas-right (+ (position-x scroll-pos)(interior-width pane))
       :canvas-bottom (+ (position-y scroll-pos)(interior-height pane))
       
       :extended-canvas-left 0
       :extended-canvas-top 0
       :extended-canvas-right (page-width pane)
       :extended-canvas-bottom (page-height pane)
       
       ;; These may be a trade-off for incremental adjustments
       ;; versus generating the graph from scratch.  The
       ;; :keep-acceptable-node-positions and :protect-tail-node-positions
       ;; options reduce jitters at the end and can shorten the total time,
       ;; but may reduce the neatness of the final layout; not much, though,
       ;; so they're probably best defaulting to true in both cases.
       :work-from-current-layout (not from-scratch)
       :compress-layout-at-end from-scratch
       :ignore-crossed-tails nil
       :protect-tail-node-positions t
       :consider-alternate-spots-by-linked-nodes t ;; or :after-crossed-links
       
       :selected-node (selected-node-picture network)
       :selected-node-steps (and move-to-center 8)
       :fixed-nodes fixed-nodes
       :max-iterations max-iterations
       :animate t ;; nil or t or :node
       :pause nil
       
       :redisplay-function 'node-pane-redisplay-function
       :cancel-function 'escape-was-pressed))))

(cache-pixmap
 (make-instance 'pixmap
   :name :melvin
   :contents
   '((11 11 11 11 11 11 11 11 11 11 11 00 08 08 00 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 11 11 11 00 08 08 00 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 11 11 11 00 08 08 00 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 11 11 11 00 08 08 00 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 11 11 11 00 08 08 00 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 00 00 00 00 00 00 00 00 08 08 00 00 00 00 00 00 00 00 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 00 00 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 00 00 11 11 11 0 0 0 0 0 0)
     (11 11 00 00 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 00 00 11 11 0 0 0 0 0 0)
     (11 11 00 08 08 08 08 08 00 00 00 00 00 00 00 00 00 00 08 08 08 08 08 00 11 11 0 0 0 0 0 0)
     (11 11 00 08 08 08 08 00 00 08 08 08 08 08 08 08 08 00 00 08 08 08 08 00 11 11 0 0 0 0 0 0)
     (11 11 00 08 08 08 08 00 08 08 08 08 08 08 08 08 08 08 00 08 08 08 08 00 11 11 0 0 0 0 0 0)
     (11 11 00 00 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 00 00 11 11 0 0 0 0 0 0)
     (11 11 11 00 00 08 08 08 08 08 00 00 08 08 00 00 08 08 08 08 08 00 00 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 00 00 00 00 08 08 00 00 08 08 00 00 08 08 00 00 00 00 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 00 00 08 08 08 08 08 08 08 08 00 00 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 00 00 08 08 08 08 08 08 00 00 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 11 00 08 08 08 08 08 08 00 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 11 00 08 08 08 08 08 08 00 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 11 00 08 08 08 08 08 08 00 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 11 00 08 08 08 08 08 08 00 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 11 00 00 08 08 08 08 00 00 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 11 11 00 00 08 08 00 00 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 11 11 11 00 00 00 00 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
     (11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0))
   :width 26 :height 26 :bits-per-pixel 4
   :invert-p t))

;;; Default the file dialog to the directory that contains
;;; example networks.
(defparameter *nodes-dir* nil)
(setq *nodes-dir* *load-pathname*)

;;; ----------------------------------------------
;;; The Entry Point Function to Call for This Demo

;;; This demo function will create the demo window and then display
;;; nodes and links in it that are read from a file.  Two other demos
;;; are supplied on the File menu for creating networks of nodes and
;;; links programmatically.

(defun run-nodes-example ()
  
  ;; Create the example window.
  (let* ((window (make-window :nodes-and-links-example
                   :class 'example-node-window
                   :owner (screen *system*)
                   :resizable t
                   :scrollbars t
                   :exterior (make-box-relative 200 100 900 700)
                   :page-width 2400
                   :page-height 1800
                   :background-color (system-dialog-background-color)
                   :title "Node Example"))
         (pane (frame-child window))
         example-file)
    (add-common-status-bar window)
    (scroll-window-to-middle pane)
    (setf (transparent-character-background pane) t)
    
    ;; Load the nodes-sample.nod file containg a network of
    ;; nodes and links to display.
    (if* (and *nodes-dir*
              (probe-file (setq example-file (merge-pathnames
                                              "nodes-sample.nod"
                                              *nodes-dir*))))
       then (open-network-of-nodes pane example-file)
       else (new-network-of-nodes (frame-child window)))
    
    ;; Bring the window to the front and give it the keyboard focus.
    (set-foreground-window window)
    (select-window window)
    window))

#+run-example
(run-nodes-example)

