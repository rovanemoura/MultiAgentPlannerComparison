;/* example-start tree tree.c */
;
;#include <gtk/gtk.h>
;
;/* for all the GtkItem:: and GtkTreeItem:: signals */
;static void cb_itemsignal( GtkWidget *item,
;			    gchar     *signame )
;{
;  gchar *name;
;  GtkLabel *label;
;
;  /* It's a Bin, so it has one child, which we know to be a
;     label, so get that */
;  label = GTK_LABEL (GTK_BIN (item)->child);
;  /* Get the text of the label */
;  gtk_label_get (label, &name);
;  /* Get the level of the tree which the item is in */
;  g_print ("%s called for item %s->%p, level %d\n", signame, name,
;	    item, GTK_TREE (item->parent)->level);
;}
;
;/* Note that this is never called */
;static void cb_unselect_child( GtkWidget *root_tree,
;				GtkWidget *child,
;				GtkWidget *subtree )
;{
;  g_print ("unselect_child called for root tree %p, subtree %p, child %p\n",
;	    root_tree, subtree, child);
;}
;
;/* Note that this is called every time the user clicks on an item,
;   whether it is already selected or not. */
;static void cb_select_child (GtkWidget *root_tree, GtkWidget *child,
;			      GtkWidget *subtree)
;{
;  g_print ("select_child called for root tree %p, subtree %p, child %p\n",
;	    root_tree, subtree, child);
;}
;
;static void cb_selection_changed( GtkWidget *tree )
;{
;  GList *i;
;  
;  g_print ("selection_change called for tree %p\n", tree);
;  g_print ("selected objects are:\n");
;
;  i = GTK_TREE_SELECTION(tree);
;  while (i){
;    gchar *name;
;    GtkLabel *label;
;    GtkWidget *item;
;
;    /* Get a GtkWidget pointer from the list node */
;    item = GTK_WIDGET (i->data);
;    label = GTK_LABEL (GTK_BIN (item)->child);
;    gtk_label_get (label, &name);
;    g_print ("\t%s on level %d\n", name, GTK_TREE
;	      (item->parent)->level);
;    i = i->next;
;  }
;}
;
;int main( int   argc,
;	   char *argv[] )
;{
;  GtkWidget *window, *scrolled_win, *tree;
;  static gchar *itemnames[] = {"Foo", "Bar", "Baz", "Quux",
;				"Maurice"};
;  gint i;
;
;  gtk_init (&argc, &argv);
;
;  /* a generic toplevel window */
;  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;  gtk_signal_connect (GTK_OBJECT(window), "delete_event",
;		       GTK_SIGNAL_FUNC (gtk_main_quit), NULL);
;  gtk_container_set_border_width (GTK_CONTAINER(window), 5);
;
;  /* A generic scrolled window */
;  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
;  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
;				   GTK_POLICY_AUTOMATIC,
;				   GTK_POLICY_AUTOMATIC);
;  gtk_widget_set_usize (scrolled_win, 150, 200);
;  gtk_container_add (GTK_CONTAINER(window), scrolled_win);
;  gtk_widget_show (scrolled_win);
;  
;  /* Create the root tree */
;  tree = gtk_tree_new();
;  g_print ("root tree is %p\n", tree);
;  /* connect all GtkTree:: signals */
;  gtk_signal_connect (GTK_OBJECT(tree), "select_child",
;		       GTK_SIGNAL_FUNC(cb_select_child), tree);
;  gtk_signal_connect (GTK_OBJECT(tree), "unselect_child",
;		       GTK_SIGNAL_FUNC(cb_unselect_child), tree);
;  gtk_signal_connect (GTK_OBJECT(tree), "selection_changed",
;		       GTK_SIGNAL_FUNC(cb_selection_changed), tree);
;  /* Add it to the scrolled window */
;  gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW(scrolled_win),
;					  tree);
;  /* Set the selection mode */
;  gtk_tree_set_selection_mode (GTK_TREE(tree),
;				GTK_SELECTION_MULTIPLE);
;  /* Show it */
;  gtk_widget_show (tree);
;
;  for (i = 0; i < 5; i++){
;    GtkWidget *subtree, *item;
;    gint j;
;
;    /* Create a tree item */
;    item = gtk_tree_item_new_with_label (itemnames[i]);
;    /* Connect all GtkItem:: and GtkTreeItem:: signals */
;    gtk_signal_connect (GTK_OBJECT(item), "select",
;			 GTK_SIGNAL_FUNC(cb_itemsignal), "select");
;    gtk_signal_connect (GTK_OBJECT(item), "deselect",
;			 GTK_SIGNAL_FUNC(cb_itemsignal), "deselect");
;    gtk_signal_connect (GTK_OBJECT(item), "toggle",
;			 GTK_SIGNAL_FUNC(cb_itemsignal), "toggle");
;    gtk_signal_connect (GTK_OBJECT(item), "expand",
;			 GTK_SIGNAL_FUNC(cb_itemsignal), "expand");
;    gtk_signal_connect (GTK_OBJECT(item), "collapse",
;			 GTK_SIGNAL_FUNC(cb_itemsignal), "collapse");
;    /* Add it to the parent tree */
;    gtk_tree_append (GTK_TREE(tree), item);
;    /* Show it - this can be done at any time */
;    gtk_widget_show (item);
;    /* Create this item's subtree */
;    subtree = gtk_tree_new();
;    g_print ("-> item %s->%p, subtree %p\n", itemnames[i], item,
;	      subtree);
;
;    /* This is still necessary if you want these signals to be called
;	for the subtree's children.  Note that selection_change will be 
;	signalled for the root tree regardless. */
;    gtk_signal_connect (GTK_OBJECT(subtree), "select_child",
;			 GTK_SIGNAL_FUNC(cb_select_child), subtree);
;    gtk_signal_connect (GTK_OBJECT(subtree), "unselect_child",
;			 GTK_SIGNAL_FUNC(cb_unselect_child), subtree);
;    /* This has absolutely no effect, because it is completely ignored 
;	in subtrees */
;    gtk_tree_set_selection_mode (GTK_TREE(subtree),
;				  GTK_SELECTION_SINGLE);
;    /* Neither does this, but for a rather different reason - the
;	view_mode and view_line values of a tree are propagated to
;	subtrees when they are mapped.  So, setting it later on would
;	actually have a (somewhat unpredictable) effect */
;    gtk_tree_set_view_mode (GTK_TREE(subtree), GTK_TREE_VIEW_ITEM);
;    /* Set this item's subtree - note that you cannot do this until
;	AFTER the item has been added to its parent tree! */
;    gtk_tree_item_set_subtree (GTK_TREE_ITEM(item), subtree);
;
;    for (j = 0; j < 5; j++){
;      GtkWidget *subitem;
;
;      /* Create a subtree item, in much the same way */
;      subitem = gtk_tree_item_new_with_label (itemnames[j]);
;      /* Connect all GtkItem:: and GtkTreeItem:: signals */
;      gtk_signal_connect (GTK_OBJECT(subitem), "select",
;			   GTK_SIGNAL_FUNC(cb_itemsignal), "select");
;      gtk_signal_connect (GTK_OBJECT(subitem), "deselect",
;			   GTK_SIGNAL_FUNC(cb_itemsignal), "deselect");
;      gtk_signal_connect (GTK_OBJECT(subitem), "toggle",
;			   GTK_SIGNAL_FUNC(cb_itemsignal), "toggle");
;      gtk_signal_connect (GTK_OBJECT(subitem), "expand",
;			   GTK_SIGNAL_FUNC(cb_itemsignal), "expand");
;      gtk_signal_connect (GTK_OBJECT(subitem), "collapse",
;			   GTK_SIGNAL_FUNC(cb_itemsignal), "collapse");
;      g_print ("-> -> item %s->%p\n", itemnames[j], subitem);
;      /* Add it to its parent tree */
;      gtk_tree_append (GTK_TREE(subtree), subitem);
;      /* Show it */
;      gtk_widget_show (subitem);
;    }
;  }
;
;  /* Show the window and loop endlessly */
;  gtk_widget_show (window);
;  gtk_main();
;  return 0;
;}
;/* example-end */


(defpackage "13.06-tree" (:use :excl :common-lisp))
(in-package "13.06-tree")

(ff:defun-foreign-callable cb-itemsignal ((item (* gtk:GtkWidget))
					  (signame (* gtk:gchar)))
  (let ((name (ff:allocate-fobject '(* gtk:gchar)
				   :foreign-static-gc))
	(label nil))
    
    (setq label (gtk:GTK_LABEL
		 (ff:fslot-value-typed 'gtk:GtkBin nil
				       (gtk:GTK_BIN item) 'gtk::child)))
    (gtk:gtk_label_get label name)
    (format t "~s called for item ~s->#x~x, level ~d~%"
	    (native-to-string signame
			      :external-format gtk:gpointer-to-string-ef)
	    (native-to-string
	     (ff:fslot-address-typed '(* gtk:gchar) nil name 0))
	    item
	    (ff:fslot-value-typed
	     'gtk:GtkTree nil
	     (gtk:GTK_TREE (ff:fslot-value-typed 'gtk:GtkWidget nil
						 item 'gtk::parent))
	     'gtk::level))))

(ff:defun-foreign-callable cb-unselect-child ((root-tree (* gtk:GtkWidget))
					      (child (* gtk:GtkWidget))
					      (subtree (* gtk:GtkWidget)))
  (format t "~
unselect_child called for root tree #x~x, subtree #x~x, child #x~x~%"
	  root-tree subtree child))

(ff:defun-foreign-callable cb-select-child ((root-tree (* gtk:GtkWidget))
					    (child (* gtk:GtkWidget))
					    (subtree (* gtk:GtkWidget)))
  (format t "~
select_child called for root tree #x~x, subtree #x~x, child #x~x~%"
	  root-tree subtree child))

(ff:defun-foreign-callable cb-selection-changed ((tree (* gtk:GtkWidget)))
  (let ((i nil))
    (format t "selection_change called for tree #x~x~%" tree)
    (format t"selected objects are:~%")

    (setq i (gtk:GTK_TREE_SELECTION tree))
    (loop
      (when (eql gtk:NULL i)
	(return))
      (let ((name (ff:allocate-fobject '(* gtk:gchar)
				       :foreign-static-gc))
	    (label nil)
	    (item nil))
	(setq item (gtk:GTK_WIDGET
		    (ff:fslot-value-typed 'gtk:GList nil
					  i 'gtk::data)))
	(setq label (gtk:GTK_LABEL
		     (ff:fslot-value-typed 'gtk:GtkBin nil
					   (gtk:GTK_BIN item) 'gtk::child)))
	(gtk:gtk_label_get label name)
	(format t "~t~s on level ~d~%"
		(native-to-string
		 (ff:fslot-address-typed '(* gtk:gchar) nil name 0))
		(ff:fslot-value-typed
		 'gtk:GtkTree nil
		 (gtk:GTK_TREE
		  (ff:fslot-value-typed 'gtk:GtkWidget nil item 'gtk::parent))
		 'gtk::level))
	(setq i (ff:fslot-value-typed 'gtk:GList nil i 'gtk::next))))))

(ff:defun-foreign-callable cb-gtk-main-quit ()
  (gtk:gtk-main-quit))

(defun tree ()
  (let ((window nil)
	(scrolled-win nil)
	(tree nil)
	(itemnames '#("Foo" "Bar" "Baz" "Quux" "Maurice")))
    (gtk:gtk_init 0 0) 

    (setq window  (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "delete_event"
			    (gtk:GTK_SIGNAL_FUNC
			     #+original (ff:get-entry-point "gtk_main_quit")
			     #-original (ff:register-foreign-callable
					 'cb-gtk-main-quit))
			    gtk:NULL)
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 5)

    (setq scrolled-win (gtk:gtk_scrolled_window_new gtk:NULL gtk:NULL))
    (gtk:gtk_scrolled_window_set_policy (gtk:GTK_SCROLLED_WINDOW scrolled-win) 
					gtk:GTK_POLICY_AUTOMATIC 
					gtk:GTK_POLICY_AUTOMATIC)
    (gtk:gtk_widget_set_usize scrolled-win 150 200)
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) scrolled-win)
    (gtk:gtk_widget_show scrolled-win)

    (setq tree (gtk:gtk_tree_new))
    (format t "root tree is #x~x~%" tree)

    (gtk:gtk_signal_connect (gtk:GTK_OBJECT tree) "select_child" 
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable 'cb-select-child))
			    tree)
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT tree) "unselect_child" 
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable 'cb-unselect-child))
			    tree)
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT tree) "selection_changed" 
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable
			      'cb-selection-changed))
			    tree)

    (gtk:gtk_scrolled_window_add_with_viewport
     (gtk:GTK_SCROLLED_WINDOW scrolled-win) 
     tree)

    (gtk:gtk_tree_set_selection_mode (gtk:GTK_TREE tree) 
				     gtk:GTK_SELECTION_MULTIPLE)

    (gtk:gtk_widget_show tree)

    (dotimes (i 5)
      (let ((subtree nil)
	    (item nil))

	(setq item (gtk:gtk_tree_item_new_with_label
		    (aref itemnames i)))

	(gtk:gtk_signal_connect (gtk:GTK_OBJECT item) "select" 
				(gtk:GTK_SIGNAL_FUNC
				 (ff:register-foreign-callable 'cb-itemsignal))
				"select")
	(gtk:gtk_signal_connect (gtk:GTK_OBJECT item) "deselect" 
				(gtk:GTK_SIGNAL_FUNC
				 (ff:register-foreign-callable 'cb-itemsignal))
				"deselect")
	(gtk:gtk_signal_connect (gtk:GTK_OBJECT item) "toggle" 
				(gtk:GTK_SIGNAL_FUNC
				 (ff:register-foreign-callable 'cb-itemsignal))
				"toggle")
	(gtk:gtk_signal_connect (gtk:GTK_OBJECT item) "expand" 
				(gtk:GTK_SIGNAL_FUNC
				 (ff:register-foreign-callable 'cb-itemsignal))
				"expand")
	(gtk:gtk_signal_connect (gtk:GTK_OBJECT item) "collapse" 
				(gtk:GTK_SIGNAL_FUNC
				 (ff:register-foreign-callable 'cb-itemsignal))
				"collapse")

	(gtk:gtk_tree_append (gtk:GTK_TREE tree) item)

	(gtk:gtk_widget_show item)

	(setq subtree (gtk:gtk_tree_new))
	(format t "-> item ~s->#x~x  subtree #x~x~%"
		(aref itemnames i) item subtree)

	(gtk:gtk_signal_connect (gtk:GTK_OBJECT subtree) "select_child" 
				(gtk:GTK_SIGNAL_FUNC
				 (ff:register-foreign-callable
				  'cb-select-child))
				subtree)
	(gtk:gtk_signal_connect (gtk:GTK_OBJECT subtree) "unselect_child" 
				(gtk:GTK_SIGNAL_FUNC
				 (ff:register-foreign-callable
				  'cb-unselect-child))
				subtree)

	(gtk:gtk_tree_set_selection_mode (gtk:GTK_TREE subtree) 
					 gtk:GTK_SELECTION_SINGLE)

	(gtk:gtk_tree_set_view_mode (gtk:GTK_TREE subtree)
				    gtk:GTK_TREE_VIEW_ITEM)

	(gtk:gtk_tree_item_set_subtree (gtk:GTK_TREE_ITEM item) subtree)

	(dotimes (j 5)
	  (let ((subitem nil))

	    (setq subitem (gtk:gtk_tree_item_new_with_label
			   (aref itemnames j)))
	    (gtk:gtk_signal_connect (gtk:GTK_OBJECT subitem) "select" 
				    (gtk:GTK_SIGNAL_FUNC
				     (ff:register-foreign-callable
				      'cb-itemsignal))
				    "select")
	    (gtk:gtk_signal_connect (gtk:GTK_OBJECT subitem) "deselect" 
				    (gtk:GTK_SIGNAL_FUNC
				     (ff:register-foreign-callable
				      'cb-itemsignal))
				    "deselect")
	    (gtk:gtk_signal_connect (gtk:GTK_OBJECT subitem) "toggle" 
				    (gtk:GTK_SIGNAL_FUNC
				     (ff:register-foreign-callable
				      'cb-itemsignal))
				    "toggle")
	    (gtk:gtk_signal_connect (gtk:GTK_OBJECT subitem) "expand" 
				    (gtk:GTK_SIGNAL_FUNC
				     (ff:register-foreign-callable
				      'cb-itemsignal))
				    "expand")
	    (gtk:gtk_signal_connect (gtk:GTK_OBJECT subitem) "collapse" 
				    (gtk:GTK_SIGNAL_FUNC
				     (ff:register-foreign-callable
				      'cb-itemsignal))
				    "collapse")
	    (format t "-> -> item ~s->#x~x~%" (aref itemnames j) subitem)

	    (gtk:gtk_tree_append (gtk:GTK_TREE subtree) subitem)
	    (gtk:gtk_widget_show subitem)))))

    (gtk:gtk_widget_show window)
    #+original (gtk:gtk_main)
    #-original (gtk:gtk-main)))


(flet ((run-example (name function)
	 ;; workaround for bogus (imo) redef. warnings generated by defvar
	 (declare (special gtk::*run-example*))
	 (unless (boundp 'gtk::*run-example*)
	   (setq gtk::*run-example* t))
	 (when gtk::*run-example*
	   (mp:process-run-function
	    (format nil "GTK+ Example: ~a" name)
	    function))))
  (run-example "13.06-tree" #'tree))
