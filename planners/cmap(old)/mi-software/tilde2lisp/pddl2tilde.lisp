(in-package "COMMON-LISP-USER")

(asdf:oos 'asdf:load-op "cl-ppcre" :force t)
;; (require "cl-ppcre")

;; tilde-goal can be either: classify or regression
;; mode can be (for now): control-rules or policies
(defun write-to-tilde (examples-file domain domain-file tilde-goal mode odir)
  (declare (special *pspace*))
  (say-domain domain domain-file)
  (let ((predicates (dom-predicates *pspace*))
	(actions (dom-actions *pspace*))
	(types (generic-all-types))
	(actions-dir (concatenate 'string odir "actions/"))
	(bindings-dir ""))
    (ensure-directories-exist odir)
    (ensure-directories-exist actions-dir)
    (dolist (action actions)
      (ensure-directories-exist (format nil "~abindings-~(~a~)/" odir (action-name action))))
    (dolist (action actions)
      (setq bindings-dir (format nil "~abindings-~(~a~)/" odir (action-name action)))
      (if (eq mode 'control-rules) (write-tilde-actions tilde-goal mode actions predicates types domain actions-dir))
      (write-tilde-bindings tilde-goal mode (remove-dash (action-name action)) (action-parameters action) predicates types domain bindings-dir)
      (funcall (if (eq mode 'policies) #'write-tilde-policies-examples #'write-tilde-control-rules-examples) tilde-goal examples-file domain actions-dir odir))))

(defun write-tilde-actions (tilde-goal mode actions predicates types domain odir)
  (with-open-file (ostream (format nil "~a~(~a~)-select-action.kb" odir domain) :direction :output :if-exists :supersede :if-does-not-exist :create))
  (with-open-file (ostream (format nil "~a~(~a~)-select-action.s" odir domain) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format ostream "%%*****************************************************************
%%  File Automatically generated with the pddl2tilde program 
%%  To learn the action to apply in domain ~a
%% Consider the use of lookahead for recursive predicates!!
%%*****************************************************************
% Tilde Options" domain)
    (format ostream "~%tilde_mode(~(~a~)).
typed_language(yes).
output_options([c45,lp,prolog]).

% The target concept" tilde-goal)
    ;; output_options([c45,c45c,c45e,lp,prolog_probab]).
    (format ostream "~%predict(select_action(+Example,-Class)).")
    (format ostream "~%classes([~(~a~)" (remove-dash (action-name (car actions))))
    (dolist (action (cdr actions))
      (format ostream ",~a" (remove-dash (action-name action))))
    (format ostream "]).")
    (format ostream "~%type(select_action(example,class)).")
  (write-tilde-predicates predicates types mode ostream)))

(defun write-tilde-predicates (predicates types mode ostream)
  (format ostream "~2%% The domain predicates")
  (maphash #'(lambda (pred-name args)
	       (format ostream "~2%rmode(~(true_in_state_~a~)(+Example" (remove-dash pred-name))
	       (dolist (arg args)
		 (format ostream ",+-~:(~a~)" (remove-dash (car arg))))
	       (format ostream ")).~%type(true_in_state_~(~a~)(example" (remove-dash pred-name))
	       (dolist (arg args)
		 (format ostream ",~(~a~)" (remove-dash (cdr arg))))
	       (format ostream "))." (remove-dash pred-name))
	       (format ostream "~%rmode(target_goal_~(~a~)(+Example" (remove-dash pred-name))
	       (dolist (arg args)
		 (format ostream ",+-~:(~a~)" (remove-dash (car arg))))
	       (format ostream ")).~%type(~(target_goal_~a~)(example" (remove-dash pred-name))
	       (dolist (arg args)
		 (format ostream ",~(~a~)" (remove-dash (cdr arg))))
	       (format ostream "))." (remove-dash pred-name)))
	   predicates)
  (dolist (type types)
    (format ostream "~%rmode(~(type_of_object_~a~)(+Example,+Object))." type)
    (format ostream "~%type(~(type_of_object_~a~)(example,object))." type))
  (format ostream "~2%% The ACE commands
execute(tilde).
execute(quit).
"))

(defun write-tilde-bindings (tilde-goal mode action-name parameters predicates types domain odir)
  (let ((target-concept-prefix (if (eq mode 'policies) 'execution 'select_bindings)))
    (with-open-file (ostream (format nil "~a~(~a-select-bindings-~a~).kb" odir domain action-name) :direction :output :if-exists :supersede :if-does-not-exist :create))
    (with-open-file (ostream (format nil "~a~(~a-select-bindings-~a~).s" odir domain action-name) :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format ostream "%%*****************************************************************
%%  File Automatically generated with the pddl2tilde program 
%%  To learn the success patterns of action ~a
%%*****************************************************************
% Tilde Options" action-name)
      (format ostream "~%tilde_mode(~(~a~)).
typed_language(yes).
output_options([c45,lp,prolog]).

% The target concept" tilde-goal)
      ;; output_options([c45,c45c,c45e,lp,prolog_probab]).
      (cond ((eq tilde-goal 'classify)
	     (format ostream "~%predict(~(~a_~a~)(+Example" target-concept-prefix action-name)
	     (dolist (parameter parameters)
	       (format ostream ",+~a" (remove-dash (car parameter))))
	     (format ostream ",-Class)).")
	     (format ostream "~%classes([selected,rejected]).")
	     (format ostream "~%type(~(~a_~a~)(example" target-concept-prefix action-name)
	     (dolist (parameter parameters)
	       (format ostream ",~a" (remove-dash (cdr parameter))))
	     (format ostream ",class))."))
	    (t (format ostream "~%predict(exec_time_~(~a~)(+example,-number))." action-name)
	       (format ostream "~%type(exec_time_~(~a~)(example,number))." action-name)))
      (write-tilde-predicates predicates types mode ostream))))

;; If mode=policies examples are of the forms:
;; classify: (deliberative/reactive plan/execute state1 action success/failure/dead-end)
;; regression: (deliberative/reactive plan/execute state1 action state2)
(defun write-tilde-policies-examples (tilde-goal examples-file domain actions-dir bindings-dir)
  (declare (ignore bindings-dir))
  (with-open-file (istream examples-file :direction :input)
    (do* ((example (read istream nil 'eof)
		   (read istream nil 'eof))
	  (endp (eq example 'eof) (eq example 'eof))
	  (state (cadr example) (if (not endp) (cadr example)))
	  (action (caddr example) (if (not endp) (caddr example)))
	  (i 0 (1+ i)))
	(endp)
      (if (eq (car example) 'deliberative)
	  (with-open-file (ostream (format nil "~a~(~a~).kb" odir (car action)) :direction :output :if-exists :append :if-does-not-exist :create)
	    (format ostream "~2%%% Example ~d" i)
	    (if (eq tilde-goal 'classify)
		(format ostream "~%execution_~(~a~)(e~d" (remove-dash (car action)) i)
		(format ostream "~%exec_time_~(~a~)(e~d" (remove-dash (car action)) i))
	    (dolist (parameter (cdr action))
	      (format ostream ",+~a" (remove-dash parameter)))
	    (if (eq tilde-goal 'classify)
		(format ostream ",~(~a~))." (remove-dash (nth 4 example)))
		(format ostream ",~,3,,f)." (some #'(lambda (literal)
						      (if (and (eq (car literal) '=)
							       (eq (caadr literal) 'exec-time))
							  (caddr literal)))
						  (nth 4 example))))
	    (dolist (literal state)
	      (unless (eq (car literal) '=)
		(format ostream "~%~a(e~d" (remove-dash (car literal)) i)
		(dolist (arg (cdr literal))
		  (format ostream ",~(~a~)" (remove-dash arg)))
		(format ostream ")."))))))))

;; If mode=control-rules examples are of the forms:
;; classify: (example-number positivep preconds effects)
(defun write-tilde-control-rules-examples (tilde-goal examples-file domain actions-dir bindings-dir)
  (with-open-file (istream examples-file :direction :input)
    (do* ((example (read istream nil 'eof)
		   (read istream nil 'eof))
	  (endp (eq example 'eof) (eq example 'eof))
	  (i (car example) (if (not endp) (car example)))
	  (positivep (nth 1 example) (if (not endp) (nth 1 example)))
	  (meta-state (nth 2 example) (if (not endp) (nth 2 example)))
	  (effects (nth 2 (nth 3 example)) (if (not endp) (nth 2 (nth 3 example)))))
	 (endp)
      (if positivep
	  (with-open-file (ostream (format nil "~a~(~a~)-select-action.kb" actions-dir domain) :direction :output :if-exists :append :if-does-not-exist :create)
	    (format ostream "~%%% Example ~d" i)
	    (format ostream "~%select_action~((e~d,~a~))." i (remove-dash (car effects)))
	    (write-tilde-example i meta-state effects ostream)
	    (terpri ostream)))
      (with-open-file (ostream (format nil "~a~(bindings-~a/~a-select-bindings-~a~).kb" bindings-dir (car effects) domain (car effects))
			       :direction :output :if-exists :append :if-does-not-exist :create)
	(format ostream "~%%% Example ~d" i)
	(format ostream "~%select_bindings_~(~a~)(e~d" (remove-dash (car effects)) i)
	(dolist (parameter (cdr effects))
	  (format ostream ",~a" (remove-dash parameter)))
	(format ostream ",~(~a~))." (if positivep 'selected 'rejected))
	(write-tilde-example i meta-state effects ostream)
	(terpri ostream)))))

(defun write-tilde-example (i meta-state effects ostream)
  (dolist (literal meta-state)
    (case (car literal)
      (= nil)
      ((true-in-state target-goal)
       (format ostream "~%~(~a_~a~)(e~d" (remove-dash (car literal)) (remove-dash (caadr literal)) i)
       (dolist (arg (cdadr literal))
	(format ostream ",~(~a~)" (remove-dash arg)))
      (format ostream ")."))
      (type-of-object
       (format ostream "~%~(type_of_object_~a(e~d,~a)~)." (remove-dash (caddr literal)) i (remove-dash (cadr literal))))
      (otherwise nil))))

(defun remove-dash (name)
  (cl-ppcre:regex-replace-all "-" (format nil "~(~a~)" name) "_"))

(defun remove-underscore (name)
  (cl-ppcre:regex-replace-all "_" (format nil "~a" name) "-"))

