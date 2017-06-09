;; =========================================================================    
;;  (C) Copyright 2006 - 2008 
;;      Universidad Carlos III de Madrid
;;      Planning & Learning Group (PLG)
;; 
;; =========================================================================
;; 
;; This file is part of SAYPHI
;; 
;; 
;; SAYPHI is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; SAYPHI is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with SAYPHI.  If not, see <http://www.gnu.org/licenses/>.
;; ========================================================================
;; Author: Tomas de la Rosa 
;; Description: SAYPHI Experimenter Definitions
;; 
;; ========================================================================



(defparameter *training-standards*
  (list '(roller-static (learn-helpful-tilde *this-domain* *this-domain-file* *train-probs-prefix* 
			 :static-facts-p t 
			 :timeout *train-timeout*))
	'(roller-simple (learn-helpful-tilde *this-domain* *this-domain-file* *train-probs-prefix* 
			 :static-facts-p t :inrxplan-p nil :whole-tree nil
			 :timeout *train-timeout*))
	'(roller-incremental (al-learn-helpful *this-domain* *this-domain-file* *train-probs-prefix*
			      *test-probs-prefix*
			      :static-facts-p t :prob-per-iteration 5
			      :timeout *train-timeout*))
	'(delete-casebase (delete-casebase))
	'(cases (cbp-runset *this-domain* *this-domain-file* :store-case t :algorithm 'hc-bnb 
		 :timeout *train-timeout* :probs-prefix *train-probs-prefix*))
	'(utility-cases-ehc (cbp-runset *this-domain* *this-domain-file*
			     :runtype 'ehc-cbr-utility :use-case t :algorithm 'guided-ehc
			     :store-utility 'compute-cbp-utility :timeout *this-timeout* :probs-prefix *valid-probs-prefix*))
	'(utility-cases-hc (cbp-runset *this-domain* *this-domain-file*
			    :runtype 'ehc-cbr-utility :use-case t :algorithm 'cbr-hc
			    :store-utility 'compute-advise-utility :timeout *this-timeout* :probs-prefix *valid-probs-prefix*))
	'(refine-cases (cbr-refine-types *this-domain* *this-domain-file* *valid-probs-prefix* :timeout *train-timeout*))
	'(cases-incremental (incremental-cabala-learn *this-domain* *this-domain-file* *train-probs-prefix*
			      *test-probs-prefix* :prob-per-iteration 5 :timeout *train-timeout*))

	'(replica-examples  (relational-learning *this-domain* *this-domain-file* :timeout *train-timeout* 
			     :probs-prefix *train-probs-prefix*))
	'(replica-learning  (relational-algorithm *this-domain* *this-domain-file* 
			     :compute-matrix t :runs *replica-runs*))
	'(flare (learn-flare-tilde *this-domain* *this-domain-file* *train-probs-prefix* 
			 :static-facts-p t :prune-cases 5 :timeout *train-timeout*))


;; 	  '(replica (relational-learning))
	  ))


(defparameter *experiments-standards*
      (list
        '(ehc (runset *this-domain* *this-domain-file* 
 	       :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
	       :internal-save t :internal-reload t
	       :out-file (conc-outfile-tag "ehc")))
        '(hill-climbing (runset *this-domain* *this-domain-file* :algorithm 'hill-climbing
 	       :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
               :internal-save t :internal-reload t
	       :out-file (conc-outfile-tag "hill-climbing")))
	;;igual que hill-climbing solo cambio de nombre
        '(escalada (runset *this-domain* *this-domain-file* :algorithm 'hill-climbing
 	       :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
	       :out-file (conc-outfile-tag "escalada")))
        '(beam-search (runset *this-domain* *this-domain-file* :algorithm 'beam-search
 	       :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
               :internal-save t :internal-reload t
	       :out-file (conc-outfile-tag "beam-search")))
        '(a-star (runset *this-domain* *this-domain-file* :algorithm 'a-star
 	       :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
	       :out-file (conc-outfile-tag "a-star")))
	'(wa-star (runset *this-domain* *this-domain-file* :algorithm 'a-star :w_h 3
		   :internal-save t :internal-reload t
		  :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		  :out-file (conc-outfile-tag "wa-star")))
        '(dfbnb (runset *this-domain* *this-domain-file* :algorithm 'hc-bnb
 	       :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
	       :out-file (conc-outfile-tag "dfbnb")))
	'(depth-first (runset *this-domain* *this-domain-file* :algorithm 'depth-first
 	       :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
	       :out-file (conc-outfile-tag "depth-first")))
 	'(roller-policy (runset *this-domain* *this-domain-file* :algorithm 'roller-policy 
			  :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			  :out-file (conc-outfile-tag "roller-policy")))
 	'(roller-policy-ratio (runset *this-domain* *this-domain-file* :algorithm 'roller-hc-ratio 
			  :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			  :out-file (conc-outfile-tag "roller-policy")))
 	'(roller-policy-nh (runset *this-domain* *this-domain-file* :algorithm 'roller-policy :helpful nil
			  :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			  :out-file (conc-outfile-tag "roller-policy-nh")))
	'(roller-bfs-policy (runset *this-domain* *this-domain-file* :algorithm 'roller-bfs-policy :helpful nil
			    :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			    :out-file (conc-outfile-tag "roller-bfs-policy")))
	'(roller-ehc (runset *this-domain* *this-domain-file* 
		       :algorithm 'roller-ehc :search-options '((:roller-run :with-bindings))
		       :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		       :out-file (conc-outfile-tag "roller-ehc")))
	'(roller-lookahead (runset *this-domain* *this-domain-file* :algorithm 'roller-lookahead-ehc 
			    :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			    :out-file (conc-outfile-tag "roller-looka-ehc")))
	'(roller-lookahead-repair (runset *this-domain* *this-domain-file* :algorithm 'roller-lookahead-ehc 
				   :lookahead :lh-repair-rx
				   :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
				   :out-file (conc-outfile-tag "roller-looka-ehc-rep")))
	'(roller-lookahead-policy (runset *this-domain* *this-domain-file* :algorithm 'roller-lookahead-policy 
				   :lookahead t
				   :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
				   :out-file (conc-outfile-tag "roller-looka-policy")))
	'(roller-lookahead-policy-repair (runset *this-domain* *this-domain-file* :algorithm 'roller-lookahead-policy 
					  :lookahead :lh-repair-rx
					  :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
					  :out-file (conc-outfile-tag "roller-looka-policy-rep")))
	'(roller-lh-bfs (runset *this-domain* *this-domain-file* :algorithm 'roller-lookahead-bfs 
				   :lookahead t
				   :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
				   :out-file (conc-outfile-tag "roller-lh-bfs")))
	'(roller-lh-bfs-repair (runset *this-domain* *this-domain-file* :algorithm 'roller-lookahead-bfs 
				:lookahead :lh-repair-rx
				:timeout *this-timeout* :probs-prefix *test-probs-prefix* 
				:out-file (conc-outfile-tag "roller-lh-bfs-r")))
	'(roller-plh-bfs (runset *this-domain* *this-domain-file* :algorithm 'roller-lookahead-bfs 
			  :lookahead t :search-options '((:lh-inpath t))
			  :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			  :out-file (conc-outfile-tag "roller-plh-bfs")))
	'(roller-plh-bfs-repair (runset *this-domain* *this-domain-file* :algorithm 'roller-lookahead-bfs 
				 :lookahead :lh-repair-rx :search-options '((:lh-inpath t))
				 :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
				 :out-file (conc-outfile-tag "roller-plh-bfs-r")))

	'(flare-wbfs (runset *this-domain* *this-domain-file* :algorithm 'flare-lookahead-bfs 
		      :w_h 3 :helpful :bfs-helpful 
		      :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		      :out-file (conc-outfile-tag "flare-wbfs")))
	'(flare-wbfs-h (runset *this-domain* *this-domain-file* :algorithm 'flare-lookahead-bfs 
		      :w_h 3 :helpful :bfs-helpful :w_g 0
		      :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		      :out-file (conc-outfile-tag "flare-wbfs")))
	'(flare-play (runset *this-domain* *this-domain-file* :algorithm 'flare-play 
		      :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		      :out-file (conc-outfile-tag "flare-play")))
	
	'(say-yashp (runset *this-domain* *this-domain-file* :algorithm 'say-yahsp 
		   :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		   :out-file (conc-outfile-tag "say-yahsp")))
	'(la-play (runset *this-domain* *this-domain-file* :algorithm 'la-play 
		      :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		      :out-file (conc-outfile-tag "la-play")))
	'(lookahead-ehc (runset *this-domain* *this-domain-file* :lookahead t
 	       :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
	       :out-file (conc-outfile-tag "ehc-lookahead")))
	'(lookahead-bfs (runset *this-domain* *this-domain-file* :algorithm 'lookahead-bfs
			 :max-solutions 1
			 :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			 :out-file (conc-outfile-tag "lookahead-bfs")))
	'(lookahead-bfs-q (runset *this-domain* *this-domain-file* :algorithm 'lookahead-bfs
			   :search-options '((:lh-inpath t))
			   :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			   :out-file (conc-outfile-tag "lookahead-bfs-q")))
	'(lookahead-path-bfs (runset *this-domain* *this-domain-file* :algorithm 'lookahead-bfs
			 :search-options '((:lh-inpath t)) :max-solutions 1
			 :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			 :out-file (conc-outfile-tag "lookahead-path-bfs")))
;; 	'(lookahead-a-star (runset *this-domain* *this-domain-file* :algorithm 'a-star :w_h 3
;; 			    :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
;; 			    :out-file (conc-outfile-tag "lookahead-bfs")))
	'(la-wbfs (runset *this-domain* *this-domain-file* :algorithm 'a-star 
			  :w_h 3 :helpful :bfs-helpful :lookahead t
			  :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			  :out-file (conc-outfile-tag "lookahead-wbfs")))
	'(la-wbfs-h (runset *this-domain* *this-domain-file* :algorithm 'a-star 
			  :w_h 3 :helpful :bfs-helpful :lookahead t :w_g 0
			  :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			  :out-file (conc-outfile-tag "lookahead-wbfs-h")))




	'(hc-cbr (cbp-runset *this-domain* *this-domain-file* :algorithm 'cbr-hc 
		  :use-case t :timeout *this-timeout* :probs-prefix *test-probs-prefix* :save-result t
		  :out-file (conc-outfile-tag "hc-cbr")))
	'(ehc-cbr (cbp-runset *this-domain* *this-domain-file* :algorithm 'guided-ehc :save-result t
		  :use-case t :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		  :out-file (conc-outfile-tag "ehc-cbr")))
	'(beam-cbr (cbp-runset *this-domain* *this-domain-file* :algorithm 'guided-beamsearch 
		  :use-case t :timeout *this-timeout* :probs-prefix *test-probs-prefix* :save-result t
		  :out-file (conc-outfile-tag "beam-cbr")))
	'(cbsorted-ehc (cbp-runset *this-domain* *this-domain-file* :algorithm 'cbsorted-ehc :save-result t
		  :use-case t :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		  :out-file (conc-outfile-tag "cbsorted-ehc")))

	'(a-star-cbr (cbp-runset *this-domain* *this-domain-file* :algorithm 'guided-bfs :save-result t
		      :use-case t :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		      :out-file (conc-outfile-tag "astar-cbr")))
	'(wa-star-cbr (cbp-runset *this-domain* *this-domain-file* :algorithm 'wa-star-sorted :w_h 3 :save-result t
		       :use-case t :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		       :out-file (conc-outfile-tag "wastar-cbr")))
	'(a-star-dual-cbr (cbp-runset *this-domain* *this-domain-file* :algorithm 'guided-dual-bfs :save-result t
			    :use-case t :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			    :out-file (conc-outfile-tag "astar-dual-cbr")))
	'(wa-star-dual-cbr (cbp-runset *this-domain* *this-domain-file* :algorithm 'guided-dual-bfs :w_h 3 :save-result t
			    :use-case t :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			    :out-file (conc-outfile-tag "wastar-dual-cbr")))

	
	'(cabala-ehc (cbp-runset *this-domain* *this-domain-file* :algorithm 'cabala-ehc :save-result t
		      :use-case t :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		      :out-file (conc-outfile-tag "cabala-ehc")))
	'(cabala-r-ehc (cbp-runset *this-domain* *this-domain-file* :algorithm 'cabala-ehc :lookahead :lh-repair :save-result t
			:use-case t :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			:out-file (conc-outfile-tag "cabala-r-ehc")))
	
	'(cabala-bfs (cbp-runset *this-domain* *this-domain-file* :algorithm 'cabala-bfs  :save-result t
		      :max-solutions 1 
		      :use-case t :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		      :out-file (conc-outfile-tag "cabala-bfs")))
	'(cabala-bfs-q (cbp-runset *this-domain* *this-domain-file* :algorithm 'cabala-bfs :save-result t
			:search-options '((:lh-inpath t))
			:use-case t :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
			:out-file (conc-outfile-tag "cabala-bfs-q")))
;; 	'(cabala-a-star (cbp-runset *this-domain* *this-domain-file* :algorithm 'cabala-a-star :save-result t :w_h 3
;; 			 :use-case t :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
;; 			 :out-file (conc-outfile-tag "cabala-bfs")))
	'(cabala-wbfs (cbp-runset *this-domain* *this-domain-file* :algorithm 'cabala-a-star :save-result t 
			 :w_h 3 :helpful :bfs-helpful
			 :use-case t :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		       :out-file (conc-outfile-tag "cabala-wbfs")))


	

	'(replica (relational-test *this-domain* *this-domain-file* 
		   :timeout *this-timeout* :probs-prefix *test-probs-prefix* 
		   :out-prefix (conc-outfile-tag "replica")))
	
	;;EXTERNAL CALLS TO PLANNERS IN /EXTERNAL
	'(ff (external-runset *this-domain* :domain-file *this-domain-file* :planner *ff* 
	      :timeout *this-timeout* :probs-prefix *test-probs-prefix*))
	'(lpg (external-runset *this-domain* :domain-file *this-domain-file* :planner *lpg* 
	      :timeout *this-timeout* :probs-prefix *test-probs-prefix*))
	'(sgplan (external-runset *this-domain* :domain-file *this-domain-file* :planner *sgp* 
	      :timeout *this-timeout* :probs-prefix *test-probs-prefix*))
	'(yahsp (external-runset *this-domain* :domain-file *this-domain-file* :planner *yahsp* 
	      :timeout *this-timeout* :probs-prefix *test-probs-prefix*))
	'(sayphi (external-runset *this-domain* :domain-file *this-domain-file* :planner *sayphi* 
		 :timeout *this-timeout* :probs-prefix *test-probs-prefix*))
	'(sayexe-yahsp (external-runset *this-domain* :domain-file *this-domain-file* :planner *say-yahsp* 
		 :timeout *this-timeout* :probs-prefix *test-probs-prefix*))
	'(sayexe-laplay (external-runset *this-domain* :domain-file *this-domain-file* :planner *say-laplay* 
		 :timeout *this-timeout* :probs-prefix *test-probs-prefix*))
	'(roller (external-runset *this-domain* :domain-file *this-domain-file* :planner *roller* 
		  :timeout *this-timeout* :probs-prefix *test-probs-prefix*))
	'(ff-lhbfs (external-runset *this-domain* :domain-file *this-domain-file* :planner *lh-bfs* 
		    :timeout *this-timeout* :probs-prefix *test-probs-prefix*))
	'(lama (external-runset *this-domain* :domain-file *this-domain-file* :planner *lama* 
		:timeout *this-timeout* :probs-prefix *test-probs-prefix*))

	'(ff-roller (external-runset *this-domain* :domain-file *this-domain-file* :planner *ff-roller* 
		  :timeout *this-timeout* :probs-prefix *test-probs-prefix* :knowledge-path *this-knowledge-path*
		     :reload-results-p nil))

	'(ffd-ehc (external-runset *this-domain* :domain-file *this-domain-file* :planner *ff-hdiff* 
		      :timeout *this-timeout* :probs-prefix *test-probs-prefix* :reload-results-p nil
		      :rename-tag "FFHDIFF-EHC" :update-options '("2" "0")))
	'(ffd-ehc-hdiff (external-runset *this-domain* :domain-file *this-domain-file* :planner *ff-hdiff* 
		      :timeout *this-timeout* :probs-prefix *test-probs-prefix* :reload-results-p nil
		      :rename-tag "FFHDIFF-EHC-HDIFF" :update-options '("2" "1")))
	'(ffd-ehc-diffop (external-runset *this-domain* :domain-file *this-domain-file* :planner *ff-hdiff* 
		      :timeout *this-timeout* :probs-prefix *test-probs-prefix* :reload-results-p nil
		      :rename-tag "FFHDIFF-EHC-DIFFOP" :update-options '("2" "2")))
	'(ffd-hc (external-runset *this-domain* :domain-file *this-domain-file* :planner *ff-hdiff* 
		      :timeout *this-timeout* :probs-prefix *test-probs-prefix* :reload-results-p nil
		      :rename-tag "FFHDIFF-HC" :update-options '("11" "0")))
	'(ffd-hc-hdiff (external-runset *this-domain* :domain-file *this-domain-file* :planner *ff-hdiff* 
		      :timeout *this-timeout* :probs-prefix *test-probs-prefix* :reload-results-p nil
		      :rename-tag "FFHDIFF-HC-HDIFF" :update-options '("11" "1")))
	'(ffd-hc-diffop (external-runset *this-domain* :domain-file *this-domain-file* :planner *ff-hdiff* 
		      :timeout *this-timeout* :probs-prefix *test-probs-prefix* :reload-results-p nil
		      :rename-tag "FFHDIFF-HC-DIFFOP" :update-options '("11" "2")))
	'(ffd-wbfs (external-runset *this-domain* :domain-file *this-domain-file* :planner *ff-hdiff* 
		      :timeout *this-timeout* :probs-prefix *test-probs-prefix* :reload-results-p nil
		      :rename-tag "FFHDIFF-WBFS" :update-options '("3" "0")))
	'(ffd-wbfs-hdiff (external-runset *this-domain* :domain-file *this-domain-file* :planner *ff-hdiff* 
		      :timeout *this-timeout* :probs-prefix *test-probs-prefix* :reload-results-p nil
		      :rename-tag "FFHDIFF-WBFS-HDIFF" :update-options '("3" "1")))
	'(ffd-wbfs-diffop (external-runset *this-domain* :domain-file *this-domain-file* :planner *ff-hdiff* 
		      :timeout *this-timeout* :probs-prefix *test-probs-prefix* :reload-results-p nil
		      :rename-tag "FFHDIFF-WBFS-DIFFOP" :update-options '("3" "2")))

	))


(defparameter *result-output-standards*
  (list '(num-problems-solved (conc-outfile-tag "_res-probsolved"))
	'(data-toplot 'normal)
	'(data-toplot 'accumulated)
	'(score-write-results)
	'(plot-scripts-all)
	'(distribution-results 'runtime)
	'(distribution-results 'length)
	'(distribution-results 'evaluated)
	'(plot-field-distribution 'runtime :spanish-label t)
	'(plot-field-distribution 'length :logscale-p nil :spanish-label t)
	'(plot-field-distribution 'evaluated :spanish-label t)
;; 	'(plot-field-distribution 'runtime)
;; 	'(plot-field-distribution 'length :logscale-p nil)
;; 	'(plot-field-distribution 'evaluated)



))


(defun setexperiment-train (test-list)
  (mapcar #'(lambda (test)
	      (cadr (assoc test *training-standards*))) 
	  test-list))


(defun setexperiment-test (test-list)
  (mapcar #'(lambda (test)
	      (let ((test-parameters (assoc test *experiments-standards*)))
		(list `(quote ,(car test-parameters)) (cadr test-parameters)))) 
	  test-list))


(defparameter *void-exp-default*
  '(runset *this-domain* *this-domain-file* 
	     :timeout *this-timeout* :probs-prefix *test-probs-prefix*))

(defparameter *cbp-exp-default*
  '(cbp-runset *this-domain* *this-domain-file* 
	     :timeout *this-timeout* :probs-prefix *test-probs-prefix*))



(defun create-experiment-test (test-tag &rest params)
  (let* ((i-varname `(quote ,test-tag)))
    (list i-varname (append *void-exp-default*
			    params 
			    (list :out-file (list 'conc-outfile-tag (format nil "~a" test-tag)))))))
	 

(defun create-cb-experiment-test (test-tag &rest params)
  (let* ((i-varname `(quote ,test-tag)))
    (list i-varname (append *cbp-exp-default*
			    params 
			    (list :out-file (list 'conc-outfile-tag (format nil "~a" test-tag)))))))


(defparameter *replica-exp-default*
  '(relational-test *this-domain* *this-domain-file* 
	     :timeout *this-timeout* :probs-prefix *test-probs-prefix*))

  
(defun insert-experiment-replica (&key (runs 10))
  (let ((exp-params nil))
    (dotimes (i runs (reverse exp-params))
      (let* ((i-expvar (intern (format nil "REPLICA~a" i)))
	     (i-varname `(quote ,i-expvar))
	     (i-replica-test (append *replica-exp-default* 
				     (list :classifier i
					   :out-prefix (list 'conc-outfile-tag  (format nil "replica~a" i))))))
;; 	(format t "~%~a" i-replica-test)
;; 	(push (list `(quote ,i-expvar ,i-replica-test)) exp-params)
 	(push (list i-varname i-replica-test) exp-params)
	))))
