(in-package "COMMON-LISP-USER")

(defun execute-lama (&key (domain-file "domain.pddl") (problem-file "test.pddl")
			(domain-directory *ff-output-directory*) (output-directory *ff-output-directory*))
  (let ((output-file (concatenate 'string output-directory "lama-output.lisp")))
    (with-open-file (ostream output-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format ostream "~%(~a" problem-file))
    (inner-execute-lama domain-directory domain-file problem-file output-directory output-file)
    (with-open-file (ostream output-file :direction :output :if-exists :append :if-does-not-exist :create)
      (format ostream ")"))))
;;  | tail -n 2 -
;;  "t[-_0-9a-zA-Z]*.pddl"

(defun inner-execute-lama (domain-directory domain-file problem-file output-directory output-file)
  (let ((command (concatenate 'string "/Users/dborrajo/planning/fast-downward/lama11.sh " 
			      domain-directory domain-file " " domain-directory problem-file " " output-file
			      " | grep \"Plan length: \\|Plan cost: \\|Expanded\\|Total time:\""
 			      "| sed -e \"s/step(s).//\""
			      "| sed -e \"s/Plan length: / /\""
			      "| sed -e \"s/Plan cost: / /\""
 			      "| sed -e \"s/Expanded //\""
 			      "| sed -e \"s/state(s)./ /\""
 			      "| sed -e \"s/Total time: //\""
 			      "| sed -e \"s/s//\""
			      ">> " output-directory "lama-output.lisp")))
    (execute-shell-command command  "/Users/dborrajo/tmp/run-ff")))


