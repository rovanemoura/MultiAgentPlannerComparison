#|
===========================
File: diret.lisp
Authors: Vladimir Kulyukin,
         Alex Lifshits 

Description: variables and parameters for Diret1.0
Copyright (c) 1999 

Comments and bugs to vkulyukin@cs.depaul.edu
=========================== 
|# 

(in-package :cl-user)

(eval-when (compile load eval)
  (pushnew :diret-params *features*))

(export '(DIRET-SERVER-ACTIVE-P DIRET-CLIENT-ACTIVE-P))

(defparameter *diret-dir* (directory-namestring *load-truename*))

(defparameter *stocks-dir-inx* "stocks-dir-inx.ior")
(defparameter *stocks-dir-ret* "stocks-dir-ret.ior")
(defparameter *bonds-dir-inx*  "bonds-dir-inx.ior")
(defparameter *bonds-dir-ret*  "bonds-dir-ret.ior")
(defparameter *cash-dir-inx*   "cash-dir-inx.ior")
(defparameter *cash-dir-ret*   "cash-dir-ret.ior")

(defparameter *coldir* (concatenate 'string *diret-dir* "data\\"))
(defparameter *stoplist-path* (concatenate 'string *coldir* "stoplist.txt"))
(defparameter *files* '(
                        "common-stock-funds.txt"
                        "growth-and-income-funds.txt"
                        "small-company-funds.txt"
                        "equity-income-funds.txt"
                        "growth-funds.txt"
                        "emerging-market-funds.txt"
                        "international-mutual-funds.txt"
                        "myths-about-indexing.txt"
                        "investment-grade-corporate-bond-funds.txt"
                        "municipal-tax-free-bond-funds.txt"
                        "us-treasure-and-government-bond-funds.txt"
                        "bond-funds.txt"
                        "basics-of-bonds.txt"
                        "morgage-backed-securities-funds.txt"
                        "bond-funds-risks.txt"
                        "investing-in-individual-bonds.txt"
                        "money-markets.txt"
                        "bear-market-causes.txt"
                        "bear-market-survival.txt"
                        "bear-markets.txt"
                        "dollar-cost-averaging.txt"
                        "financing-college.txt"
                        "mutual-funds-costs.txt"
                        "mutual-funds-and-taxes.txt"
                        "past-bear-markets.txt"
                        "readiness-for-bear-markets.txt"                        
                        ))
	       
(defparameter *stock-funds* '(
                              "common-stock-funds.txt"
                              "growth-and-income-funds.txt"
                              "small-company-funds.txt"
                              "equity-income-funds.txt"
                              "growth-funds.txt"
                              "emerging-market-funds.txt"
                              "international-mutual-funds.txt"
                              "myths-about-indexing.txt"
                              ))

(defparameter *bond-funds* '(
                             "investment-grade-corporate-bond-funds.txt"
                             "municipal-tax-free-bond-funds.txt"
                             "us-treasure-and-government-bond-funds.txt"
                             "bond-funds.txt"
                             "basics-of-bonds.txt"
                             "morgage-backed-securities-funds.txt"
                             "bond-funds-risks.txt"
                             "investing-in-individual-bonds.txt"
                             ))

(defparameter *cash-funds* '(
                             "money-markets.txt"
                             "bear-market-causes.txt"
                             "bear-market-survival.txt"
                             "bear-markets.txt"
                             "dollar-cost-averaging.txt"
                             "financing-college.txt"
                             "mutual-funds-costs.txt"
                             "mutual-funds-and-taxes.txt"
                             "past-bear-markets.txt"
                             "readiness-for-bear-markets.txt"
                             ))

(defparameter *sample-documents* '(
                                   "sample-document1.txt"
				   "sample-document2.txt"
				   "sample-document3.txt"
                                   ))

(defun user-profile-path ()
   (merge-pathnames (current-directory) "user-profile.txt"))

(defun user-retrievals-path ()
   (merge-pathnames (current-directory) "user-retrievals.txt"))

(defun diret-server-active-p ()
   (if (find :diret-server *features*) t nil))

(defun diret-client-active-p ()
   (if (find :diret-client *features*) t nil))

