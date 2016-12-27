;; ido-speed-hack.el -- speed improvements for Emacs IDO mode

;; Copyright (C) 2012  Daniel Skarda
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
;;
;; ido-speed-hack.el
;; =================
;;
;; Speed optimisations for Emacs IDO mode. It makes ido completion fast
;; and interactive even with large sets of completions (eg. list of all
;; commands in M-x, or large Info index files like Elisp or Emacs
;; manual).
;;
;; Optimisations
;; -------------
;; ido-speed-hack increases ido performance in several ways:
;;
;; * disabling ido-case-fold for some completions (all commands are lowercase anyway)
;; * prunning collections based on character bitmaps
;; * caching character bitmaps during completion or for same completions
;; * caching intermediate completion lists when adding new characters
;;
;; Optimisations
;; -------------
;; ido-speed-hack increases ido performance in several ways:
;;
;; * function inlining (defsubst)
;; * disabling ido-case-fold for some completions (all commands are lowercase anyway)
;; * prunning collections based on character bitmaps
;; * caching character bitmaps during completion or for same completions
;; * caching intermediate completion lists when adding new characters
;;
;; Last three optimisations works only when regular expressions are disabled.
;;
;; Character bitmaps
;; -----------------
;; List of completions are prefiltered based on character bitmaps
;; Character bitmap is computed in function ido-string-to-bitmask.
;;
;; Each character from a .. z (regardless case) is assigned one bit.
;; Numbers, special characters - and * has also one bit assigned. 30 bits
;; are enought to store all flags.
;;
;; Character bitmap of a string is or of each individual string
;; characters.
;;
;; Before ido starts to examine completion list, ido-speed-hack
;; prefilters the list and removes all words which miss some characters
;; in ido-text. The filter is based on bitmap comparision.
;;
;; Bitmap caching
;; --------------
;; All bitmaps are cached during an execution of completing-read.
;; For special commands (execute-extended-command, describe-function,
;; smex etc) this cache is shared between executions.
;;
;; Result caching
;; --------------
;; If previous completion is a prefix of current completion, speed hack
;; reuses results from previous call so ido does not start from original
;; (long) list.
;;
;; Configuration
;; -------------
;; Use variable ido-speed-hints-alist to fine-tune hints for individual commands.

(defvar ido-shared-bitcache (make-hash-table :test 'equal :size 65537)
  "Hash table for caching computed bitmaps (usually for command names)")

(defvar ido-speed-hints-alist '((execute-extended-command shared case)
				(describe-function shared case)
				(describe-variable shared case)
				(smex shared case)
				(smex-major-mode-commands shared case)
				;; help is a special case as it is ugly dispatch
				;; for functions we would like to hint :(
				(help shared case))
  "Alist of function and function hint list.
Hints:
  shared	- use shared bitcache
  case		- case sensitive search
Both hints are intended for Emacs symbol searches (like in functions,
variables etc) as they are mostly lowercase (with few exceptions)")

(defsubst ido-string-to-bitmask (word &optional bit-cache)
  "Compute string bitmask (one bit for each character a-z, number, dash or star)"
  (or (and bit-cache
	   (gethash word bit-cache nil))
      (let ((r 0) x)
	(dotimes (n (length word))
	  (setq x (aref word n))
	  (if (<= ?a x)
	      (if (<= x ?z)		(setq r (logior r (lsh 1 (- x ?a -3)))))
	    (if (<= x ?9)
		(cond
		 ((<= ?0 x)		(setq r (logior r 1)))	; number
		 ((= x ?-)		(setq r (logior r 2)))	; -
		 ((= x ?*)		(setq r (logior r 4))))	; *
	      (if (and (<= ?A x)
		       (<= x ?Z))	(setq r (logior r (lsh 1 (- x ?A -3))))))))
	(when bit-cache
	  (puthash word r bit-cache))
	r)))

(defun ido-prefilter-list (list word &optional bit-cache)
  "Prefilter LIST using bitmask.
Result will contain only strings which contain all characters in
WORD (using bitmask). Optionally use BIT-CACHE to store bitmasks."
  (let ((mask (ido-string-to-bitmask word bit-cache)))
    (if (zerop mask)
	list
      (let ((result nil))
	(dolist (x list)
	  (when (= mask (logand mask
				;;			unroll ido-name
				(ido-string-to-bitmask (if (consp x) (car x) x)
						       bit-cache)))
	    (push x result)))
	result))))

(defvar ido-speed-text* nil)
(defvar ido-speed-orig* nil)
(defvar ido-speed-cache* nil)
(defvar ido-speed-bitcache* nil)

(defadvice ido-read-internal (around ido-speed-read first nil activate)
  "Prepare variables for ido-speed"
  (let* ((hints			(assq this-command ido-speed-hints-alist))
	 (*ido-speed-text*     "")	; text from previous call
	 (*ido-speed-orig*     nil)	; original list from previous call - used to
					; detect wheter we can reuse cache
	 (*ido-speed-cache*    nil)	; cache of prefiltered list from previous call
					; bit cache to store precomputed bitmasks
	 (*ido-speed-bitcache* (if (memq 'shared hints)
				   ido-shared-bitcache
				 (make-hash-table :test 'equal :size 65537)))
	 (ido-case-fold	       (if (memq 'case hints)
				   nil
				 ido-case-fold)))
    ad-do-it))

(defadvice ido-set-matches-1 (before ido-speed-match-first first nil activate)
  "An advice which uses prefiltering AND substring optimalization"
  (unless ido-enable-regexp
    (let ((input-list (ad-get-arg 0)))
      ;; restart the cache if
      ;; a) input list changed
      ;; b) previous ido-text is not substring of current ido-text
      (unless (and (equal input-list *ido-speed-orig*)
		   (<= (length *ido-speed-text*)
		       (length ido-text))
		   (string= *ido-speed-text*
			    (substring ido-text 0
				       (min (length *ido-speed-text*)
					    (length ido-text)))))
	(setq *ido-speed-cache* input-list
	      *ido-speed-orig*  input-list))

      (setq *ido-speed-text* ido-text
	    *ido-speed-cache* (ido-prefilter-list *ido-speed-cache* ido-text
						  *ido-speed-bitcache*))
      (ad-set-arg 0 *ido-speed-cache*))))

(provide 'ido-speed-hack)
