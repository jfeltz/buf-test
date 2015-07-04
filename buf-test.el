;;; buf-test.el --- Language agnostic unit-test switching, generation, and running

;; Copyright (C) 2015 John P. Feltz 

;; Author: John Feltz <jfeltz@gmail.com>
;; Keywords: tools, convenience, matching, languages, files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 's) 

(defun nil-var-error (var)
  (concat
   (format "%s" var)
   ", the "
   (documentation-property var 'variable-documentation) ", is nil"))

(defgroup buf-test nil
  "facilitates toggle between source/test, generation, and dispatch of test-runner on buffer" 
  :group 'convenience 
  :link '(emacs-library-link :tag "Lisp File" "buf-test.el"))

(defcustom buf-test:test-runner nil 
  "function to convert relative test source to test goal, i.e:
  test-src -> command test goal, the function should be of the form:
  (defun my-test-runner (test-path) ..) 
  See examples.
  "
  :group 'buf-test
  )

(defcustom buf-test:iso nil 
  "the source-to-test isomorphism to apply"
  :group 'buf-test
  )

(defcustom buf-test:test-writer  nil
  "the test writer, a function producing a strifng and of the form:
   f test-path source-path "
  :group
  'buf-test)

(defcustom buf-test:source-writer nil
  "the source writer, a function producing a string and of the form
   f source-path test-path"
  :group 'buf-test)

;; an enumerated type
(defvar test-t)
(defvar source-t)

;; record accessors 
(defvar get-writer)
(defvar get-alt)

;; base isomorphism and common records for source and test
(defconst base-iso
  '((source-t .
    ((get-writer . buf-test:source-writer)
     (get-alt    . test-t)))
    (test-t   .
    ((get-writer . buf-test:test-writer)
     (get-alt    . source-t)))))

(defun to-alt (type)
  (cdr (assoc 'get-alt (cdr (assoc type base-iso)))))
(defun to-writer (type)
  (cdr (assoc 'get-writer (cdr (assoc type base-iso)))))
(defun to-fn-morph (iso type)
  (cdr (assoc 'get-morph (cdr (assoc type iso)))))
(defun to-file-pred (iso type)
  (cdr (assoc 'get-pred (cdr (assoc type iso)))))

(defun from-iso-pred (p abspath iso)
  (assert (f-absolute? abspath))
  (let
    ((predicate (to-file-pred iso p)))
    (if (funcall predicate abspath) p)))

(defun to-alt-path (type abspath)
  (assert (f-absolute? abspath) nil)
  (funcall (to-fn-morph buf-test:iso type) abspath))

(defun to-type (abspath)
  "return file identity"
  (assert (f-absolute? abspath) nil)
  (or
    (from-iso-pred 'test-t   abspath buf-test:iso)
    (from-iso-pred 'source-t abspath buf-test:iso)))

(defun update-editor (writer dest-path origin-path)
  (let 
     ((exists (file-exists-p dest-path)))
     (cond
      ((and (not exists) writer)
      (progn
        (message "buf-test: creating file from writer")
        ;(message "writer: %s" (symbol-function (eval writer)))
        (f-write-text
         (funcall (eval writer) dest-path origin-path) 'utf-8 dest-path)))
      ((and (not exists) (not writer))
        (message "buf-test: switching to empty buffer of %s" dest-path))
      (t (message "visiting for %s" dest-path)))
      (find-file dest-path)))

(defun buf-test:toggle ()
  (interactive)
  (if (not buf-test:iso) (nil-var-error buf-test:iso)) 
  (let* ((type (to-type buffer-file-name)))
    (if (not type)
      (message "buf-test: buffer path is unrecognized")
      (update-editor
        (to-writer (to-alt type))
        (to-alt-path type buffer-file-name) buffer-file-name))))

(defun buf-test:run-test (test-path)
  (cond
    ((not (f-exists? test-path)) 
     (message "associated test, %s doesn't exist." test-path))
    ((not buf-test:test-runner)
     (message "buf-test:test-runner is not set, see documentation."))
    (t (compile (funcall buf-test:test-runner test-path)))))

(defun buf-test:test ()
  "executes test runner on associated test-buffer path"
  (interactive)
  (let ((type (to-type buffer-file-name)))
    (cond
     ((not type) (message "buf-test: buffer path is unrecognized"))
     ((and (eq type 'test-t))
      (buf-test:run-test buffer-file-name))
     ((eq type 'source-t)
      (buf-test:run-test (to-alt-path type buffer-file-name))))))

(defun rooted (root parent abspath)
  (assert (f-absolute? abspath))
  (and
    (f-ancestor-of? root abspath)
    (or (not parent) (f-ancestor-of? (f-join root parent) abspath))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the suffix isomorphism, predicates, and helper functions ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar get-morph)
(defvar get-pred)

(defun iso:is-source (root parent abspath)
  (assert (f-absolute? abspath))
  (message "is-source %s %s %s" root parent abspath)
  (rooted root parent abspath))

(defun iso:to-sub-path (root parent abspath)
  (assert (f-ancestor-of? root abspath))
  (assert (f-filename abspath))
  (let
    ((path-list 
     (funcall
     (if (not parent) 'identity 'cdr)
     (delete (f-filename abspath) (f-split (f-relative abspath root ))))))
     (if path-list (apply 'f-join path-list) nil)))

(defun iso:to-abs (root parent sub-path filename)
  (f-join root (if parent parent "") (if sub-path sub-path "") filename))

(defun suffix-iso:is-test (suffix root test-parent abspath)
  (and
    (rooted root test-parent abspath)
    (string-match-p (concat ".+" suffix "\\(\.\\|$\\)") (f-filename abspath))))

(defun suffix-iso:src-to-test (suffix root test-parent source-parent abspath)
  (assert (iso:is-source root source-parent abspath) nil)
  (let* ((fn (f-filename abspath)))
    (iso:to-abs
     root
     test-parent
     (iso:to-sub-path root source-parent abspath)
     (concat (f-no-ext fn) suffix "." (f-ext fn)))))

(defun suffix-iso:test-to-src (suffix root test-parent source-parent abspath)
  (assert (suffix-iso:is-test suffix root test-parent abspath) nil)
  (let*
    ((fn (f-filename abspath)))
     (iso:to-abs
      root
      source-parent
      (iso:to-sub-path root source-parent abspath)
      (concat (s-chop-suffix suffix (f-no-ext fn)) "." (f-ext fn)))))
  
(defun suffix-iso (suffix root test-parent source-parent)
  (assert
    (and (f-directory? root) (f-absolute? root)) nil "invalid abs. directory")
  (list
   (cons 'source-t
    (list
     (cons 'get-morph
           (apply-partially
             'suffix-iso:src-to-test suffix root test-parent source-parent))
     (cons 'get-pred
           (apply-partially 'iso:is-source root source-parent))))
   (cons 'test-t
     (list
      (cons 'get-morph
            (apply-partially
             'suffix-iso:test-to-src suffix root test-parent source-parent))
      (cons 'get-pred
            (apply-partially 'suffix-iso:is-test suffix root test-parent))))))

(provide 'buf-test)
