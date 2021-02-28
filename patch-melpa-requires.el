;;; patch-melpa-requires.el --- Reconcile MELPA versioning  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2021 The Authors of patch-melpa-requires.el

;; Authors: dickmao <github id: dickmao>
;; Version: 0.1.0
;; Keywords: maint tools
;; URL: https://github.com/dickmao/patch-melpa-requires
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with patch-melpa-requires.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package aims to Do the Right Thing by advising ``package-installed-p``
;; to disregard the MELPA-imposed version in ``PACKAGE-pkg.el``, and
;; instead refer directly to the Version header in package source files.
;;
;; To ensure the user has acknowledged the risks of `Advising Functions`_, he
;; must explicitly insert into his ``.emacs``:
;;
;; ::
;;
;;     (require 'patch-melpa-requires)
;;     (patch-melpa-requires-activate)
;;
;; Rationale: ``package-install`` generally will not update a bumped package dependency
;; because MELPA's timestamp versioning is incompatible with the semantic version numbers
;; in ``Package-Requires`` clauses (the Schism).
;;
;; The particulars of the Schism are expatiated in uninteresting detail in `Issue 2944`_.

;;; Code:

(require 'package)
(require 'subr-x)

(defun patch-melpa-requires--dir-info ()
  "Snippet from `package-dir-info'.
I considered advising `package-dir-info' but things are already too trick."
  (let ((files (directory-files default-directory t "\\.el\\'" t))
        info)
    (while files
      (with-temp-buffer
        (insert-file-contents (pop files))
        (when (setq info (ignore-errors (package-buffer-info)))
          (setq files nil)
          (setf (package-desc-kind info) 'dir))))
    (unless info
      (error "No .el files with package headers in `%s'" default-directory))
    info))

(defun patch-melpa-requires--diagnostic ()
  "Ensure `package-install' still works the way I expect."
  (let* ((base-header (list
                       ""
                       "base.el --- base.el"
                       "Version: 0.1.0"
                       "Package-Requires: ((required \"69.69.69\"))"
                       "base.el ends here"))
         (req-header (list
                      ""
                      "required.el --- required.el"
                      "Version: 69.69.68"
                      "required.el ends here"))
         (base-desc (with-temp-buffer
                      (insert (mapconcat #'identity base-header "\n;;; "))
                      (package-buffer-info)))
         (req-desc (with-temp-buffer
                     (insert (mapconcat #'identity req-header "\n;;; "))
                     (let ((desc (package-buffer-info)))
                       (setf (package-desc-version desc)
                             (version-to-list "20210101.0"))
                       desc)))
         (req-next (progn (let ((result (copy-package-desc req-desc)))
                            (setf (package-desc-version result)
                                  (version-to-list "69.69.69"))
                            result)))
         (package-alist (list (cons 'required (list req-desc))))
         (package-archive-contents (list (cons 'required (list req-next)))))
    (package-compute-transaction nil (package-desc-reqs base-desc))))

(defun patch-melpa-requires--advice (f &rest args)
  "Wrap call of F on ARGS, where F is `package-compute-transaction'.
Override `package-installed-p' so querying versions of package descriptors in
`package-user-dir' yields an apples-to-apples semver comparison."
  (let ((fallback (symbol-function 'package-installed-p)))
    (cl-letf
        (((symbol-function 'package-installed-p)
          (cl-function
           (lambda (package &rest args &aux (min-version (car args)))
             (condition-case nil
                 (or
                  (cl-flet ((semver-p
                             (version)
                             (version-list-< version '(19001201 1))))
                    (when-let ((desc (car (alist-get package package-alist))))
                      (let* ((reqv* (package-desc-version desc))
                             (reqv (if (and (semver-p min-version)
                                            (not (semver-p reqv*)))
                                       (with-temp-buffer
                                         (when-let ((default-directory
                                                      (package-desc-dir desc)))
                                           (package-desc-version
                                            (patch-melpa-requires--dir-info))))
                                     reqv*)))
                        (version-list-<= min-version reqv))))
                  (package-built-in-p package min-version))
               (error (apply fallback package args)))))))
      (apply f args))))

;;;###autoload
(defun patch-melpa-requires-activate ()
  "Advise `package-installed-p' to refer to explicit Version headers.
When running `package-compute-transaction', disregard the machine-generated version
specified in PACKAGE-pkg.el, and instead look directly at the Version headers
of source files."
  (condition-case err
      (progn (advice-remove 'package-compute-transaction
                            #'patch-melpa-requires--advice)
             (when (patch-melpa-requires--diagnostic)
               (error "`patch-melpa-requires--diagnostic': pre-advice failed"))
             (advice-add 'package-compute-transaction :around
                         #'patch-melpa-requires--advice)
             (unless (patch-melpa-requires--diagnostic)
               (error "`patch-melpa-requires--diagnostic': post-advice failed")))
    (error
     (advice-remove 'package-compute-transaction #'patch-melpa-requires--advice)
     (display-warning 'error
                      (format "patch-melpa-requires aborted: %s"
                              (error-message-string err))))))

(provide 'patch-melpa-requires)

;;; patch-melpa-requires.el ends here
