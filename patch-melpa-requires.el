;;; patch-melpa-requires.el --- Mitigate erroneous MELPA versioning when updating packages  -*- lexical-binding: t; coding: utf-8 -*-

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

;; Extract ``Version`` header for melpa directories when checking package
;; dependencies.

;;; Code:

(require 'package)

(defun patch-melpa-requires (f package &optional min-version)
  "Override F if PACKAGE requires a semver MIN-VERSION.
Replace `package-desc-version' of descriptors in `package-user-dir'
for an apples-to-apples semver comparison."
  (cl-flet ((semver-p (version) (version-list-< version '(19001201 1))))
    (if (and min-version (semver-p min-version))
        (cl-letf (((symbol-function 'package-desc-version)
                   (lambda (desc &rest _args)
                     "DESC descriptor resident in `package-user-dir'."
                     (let ((version (cl-struct-slot-value 'package-desc 'version desc)))
                       (if (semver-p version)
                           version
                         (with-temp-buffer
                           (let ((default-directory (package-desc-dir desc)))
                             (dired-mode)
                             (cl-letf (((symbol-function 'package--description-file)
                                        (lambda (&rest _args)
                                          (make-temp-name "unexist"))))
                               (cl-struct-slot-value 'package-desc 'version
                                                     (package-dir-info))))))))))
          (funcall f package min-version))
      (funcall f package min-version))))

(if (and (fboundp 'package--description-file)
         (fboundp 'package-installed-p))
    (progn
      (remove-function (symbol-function 'package-installed-p)
                       #'patch-melpa-requires)
      (add-function
       :around (symbol-function 'package-installed-p)
       #'patch-melpa-requires))
  (display-warning 'error "patch-melpa-requires is obsolesced"))

(provide 'patch-melpa-requires)

;;; patch-melpa-requires.el ends here
