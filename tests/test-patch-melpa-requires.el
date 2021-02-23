;;; test-patch-melpa-requires.el --- Tests for patch-melpa-requires.el -*- lexical-binding: t; -*-

;; Copyright (C) 2021 The Authors of patch-melpa-requires.el

;; Authors: dickmao <github id: dickmao>
;; Version: 0.1.0
;; Keywords: package
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

;; Test stuff.

;;; Code:

(require 'patch-melpa-requires)

(ert-deftest test-patch-melpa-requires ()
  (should (package-installed-p 'ein))
  (should (package-installed-p 'dash (version-to-list "2.17")))
  (should-not (package-installed-p 'ein (version-to-list "20191221.21")))
  (should-not (package-installed-p 'ein (version-to-list "0.18"))))

(provide 'test-patch-melpa-requires)

;;; test-patch-melpa-requires.el ends here
