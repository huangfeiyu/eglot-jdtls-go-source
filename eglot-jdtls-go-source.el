;;; eglot-jdtls-go-source.el --- Go to Java source code in eglot+jdtls Java development -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Huang Feiyu

;; Author: Huang Feiyu <sibadake1@163.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (eglot "1.8"))
;; Keywords: tools, languages, Java, source code
;; URL: https://github.com/huangfeiyu/eglot-jdtls-go-source
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of eglot-jdtls-go-source.
;;
;; eglot-jdtls-go-source is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3
;; of the License, or (at your option) any later version.
;;
;; eglot-jdtls-go-source is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with eglot-jdtls-go-source; if not, see <http://www.gnu.org/licenses/>.
;;; Commentary:

;; This package register a file handler so eglot can go to the source
;; code of Java libraries and JDK source code.


;;; Code:

(defgroup eglot-jdtls-go-source nil
  "Dispay document for mouse hover."
  :prefix "eglot-jdtls-go-source-"
  :group 'eglot-jdtls-go-source)

(defcustom eglot-jdtls-go-source-lib-source-code-dir ".metadata"
  "The dir that will be used to store the source code of the Java library."
  :type 'string)

;;;###autoload
(defun eglot-jdtls-go-source-enable ()
  "Register the file name handler to eglot can go to the Java source code when jdtls is used."
  (add-to-list 'file-name-handler-alist '("\\`jdt://" . eglot-jdtls-go-source--jdtls-uri-handler)))

(defun eglot-jdtls-go-source--find-jdtls ()
  "Find the jdtls server."
  ;; the following code is adapted from zsxh's configuration file.
  (let ((filter-fn (lambda (server)
                     (cl-loop for (_ . languageid) in
                              (eglot--languages server)
                              when (string= languageid "java")
                              return languageid)))
        (servers (gethash (eglot--current-project) eglot--servers-by-project)))
    (cl-find-if filter-fn servers)))

(defun eglot-jdtls-go-source--make-path (root-dir &rest path-elements)
  "Compose a path from a base folder ROOT-DIR and a set of items PATH-ELEMENTS."
  ;; the following code is adapted from eglot-java
  (let ((new-path          (expand-file-name root-dir))
        (new-path-elements path-elements))
    (dolist (p new-path-elements)
      (setq new-path (concat (file-name-as-directory new-path) p)))
    new-path))

(defun eglot-jdtls-go-source--jdtls-uri-handler (_operation &rest args)
  "Support Eclipse jdtls `jdt://' uri scheme.
Optional argument ARGS for file name handler."
  ;; the following code is adapted from eglot-java
  (let* ((uri (car args))
         (cache-dir (expand-file-name eglot-jdtls-go-source-lib-source-code-dir (project-root (project-current t))))
         (source-file
          (expand-file-name
           (eglot-jdtls-go-source--make-path
            cache-dir
            (save-match-data
              (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot-jdtls-go-source--find-jdtls) :java/classFileContents (list :uri uri)))
            (metadata-file (format "%s.%s.metadata"
                                   (file-name-directory source-file)
                                   (file-name-base source-file))))
        (unless (file-directory-p cache-dir) (make-directory cache-dir t))
        (with-temp-file source-file (insert content))
        (with-temp-file metadata-file (insert uri))))
    source-file))

(provide 'eglot-jdtls-go-source)

;;; eglot-jdtls-go-source.el ends here
