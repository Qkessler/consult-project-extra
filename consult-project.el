;;; consult-project.el --- Consult integration for project.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  Enrique Kessler Martínez
;; Keywords: convenience project management
;; Version: 0.1 
;; Package-Requires: ((emacs "27.1") (consult "0.12"))
;; URL: https://github.com/Qkessler/consult-project.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Creates an endpoint for accessing different project sources. The consult view
;; can be narrowed to: (b) current project's buffers, (f) current project's files
;; and (p) to select from the list of known projects.

;; The buffer and project file sources are only enabled in case that the user is
;; in a project file/buffer. See `project-current'.

;; A different action is issued depending on the source. For both buffers and
;; project files, the default action is to visit the selected element. When a
;; known project is selected, a list to select from is created with the selected
;; project's files.

;;; Code:

(require 'consult)

(defface consult-project-projects
  '((t :inherit font-lock-constant-face))
  "Face used to highlight projects in `consult-project'."
  :group 'consult-project)

(defvar consult-project--project-history nil)

(defvar consult-project-display-info t
  "Whether to display information about the project in the margin of the element")

(defcustom consult-project-sources
  '(
    consult-project--source-buffer
    consult-project--source-file
    consult-project--source-project)
  "Sources used by `consult-project'.

See `consult--multi' for a description of the source values."
  :type '(repeat symbol)
  :group 'consult-project)

(defun consult-project--project-with-root (root)
  "Return the project for a given project ROOT."
  (project--find-in-directory root))

(defun consult-project--project-files (root)
  "Compute the project files given the ROOT."
  (let* ((project (consult-project--project-with-root root))
         (files (project-files project)))
    (mapcar (lambda (f)
              (let* ((inv-dir (propertize (file-name-directory f) 'invisible t))
                     (file-name (file-name-nondirectory f)))
                (concat inv-dir file-name))) files)))

(defun consult-project--file (selected-root)
  "Create a view for selecting project files for the project at SELECTED-ROOT."
  (find-file (consult--read
              (consult-project--project-files selected-root)
              :prompt "Project File: "
              :sort t
              :require-match t
              :category 'file
              :state (consult--file-preview)
              :history 'file-name-history)))

(defvar consult-project--source-buffer
  `(:name      "Project Buffer"
               :narrow    (?b . "Buffer")
               :category  buffer
               :face      consult-buffer
               :history   buffer-name-history
               :state     ,#'consult--buffer-state
               :enabled   ,#'project-current
               :items
               ,(lambda ()
                  (consult--buffer-query :sort 'visibility
                                         :directory 'project
                                         :as #'buffer-name))))

(defvar consult-project--source-file
  `(:name      "Project File"
               :narrow    (?f . "File")
               :category  file
               :face      consult-file
               :history   file-name-history
               :action    ,#'consult--file-action
               :enabled   ,#'project-current
               :items
               ,(lambda () (consult-project--project-files (project-root (project-current))))))


(defvar consult-project--source-project
  `(:name      "Known Project"
               :narrow    (?p . "Project")
               :category  'consult-project-project
               :face      consult-project-projects
               :history   consult-project--project-history
               :annotate  ,(lambda (dir) (if consult-project-display-info (progn
                                                                            (format "Project: %s"
                                                                                    (file-name-nondirectory (directory-file-name dir))))))
               :action    ,#'consult-project--file
               :items     ,#'project-known-project-roots))

;;;###autoload
(defun consult-project ()
  "Creates an endpoint for accessing different project sources. The consult view
can be narrowed to: (b) current project's buffers, (f) current project's files
and (p) to select from the list of known projects.

The buffer and project file sources are only enabled in case that the user is
in a project file/buffer. See `project-current'.

A different action is issued depending on the source. For both buffers and
project files, the default action is to visit the selected element. When a
known project is selected, a list to select from is created with the selected
project's files"
  (interactive)
  (when-let (buffer (consult--multi consult-project-sources
                                    :prompt "Switch to: "
                                    :history 'consult-project--project-history
                                    :sort nil))
    ;; When the buffer does not belong to a source,
    ;; create a new buffer with the name.
    (unless (cdr buffer)
      (funcall consult--buffer-display (car buffer)))))

;;;###autoload
(defun consult-project-other-window ()
  "Variant of `consult-project' which opens in a second window."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-window))
    (consult-project)))

(provide 'consult-project)
;;; consult-project.el ends here
