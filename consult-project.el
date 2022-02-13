;;; consult-project.el --- Consult integration for porjectile  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  Marco Pawłowski
;; Keywords: convenience
;; Version: 0.5
;; Package-Requires: ((emacs "25.1") (consult "0.12") (project "2.5.0"))
;; URL: https://gitlab.com/OlMon/consult-project

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

;; A multiview for displaying open buffers and files accociated with a project.
;; When no project is open in the current buffer display a list of known project.
;; and select a file from the selected project.
;;
;; Just run the function `consult-project' and/or bind it to a hotkey.
;;
;; To filter the multiview use:
;; B - For project related buffers
;; F - For project related files
;; P - For known projects

;;; Code:

(require 'consult)

(defface consult-project-projects
  '((t :inherit font-lock-constant-face))
  "Face used to highlight projects in `consult-project'."
  :group 'consult-project)

(defvar consult-project--project-history nil)

(defvar consult-project-display-info t
  "Settings to let `consult-project' display project information in the annotation.")

(defcustom consult-project-sources
  '(
    ;; consult-project--source-buffer
    consult-project--source-file
    consult-project--source-project)
  "Sources used by `consult-project'.

See `consult--multi' for a description of the source values."
  :type '(repeat symbol)
  :group 'consult-project)

(defun consult-project--choose-file (root)
  "Create the list of files for the consult chooser based on project's notion of files for the project at ROOT."
  (let* ((inv-root (propertize root 'invisible t))
         (files (project-files root)))
    (mapcar (lambda (f) (concat inv-root f)) files)))

(defun consult-project--file (selected-project)
  "Create a view for selecting project files for the project at SELECTED-PROJECT."
  (find-file (consult--read
              (consult-project--choose-file selected-project)
              :prompt "Project File: "
              :sort t
              :require-match t
              :category 'file
              :state (consult--file-preview)
              :history 'file-name-history)))

;; (defvar consult-project--source-buffer
;;       `(:name      "Project Buffer"
;;                    :narrow    (?b . "Buffer")
;;                    :category  buffer
;;                    :face      consult-buffer
;;                    :history   buffer-name-history
;;                    :state     ,#'consult--buffer-state
;;                    :enabled   ,#'project-current
;;                    :items
;;                    ,(lambda () (project-buffers (project-current)))))

(defvar consult-project--source-file
      `(:name      "Project File"
                   :narrow    (?f . "File")
                   :category  file
                   :face      consult-file
                   :history   file-name-history
                   :action    ,(lambda (f) (consult--file-action (concat (project-root (project-current) f)))
                   :enabled   ,#'project-current ;; FIXME: (if (project-current) t nil) (?)
                   :items
                   ,(lambda () (project-files (project-current))))))


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
  "Create a multi view with project integration.   Displays known projects when there are none or the buffers/files accociated with the project."
  (interactive)
  (when-let (buffer (consult--multi consult-project-sources
                                    :prompt "Switch to: "
                                    :history 'consult-project--project-history
                                    :sort nil))
    ;; When the buffer does not belong to a source,
    ;; create a new buffer with the name.
    (unless (cdr buffer)
      (funcall consult--buffer-display (car buffer)))))


(provide 'consult-project)
;;; consult-project.el ends here
