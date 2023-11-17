;;; consult-project-extra.el --- Consult integration for project.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  Enrique Kessler Martínez
;; Keywords: convenience project management
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (consult "0.17") (project "0.8.1"))
;; URL: https://github.com/Qkessler/consult-project-extra

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

;; Creates an endpoint for accessing different project sources.  The consult view
;; can be narrowed to: (b) current project's buffers, (f) current project's files
;; and (p) to select from the list of known projects.

;; The buffer and project file sources are only enabled in case that the user is
;; in a project file/buffer.  See `project-current'.

;; A different action is issued depending on the source.  For both buffers and
;; project files, the default action is to visit the selected element.  When a
;; known project is selected, a list to select from is created with the selected
;; project's files.

;;; Code:

(require 'project)
(require 'consult)

(defface consult-project-extra-projects
  '((t :inherit font-lock-constant-face))
  "Face used to highlight projects in `consult-project-extra'."
  :group 'consult-project-extra)

(defvar consult-project-extra--project-history nil)

(defvar consult-project-extra-display-info t
  "Whether to display information about the project in the margin of the element.")

(defun consult-project-extra--project-with-root (root)
  "Return the project for a given project ROOT."
  (project--find-in-directory root))

(defun consult-project-extra--project-files (root)
  "Compute the project files given the ROOT."
  (setq root (file-name-as-directory root))
  (let* ((project (consult-project-extra--project-with-root root))
         (project-files-relative-names t)
         (files (project-files project))
         (root-len (length root)))
    (mapcar (lambda (f) (if (file-name-absolute-p f)
                       (if (string-prefix-p root f)
                           (substring f 0 root-len)
                         (file-relative-name f root))
                     f))
            files)))

(defun consult-project-extra--annotate-project (dir)
  "Annotation function for projects. Takes project root as DIR."
  (if consult-project-extra-display-info
      (concat (propertize " " 'display '(space :align-to center))
              (format "Project: %s" (file-name-nondirectory
                                     (directory-file-name dir))))))

;; All the icons support for the new project category
(cl-defmethod all-the-icons-completion-get-icon (cand (_cat (eql project)))
  "Return the icon for the candidate CAND of completion category project."
  (all-the-icons-completion-get-icon cand 'file))

;;;###autoload
(defun consult-project-extra-project-fn (&optional may-prompt)
  "`consult-project-extra' version of `consult--default-project-function'.

Return project root directory.
When no project is found and MAY-PROMPT is non-nil ask the user."
  (interactive)
  (let ((proj (project-current)))
    (cond (proj (cond
                 ((fboundp 'project-root) (project-root proj))
                 ((fboundp 'project-roots) (car (project-roots proj)))))
          (may-prompt (consult--read
                       (mapcar #'(lambda (x) (propertize x 'face 'consult-project-extra-projects))
                               (project-known-project-roots))
                       :prompt   "Project: "
                       :sort     t
                       :category 'project
                       :history  'consult-project-extra--project-history
                       :annotate #'consult-project-extra--annotate-project)))))

(defun consult-project-extra--find-with-concat-root (candidate)
  "Find-file concatenating root with CANDIDATE."
  (consult--file-action (concat (consult--project-root) candidate)))

;; The default `consult--source-project-buffer' has the ?p as narrow key,
;; and therefore is in conflict with `consult-project-extra--source-project'.
(defvar consult-project-extra--source-buffer
  (let* ((unmodified consult--source-project-buffer)
         (modified-source (plist-put (plist-put unmodified :hidden nil) :narrow ?b)))
    modified-source))

(defvar consult-project-extra--source-file
  `(:name      "Project File"
               :narrow    (?f . "File")
               :category  project-file
	       :default   t
               :face      consult-file
               :history   file-name-history
               :action    ,#'consult-project-extra--find-with-concat-root
               :new       ,#'consult-project-extra--find-with-concat-root
               :items     ,(lambda ()
                             (consult-project-extra--project-files (consult--project-root)))))

(defvar consult-project-extra--source-project
  `(:name      "Known Project"
               :narrow    (?p . "Project")
               :category  project
               :face      consult-project-extra-projects
               :history   consult-project-extra--project-history
               :action    ,#'consult-project-extra-find
               :annotate  ,#'consult-project-extra--annotate-project
               :items     ,#'project-known-project-roots))

(defcustom consult-project-extra-sources
  (list consult-project-extra--source-buffer
        consult-project-extra--source-file
        consult-project-extra--source-project)
  "Sources used by `consult-project-extra'.

See `consult--multi' for a description of the source values."
  :type '(repeat symbol)
  :group 'consult-project-extra)

;;;###autoload
(defun consult-project-extra-find (&optional root)
  "Create an endpoint for accessing different project sources.
The consult view can be narrowed to: (b) current project's
buffers,(f) current project's files and (p) to select from the
list of known projects.

If `consult-project-extra-find' is called outside of an project,
the user is queried for a project via `consult-project-function'
before `consult-project-extra-find' is called on that project

A different action is issued depending on the source. For both
buffers and project files, the default action is to visit the
selected element. When a known project is selected,
`consult-project-function' is called recursively with the
selected project as ROOT."
  (interactive)

  (let ((consult-project-function (if root
                                      (lambda (x) (ignore x) root)
                                    consult-project-function)))
    (consult--with-project
     (consult--multi consult-project-extra-sources
                     :sort nil
                     :require-match nil))))

;;;###autoload
(defun consult-project-extra-find-other-window ()
  "Variant of `consult-project-extra' which opens in a second window."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-window))
    (consult-project-extra-find)))

(provide 'consult-project-extra)
;;; consult-project-extra.el ends here
