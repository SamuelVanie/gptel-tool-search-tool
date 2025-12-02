;;; tool-search-tool.el --- summary -*- lexical-binding: t -*-

;; Author: samuelvanie
;; Maintainer: samuelvanie
;; Version: 1.0
;; Package-Requires: (gptel)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; commentary

;;; Code:

(require 'gptel)
(eval-when-compile (require 'cl-lib))
(require 'url)
(require 'json)


;;; The embedding values for the different tools
(defvar toolsearchtool--embedding-values nil)

(defgroup toolsearchtool--embedding nil
  "Configuration for embedding requests."
  :group 'tools)

(defcustom toolsearchtool-embedding-endpoint "http://localhost:1234/v1/embeddings"
  "The full URL for the embedding endpoint."
  :type 'string
  :group 'toolsearchtool--embedding)

(defcustom toolsearchtool-embedding-model "text-embedding-qwen3-embedding-4b"
  "The model name to send in the request body."
  :type 'string
  :group 'toolsearchtool--embedding)

(defcustom toolsearchtool-get-available-tools 
  "A function that list all the tools that exists.
Check out my default implementation for gptel `toolsearchtool--default-get-available-tools'"
  :default #'toolsearchtool--default-get-available-tools
  :type 'function
  :group 'toolsearchtool--embedding)


(defun toolsearchtool--default-get-available-tools ()
  "For gptel all, the known tools are stored inside the `gptel--known-tools' variable"
  (gptel--known-tools))

(defun toolsearchtool--get-embedding (text)
  "Send TEXT to the configured embedding endpoint and return the vector.
This function is synchronous and blocks until the request completes."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data
         (json-encode `((input . ,text)
                        (model . ,toolsearchtool--embedding-model)))))
    
    (let ((buffer (url-retrieve-synchronously toolsearchtool--embedding-endpoint)))
      (if (not buffer)
          (error "Failed to retrieve embedding: Connection failed")
        (with-current-buffer buffer
          (goto-char (point-min))
          (if (search-forward "\n\n" nil t)
              (let* ((json-object-type 'plist) ;; Ensure we get plists
                     (json-array-type 'list)
                     (response (json-read))
                     ;; Parse: response -> data -> [0] -> embedding
                     (data (plist-get response :data))
                     (first-item (car data))
                     (embedding (plist-get first-item :embedding)))
                (kill-buffer buffer)
                (unless embedding
                  (error "Response did not contain an embedding field: %S" response))
                embedding)
            (kill-buffer buffer)
            (error "Invalid response headers")))))))

(defun toolsearchtool--add-embedding-value (toolname vector)
  "Add a vector embedding value to the values registered for the
  current tools. (`toolsearchtool--embedding-values')
Start by testing if the tool's value is already registered. If so, a
dialog box ask the user if he wants to recalculate the value for that tool or all of them.
We then proceeds as answered by the user.
Else we just go ahead by calculating and adding."
  (if not (map-contains-key toolsearchtool--embedding-values toolname)
    ;; vector not already registered
    ;; then add it
    (setq toolsearchtool--embedding-values
	  (acons toolname vector toolsearchtool--embedding-values))
    ;; else
    
    )
  )

(defun toolsearchtool--build-string (tool)
  "Build the description of the tool.
The tool structure is gotten from the function `'"
  (let ((tool-name (first tool))
	(tool-struct (rest tool)))
    (concat
     (format "Tool: %s\n" tool-name)
     (format "Category: %s\n" (gptel-tool-category tool-struct))
     (format "Description: %s\n" (gptel-tool-description tool-struct))
     (format "Parameters: ")
     (mapconcat (lambda (param)
		  (format "%s (%s): %s"
			  (plist-get param :name)
			  (plist-get param :type)
			  (plist-get param :description)))
		(gptel-tool-args tool-struct) ",")
     )
    )
  )

(defun toolsearchtool--get-tools-suggestion ()
  "Call the embedding model from the url defined by the user then calculate
the cosine similarity to get the appropriate
tools that will be returned to the model"
  
  )

(provide 'tool-search-tool)

;;; tool-search-tool.el ends here
