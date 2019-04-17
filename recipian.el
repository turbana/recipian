;;; recipian.el -- handle recipes in org-mode
;;
;; Author: Ian Clark <ian@cyclone.local>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:


(defun recipian-generate-static-site (org-file www-root)
  "Parse ORG-FILE for recipes, generate a static site at WWW-ROOT."
  (let ((recipes (recipian-parse-recipes org-file)))
    (recipian-write-json (concat www-root "/recipes.json") recipes)))


(defun recipian-parse-recipes (org-file)
  "Parse a list of recipes from ORG-FILE."
  (with-temp-buffer
    (insert-file-contents org-file)
    (org-element-map (org-element-parse-buffer) 'headline
      #'recipian--parse-recipe)))


(defun recipian-write-json (filename recipes)
  "Generate a `ROOT/recipes.json' from RECIPES."
  (write-region (json-encode recipes) nil filename))


(defun recipian-parse-plans (org-file)
  "Parse a list of meal plans from ORG-FILE"
  (message "parsing %s" org-file)
  (with-temp-buffer
    (insert-file-contents org-file)
    (org-element-map (org-element-parse-buffer) 'headline
      #'recipian--parse-plan)))


(defun recipian-all-names (filename)
  "Parse FILENAME for recipes and return list of names."
  (mapcar (lambda (recipe) (alist-get 'name recipe))
		  (recipian-parse-recipes filename)))


(defun recipian--org-element-tags (elem)
  "Return a list of all tags of ELEM. `org-element-property' doesn't implement
inherited tags"
  (let ((tags (org-element-property :tags elem))
        (parent (org-element-property :parent elem)))
    (if (and org-use-tag-inheritance parent)
        (delete-dups (append tags (recipian--org-element-tags parent)))
      tags)))


(defun recipian--strip-props (string)
  "Remove all string properties from STRING. `org-element' places pointers to
the parse tree on strings, so when we call `(message)' on it we get a bunch of
junk."
  (when string
    (set-text-properties 0 (length string) nil string))
  string)


(defun recipian--org-element-contents (elem)
  "Return all text content under ELEM as a string."
  (recipian--strip-props (org-element-interpret-data (org-element-contents elem))))


(defun recipian--find-child (elem name)
  "Find the first direct child of ELEM with :raw-value of `NAME'."
  (org-element-map (org-element-contents elem) 'headline
    (lambda (child)
      (when (equal name (org-element-property :raw-value child))
        child))
    nil t 'headline))


(defun recipian--child-as-list (elem)
  "Find any org list items under ELEM and return as a lisp list."
  (org-element-map (org-element-contents elem) 'item
    (lambda (item)
      (let ((start (org-element-property :contents-begin item))
            (end (org-element-property :contents-end item)))
        (buffer-substring start (1- end))))
    nil nil 'item))


(defun recipian--parse-recipe (elem)
  "Parse the recipe at ELEM and return an associated list of data. Returns NIL
on an invalid recipe."
  (let ((name (org-element-property :raw-value elem))
        (tags (recipian--org-element-tags elem))
        (ingredients (recipian--child-as-list
                      (recipian--find-child elem "Ingredients")))
        (steps (recipian--child-as-list
                (recipian--find-child elem "Steps")))
        (notes (recipian--org-element-contents
                (recipian--find-child elem "Notes")))
        (servings (org-element-property :SERVINGS elem))
        (source (org-element-property :SOURCE elem)))
    (when (and ingredients steps)
      `((name . ,name)
        (tags . ,tags)
        (ingredients . ,ingredients)
        (steps . ,steps)
        (notes . ,notes)
        (servings . ,servings)
        (source . ,source)))))


(defun recipian--parse-plan (elem)
  "Parse the plan at ELEM and return an associated list of data. Returns NIL on
an invalid plan."
  (let ((name (org-element-property :raw-value elem))
        (tags (recipian--org-element-tags elem))
        (date (org-element-property :scheduled elem)))
    (when (and date
               (or (string-prefix-p "TODO" name)
                   (string-prefix-p "DONE" name))
               (member "plan" tags))
      `((name . ,(substring name 5))
        (date . ,(format-time-string "%Y-%m-%d" (org-timestamp-to-time date)))))))


(provide 'recipian)

;;; recipian.el ends here
