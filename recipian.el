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
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'headline
      #'recipian--parse-recipe)))


(defun recipian-write-json (filename recipes)
  "Generate a `ROOT/recipes.json' from RECIPES."
  (write-region (json-encode recipes) nil filename))


(defun recipian-parse-plans (org-file)
  "Parse a list of meal plans from ORG-FILE"
  (with-temp-buffer
    (insert-file-contents org-file)
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'headline
      #'recipian--parse-plan)))


(defun recipian-all-names (filename)
  "Parse FILENAME for recipes and return list of names."
  (mapcar (lambda (recipe) (alist-get 'name recipe))
		  (recipian-parse-recipes filename)))


(defun recipian--org-element-tags (elem)
  "Return a list of all tags of ELEM. `org-element-property' doesn't implement
inherited tags or filetags"
  (mapcar #'recipian--strip-props
          (org-get-tags (org-element-property :begin elem))))


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
        (ingredients  (mapcar #'recipian--parse-ingredient
                              (recipian--child-as-list
                               (recipian--find-child elem "Ingredients"))))
        (steps (mapcar #'recipian--strip-props
                       (recipian--child-as-list
                        (recipian--find-child elem "Steps"))))
        (notes (recipian--org-element-contents
                (recipian--find-child elem "Notes")))
        (servings (org-element-property :SERVINGS elem))
        serving-size
        serving-type
        (source (org-element-property :SOURCE elem)))
    (when (and ingredients steps)
      (when (string-match "^\\([0-9]+\\) +\\(.*\\)$" servings)
        (setq serving-size (string-to-number (match-string 1 servings)))
        (setq serving-type (recipian--strip-props (match-string 2 servings))))
      `((name . ,name)
        (tags . ,tags)
        (ingredients . ,ingredients)
        (steps . ,steps)
        (notes . ,notes)
        (serving-size . ,serving-size)
        (serving-type . ,serving-type)
        (source . ,source)))))


(defun recipian--parse-ingredient (line)
  "Parse an ingredient LINE and return a triple of (AMOUNT UNIT TEXT)."
  (if (not (string-match "^ *\\([./0-9]+\\) *\\([a-z]*\\)\\b\\(.*\\)$" line))
      `((amount . nil)
        (unit . nil)
        (ingredient . ,line))
    (defun clean-match (n)
      (recipian--strip-props (string-trim  (match-string n line))))
    `((amount . ,(clean-match 1))
      (unit . ,(clean-match 2))
      (ingredient . ,(clean-match 3)))))


(defun recipian--parse-plan (elem)
  "Parse the plan at ELEM and return an associated list of data. Returns NIL on
an invalid plan."
  (let ((name (org-element-property :raw-value elem))
        (tags (recipian--org-element-tags elem))
        (todo (recipian--strip-props (org-element-property :todo-keyword elem)))
        (notes (recipian--element-as-string elem))
        (date (org-element-property :scheduled elem))
        count)
    (when (and date
               (member todo '("COOK" "PREP" "DONE"))
               (member "plan" tags))
      ;; if name ends in "xN": pull out number into count and strip from name
      (setq name
            (string-trim
             (replace-regexp-in-string
              "x[0-9]+$"
              (lambda (match)
                (setq count (string-to-number (substring match 1)))
                "")
              name
              nil)))
      `((name . ,name)
        (todo . ,todo)
        (notes . ,notes)
        (count . ,count)
        (date . ,(format-time-string "%Y-%m-%d" (org-timestamp-to-time date)))))))


(defun recipian--element-as-string (elem)
  "Return a textual representation of ELEM, without any `org-mode' special
blocks"
  (recipian--strip-props
   (mapconcat
    #'identity
    (org-element-map (org-element-contents elem) 'paragraph
      (lambda (item)
        (let ((start (org-element-property :contents-begin item))
              (end (org-element-property :contents-end item)))
          (buffer-substring start (1- end))))
      nil nil 'paragraph)
    "\n"
    )))


(provide 'recipian)

;;; recipian.el ends here
