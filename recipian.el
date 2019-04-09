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
  (let ((recipes))
    (with-temp-buffer
      (insert-file-contents org-file)
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (elem)
          (let ((recipe (recipian--parse-recipe elem)))
            (when recipe
              (recipian--generate-recipe www-root recipe)
              (push recipe recipes))))))
    (recipian--generate-index www-root recipes)
    ))


(defun recipian--is-recipe-p (elem)
  (and (recipian--org-element-find-child elem 'headline "Ingredients")
       (recipian--org-element-find-child elem 'headline "Steps")))


(defun recipian--org-element-children (elem type)
  (org-element-map elem type
    (lambda (child)
      (let ((parent (org-element-property :parent child)))
        (when (eq parent elem)
          child)))))


(defun recipian--org-element-find-child (elem type child-name)
  (seq-filter
   (lambda (child)
     (equal child-name
            (org-element-property :raw-value child)))
   (recipian--org-element-children elem type)))


(defun recipian--org-element-tags (elem)
  (let ((tags (org-element-property :tags elem))
        (parent (org-element-property :parent elem)))
    (if (and org-use-tag-inheritance parent)
        (delete-dups (append tags (recipian--org-element-tags parent)))
      tags)))


(defun recipian--parse-recipe (elem)
  (when (recipian--is-recipe-p elem)
    (let ((name (org-element-property :raw-value elem))
          (tags (recipian--org-element-tags elem))
          (ingredients (recipian--child-as-list
                        (recipian--org-element-find-child elem 'headline "Ingredients")))
          (steps (recipian--child-as-list
                  (recipian--org-element-find-child elem 'headline "Steps")))
          )
      (when (not (and ingredients steps))
        (error (format "invalid recipe '%s': must have headlines Ingredients and Steps" name)))
      `((name . ,name)
        (tags . ,tags)
        (ingredients . ,ingredients)
        (steps . ,steps)
        ))))

;; (defun recipian--org-element-to-text (elem)
;;   (let ((start (org-element-property :contents-begin elem))
;;         (end (org-element-property :contents-end elem)))
;;     (buffer-substring start (- end 1))))


(defun recipian--child-as-list (elem)
  (mapcar
   (lambda (elem)
     (let ((start (org-element-property :contents-begin elem))
           (end (org-element-property :contents-end elem)))
       (buffer-substring start (- end 1))))
   (org-element-map elem 'item #'identity nil nil 'item)))


(defun recipian--name-to-url (name)
  "Translates a recipe NAME to a valid url."
  (let ((replacements '((" " . "-")
                        ("&" . "and"))))
    (cl-loop for (src . tgt) in replacements do
         (setq name (replace-regexp-in-string (regexp-quote src) tgt name nil 'literal)))
    (downcase name)))


(defun recipian--generate-index (root recipes)
  "Generate an index.html using the exported RECIPES."
  (with-temp-file (concat root "/index.html")
    (insert "<html><head><title>recipe index</title></head><body>")
    (dolist (recipe recipes)
      (let ((name (alist-get 'name recipe)))
        (insert "<div><a href='recipe/" (recipian--name-to-url name) "'>" name "</a></div>")))
    (insert "</body>")))


(defun recipian--generate-recipe (root recipe)
  "Generate a RECIPE.html from RECIPE."
  (let ((name (alist-get 'name recipe))
        (ingredients (alist-get 'ingredients recipe))
        (steps (alist-get 'steps recipe)))
    (with-temp-file (concat root "/recipe/" (recipian--name-to-url name))
      (insert "<html><head><title>" name "</title></head><body>")
      (insert "<a href='../index.html'>go back</a>")
      (insert "<h1>" name "</h1>")
      (insert "<h2>Ingredients</h2>")
      (insert "<ul>")
      (dolist (ingredient ingredients)
        (insert "<li>" ingredient "</li>"))
      (insert "</ul>")
      (insert "<h2>Steps</h2>")
      (insert "<ul>")
      (dolist (step steps)
        (insert "<li>" step "</li>"))
      (insert "</ul>")
      )))


(provide 'recipian)

;;; recipian.el ends here
