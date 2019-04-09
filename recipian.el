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
  (with-temp-buffer
    (insert-file-contents org-file)
    ;; (org-show-subtree)
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (elem)
        (when (recipian--is-recipe-p elem)
          (recipian--export-single (recipian--parse-recipe elem)))))))


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
      )))

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


(defun recipian--export-single (recipe)
  (message "%S" recipe))


(provide 'recipian)

;;; recipian.el ends here
