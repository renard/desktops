;;; desktops.el --- Manage virtual desktops in emacs.

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-07-31
;; Last changed: 2012-08-02 17:11:53
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile (require 'cl))

(defstruct desktop
  name
  tree
  windows)

(defstruct desktop-window
  buffer
  file
  start
  point
  mark)

(defstruct desktop
  window-tree
  selected-window-path)

(defstruct desktop-split
  dir
  size
  children)


(defvar desktop-current 0
  "Current desktop index")

(defvar desktop-alist nil
  "desktop alist.")


(defun desktop:tree2list (&optional tree)
  "Convert `window-tree' to persistant list."
  (let ((tree (or tree (car (window-tree)))))
    (if (windowp tree)
	(let ((buffer (window-buffer tree)))
	  (make-desktop-window :buffer (buffer-name buffer)
			       :file (buffer-file-name buffer)
			       :start (window-start tree)
			       :point (window-point tree)
			       :mark (with-current-buffer buffer (mark))))
      (let* ((dir (car tree))
	     (children (cddr tree))
	     (child (car children)))
	(list (if dir 'vertical 'horizontal)
	      ;; compute window size
	      (cond
	       ;; child is a window and split is vertical
	       ((and (windowp child) dir)
		(/ (window-total-size child nil) (float (frame-height))))
	       ;; child is a window and split is horizontal
	       ((windowp child)
		(/ (window-total-size child t) (float (frame-width))))
	       ;; split is vertical (multi windows)
	       (dir
		(let ((edge (cadr child)))
		  (/ (- (nth 2 edge) (car edge))  (float (frame-height)))))
	       ;; split is horizontal (multi windows)
	       (t
		(let ((edge (cadr child)))
		  (/ (- (nth 3 edge) (cadr edge)) (float (frame-width))))))
	      (desktop:tree2list (car children))
	      (if (> (length children) 2)
		  (desktop:tree2list (cons dir (cons nil (cdr children))))
		(desktop:tree2list (cadr children))))))))

(defun desktop:get-window-path (tree win &optional path)
  "Retrieve WIN in the window TREE. Window position is appended to PATH.

This allows the `selected-window' to be found using `nth' on a
`window-tree' result."
  (if (windowp tree)
      ;; if TREE is the selected window, return PATH
      (when (eq win tree) path)
    (loop for children = (cddr tree) then (cdr children)
	  for i = 0 then (1+ i)
	  while children
	  nconc  (desktop:get-window-path (car children) win (append path (list i))) into conf
	  ;; WIN is found in a child window
	  when conf return conf
	  finally return conf)))

(defun desktop:get-current()
  "Return current desktop layout."
  (make-desktop :window-tree (desktop:tree2list)
		:selected-window-path (desktop:get-window-path
				       (car (window-tree))
				       (selected-window))))

(defun desktop:save-current()
  ""
  (setq desktop-alist
	(append
	 (remove* desktop-current desktop-alist :key 'car)
	 (list (cons desktop-current (desktop:get-current))))))


(defun desktop:list2tree (conf)
  "Restore CONF."
  (if (listp conf)
      (let ((newwin
             (if (eq (car conf) 'horizontal)
                 (split-window nil
			       (floor (* (cadr conf) (frame-width)))
			       t)
               (split-window nil
			     (floor (* (cadr conf) (frame-height)))
			     ))))
        (desktop:list2tree (nth 2 conf))
        (select-window newwin)
        (desktop:list2tree (nth 3 conf)))
    ;; Restore window configuration
    (message "C: %S" conf)
    (let ((buffer (get-buffer (desktop-window-buffer conf))))
      (when (buffer-live-p buffer)
	(set-window-buffer nil buffer)
	(set-window-start nil (desktop-window-start conf))
	(set-window-point nil (desktop-window-point conf))))

))




(defun desktop:restore (id)
  "Restore desktop ID"
  (let ((desktop (cdr (assoc id desktop-alist))))
    (when desktop
      (delete-other-windows)
      (desktop:list2tree (desktop-window-tree desktop)))))
      

(provide 'desktops)

;; desktops.el ends here