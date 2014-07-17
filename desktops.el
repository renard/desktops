;;; desktops.el --- Manage virtual desktops in emacs.

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-07-31
;; Last changed: 2014-07-17 10:29:06
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile (require 'cl))

(defstruct desktop-window
  buffer
  buffer-name
  file
  start
  point
  mark)

(defstruct desktop
  name
  window-tree
  selected-window-path
  buffer-list)

(defstruct desktop-split
  dir
  size
  child
  children)


(defvar desktop-current 0
  "Current desktop index")

(defvar desktop-alist nil
  "desktop alist.")



;; Helper function

(defun desktop:vdelete(vector id)
  "Delete element ID from VECTOR and return new vector. VECTOR is
not modified."
  (loop for i below (length vector)
	unless (= i id)
	collect (elt vector i) into nv
	finally return (apply #'vector nv)))

(defun desktop:vinsert (vector elt &optional id)
  "Insert ELT into VECTOR at position ID and return new
vector. VECTOR is not modified.

If ID is out of VECTOR boundaries, ELT is either prepended or
appended to VECTOR depending if ID is smaller or greater than
VECTOR size.

Id ID is nil ELT is appended to VECTOR.
"
  (cond
   ((or (not id) (>= id (length vector)))
    (vconcat v (if (vectorp elt) elt (vector elt))))
   ((< id 0)
    (vconcat (if (vectorp elt) elt (vector elt)) v))
   (t
    (loop for i below (length vector)
	  if (= i id)
	  nconc (list elt (elt vector i)) into nv
	  else
	  collect (elt vector i) into nv
	  finally return (apply #'vector nv)))))




(defun desktop:get-window-size (win dir)
  "return WIN window size in DIR direction. Returned value is a
percentage of the total frame size in DIR.

If DIR is t the direction is horizontal, vertical otherwise."
  (let ((frame-size (if dir 'frame-height 'frame-width))
	(edge1 (if dir 'cadr 'car))
	(edge2 (if dir 'cadddr 'caddr)))
    (/
     (if (windowp win)
	 (window-total-size win (not dir))
       (let ((edge (cadr win)))
	 (- (funcall edge2 edge) (funcall edge1 edge))))
     (float (funcall frame-size)))))

(defun desktop:buffer-to-desktop-window (buffer &optional tree)
  "Convert BUFFER to `desktop-window'."
  (make-desktop-window :buffer buffer
		       :buffer-name (buffer-name buffer)
		       :file (buffer-file-name buffer)
		       :start (window-start tree)
		       :point (window-point tree)
		       :mark (with-current-buffer buffer (mark))))

(defun desktop:tree2list (&optional tree)
  "Convert `window-tree' to persistant list."
  (let ((tree (or tree (car (window-tree)))))
    (if (windowp tree)
	(let ((buffer (window-buffer tree)))
	  (desktop:buffer-to-desktop-window buffer tree))
      (let* ((dir (car tree))
	     (children (cddr tree))
	     (child (car children)))

	(make-desktop-split
	 :dir (if dir 'vertical 'horizontal)
	 :size (desktop:get-window-size child dir)
	 :child (desktop:tree2list child)
	 :children (if (> (length children) 2)
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

(defun desktop:get-current(name)
  "Return current desktop layout."
  (make-desktop :name name	       
		:window-tree (desktop:tree2list)
		:selected-window-path (desktop:get-window-path
				       (car (window-tree))
				       (selected-window))
		:buffer-list (loop for b in (buffer-list)
				   collect (desktop:buffer-to-desktop-window b))))

(defun desktop:get-by-id (&optional id)
  "Return desktop structure ID"
  (let ((id (or id desktop-current)))
    (cdr (assoc id desktop-alist))))

(defun desktop:rename(name)
  (interactive "MDesktop name: ")
  (desktop:save-current name)
  (message "Saved %s" name))

(defun desktop:save-current(&optional name)
  ""
  (let ((name (or name
		  (desktop-name (desktop:get-by-id))
		  (format "Desktop %s" desktop-current))))
    (setq desktop-alist
	  (sort
	   (append
	    (remove* desktop-current desktop-alist :key 'car)
	    (list (cons desktop-current (desktop:get-current name))))
	   (lambda (a b) (< (car a) (car b)))))))

(defun desktop:create-new ()
  "Create a new desktop"
  (interactive)
  (desktop:save-current)
  (delete-other-windows)
  (setf desktop-current (length desktop-alist))
  (desktop:save-current))

(defun desktop:prev ()
  "Activate previous desktop."
  (interactive)
  (desktop:restore (1- desktop-current)))

(defun desktop:next ()
  "Activate next desktop."
  (interactive)
  (desktop:restore (1+ desktop-current)))

(defun desktop:display-current ()
  (interactive)
  (message
   (mapconcat #'identity
	      (loop for i in desktop-alist
		    collect (let* ((id (car i))
				   (name (desktop-name (desktop:get-by-id id))))
			      (if (= desktop-current id)
				  (propertize name
					      'face 'font-lock-warning-face)
			        name)))
	      " ")))

(defun desktop:list2tree (conf)
  "Restore CONF."
  (if (desktop-split-p conf)
      (let ((newwin
             (if (eq (desktop-split-dir conf) 'horizontal)
                 (split-window nil
			       (floor (* (desktop-split-size conf) (frame-width)))
			       t)
               (split-window nil
			     (floor (* (desktop-split-size conf) (frame-height)))
			     ))))
        (desktop:list2tree (desktop-split-child conf))
        (select-window newwin)
        (desktop:list2tree (desktop-split-children conf)))
    ;; Restore window configuration
    ;;(message "C: %S" conf)
    (let ((buffer (get-buffer (desktop-window-buffer-name conf))))
      (when (buffer-live-p buffer)
	(set-window-buffer nil buffer)
	(set-window-start nil (desktop-window-start conf))
	(set-window-point nil (desktop-window-point conf))))))

(defun desktop:restore (id)
  "Restore desktop ID"
  (interactive "NRestore desktop number: ")
  (let* ((id
	  (cond
	   ((>= id (length desktop-alist)) 0)
	   ((< id 0) (1- (length desktop-alist)))
	   (t id)))
	 (desktop (cdr (assoc id desktop-alist))))
    (when desktop
      (desktop:save-current)
      (delete-other-windows)
      (desktop:list2tree (desktop-window-tree desktop))
      (setf desktop-current id)
      (loop for b in (desktop-buffer-list desktop)
	    do (bury-buffer (desktop-window-buffer b)))
      (desktop:display-current))))
      

(provide 'desktops)

;; desktops.el ends here
