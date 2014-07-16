;;; desktops.el --- Manage virtual desktops in emacs.

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-07-31
;; Last changed: 2012-08-02 19:03:29
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
  window-tree
  selected-window-path)

(defstruct desktop-split
  dir
  size
  child
  children)


(defvar desktop-current 0
  "Current desktop index")

(defvar desktop-alist nil
  "desktop alist.")


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
  (let ((desktop (cdr (assoc id desktop-alist))))
    (when desktop
      (delete-other-windows)
      (desktop:list2tree (desktop-window-tree desktop)))))
      

(provide 'desktops)

;; desktops.el ends here
