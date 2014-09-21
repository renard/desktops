;;; desktops.el --- Manage virtual desktops in emacs.

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-07-31
;; Last changed: 2014-08-04 12:05:15
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'timer))

(cl-defstruct desktop-window
  buffer-name
  file
  start
  point
  mark)

(cl-defstruct desktop
  name
  window-tree
  selected-window-path
  buffer-list)

(cl-defstruct desktop-split
  dir
  size
  child
  children)


(defvar desktop-current 0
  "Current desktop index")

(defvar desktop-previous 0
  "Previous desktop index")

(defvar desktop-alist nil
  "desktop alist.")

(defvar desktop-save-file "/tmp/desktop"
  "Where to save desktop file")

(defvar desktop-default-buffer "*Scratch*"
  "Default buffer when creating a new desktop.")




(defvar desktop-prefix-char "\C-\\")

(defvar desktop-map nil
  "*Keymap for escreen commands.")
(cond
 ((null desktop-map)
  (setq desktop-map (make-sparse-keymap))
  (define-key desktop-map desktop-prefix-char 'desktop:last)
  (define-key desktop-map "c"    'desktop:create-new)
  (define-key desktop-map "g"    'desktop:switch-to)
  (define-key desktop-map "k"    'desktop:delete)
  (define-key desktop-map "l"    'desktop:display-current)
  (define-key desktop-map "r"    'desktop:rename)
  (define-key desktop-map "n"    'desktop:next)
  (define-key desktop-map "p"    'desktop:prev)))

(global-set-key desktop-prefix-char 'desktop-prefix)
(defalias 'desktop-prefix desktop-map)


(defface desktop-current-face '((t :foreground "#edd400"
				   :weight ultra-bold
				   :underline t)) "")
(defface desktop-previous-face '((t :foreground "#729fcf")) "")
(defface desktop-other-face '((t :foreground "#888a85"
				 :weight light)) "")



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
    (vconcat vector (list elt)))
   ((< id 0)
    (vconcat (list elt) vector))
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
  (make-desktop-window :buffer-name (buffer-name buffer)
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

(defun desktop:get-current(&optional name)
  "Return current desktop layout."
  (make-desktop :name (or name
			  (let ((desktop (desktop:get-by-id)))
			    (when (desktop-p desktop)
			      (desktop-name desktop)))
			  (format "Desktop %s" desktop-current))
		:window-tree (desktop:tree2list)
		:selected-window-path (desktop:get-window-path
				       (car (window-tree))
				       (selected-window))
		:buffer-list (loop for b in (buffer-list)
				   collect (desktop:buffer-to-desktop-window b))))

(defun desktop:get-by-id (&optional id)
  "Return desktop structure ID"
  (let ((id (or id desktop-current)))
    (when (and (>= id 0) (< id (length desktop-alist)))
      (elt desktop-alist id))))

(defun desktop:rename(name)
  "Rename current desktop to NAME."
  (interactive "MDesktop name: ")
  (desktop:save-current name)
  (message "Saved %s" name))

(defun desktop:save-current(&optional name)
  ""
  (setq desktop-alist
	(desktop:vinsert
	 (desktop:vdelete desktop-alist desktop-current)
	 (desktop:get-current name)
	 desktop-current)))

;;;###autoload
(defun desktop:create-new ()
  "Create a new desktop"
  (interactive)
  (desktop:save-current)
  (delete-other-windows)
  (setf desktop-current (length desktop-alist))
  (switch-to-buffer (get-buffer-create desktop-default-buffer))
  (desktop:save-current)
  (desktop:display-current))

;;;###autoload
(defun desktop:delete (&optional id)
  "Delete desktop ID or current."
  (interactive)
  (let ((current desktop-current))
    (desktop:prev)
    (setq desktop-alist (desktop:vdelete desktop-alist current)))
  (desktop:display-current))

;;;###autoload
(defun desktop:last ()
  "Activate previous desktop."
  (interactive)
  (desktop:restore desktop-previous))

;;;###autoload
(defun desktop:prev ()
  "Activate previous desktop."
  (interactive)
  (desktop:restore (1- desktop-current)))

;;;###autoload
(defun desktop:next ()
  "Activate next desktop."
  (interactive)
  (desktop:restore (1+ desktop-current)))

(defun desktop:display-current ()
  (interactive)
  (message
   (mapconcat #'identity
	      (loop for i below (length desktop-alist)
		    collect
		    (let ((face
			   (cond
			    ((= desktop-previous i) 'desktop-previous-face)
			    ((= desktop-current i) 'desktop-current-face)
			    (t 'desktop-other-face))))
		      (propertize (desktop-name (desktop:get-by-id i))
				  'face face)))
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
  (when (/= id desktop-current)
    (let* ((id
	    (cond
	     ((>= id (length desktop-alist)) 0)
	     ((< id 0) (1- (length desktop-alist)))
	     (t id)))
	   (desktop (desktop:get-by-id id)))
      (when desktop
	(desktop:save-current)
	(delete-other-windows)
	(desktop:list2tree (desktop-window-tree desktop))
	(setf desktop-previous desktop-current)
	(setf desktop-current id)
	(loop for b in (desktop-buffer-list desktop)
	      do (bury-buffer (get-buffer (desktop-window-buffer-name b))))
	(desktop:display-current)))))

(defun desktop:switch-to (&optional name)
  (interactive)
  (let ((name (or name
		  (completing-read "Switch to buffer: "
				   (loop for i across desktop-alist
					 collect (desktop-name i))
				   nil t
				   (desktop-name
				    (desktop:get-by-id desktop-previous))))))
    (loop for i below (length desktop-alist)
	  until (string= name (desktop-name (desktop:get-by-id i)))
	  finally (desktop:restore i))))


(defun desktop:save-session ()
  (interactive)
  (with-temp-file desktop-save-file
    (insert
     ";; -*- emacs-lisp -*-\n"
     ";; desktops saved sessions on " (format-time-string "%c") "\n"
     ";; You should not modify this file, but you can safely remove it\n"
     ";;\n\n"
     (format "(:desktop-current %d :desktop-previous %d :desktop-alist %S)"
	     desktop-current desktop-previous desktop-alist))))

(defun desktop:lazy-load (buffer-to-load)
  ""
  (loop for b in buffer-to-load
	do (progn
	     (find-file (desktop-window-file b))
	     (rename-buffer (desktop-window-buffer-name b)))))

;;;###autoload
(defun desktop:load-session ()
  (let ((plist (when (file-exists-p desktop-save-file)
		 (with-temp-buffer
		   (insert-file-contents-literally desktop-save-file)
		   (car (read-from-string (buffer-string))))))
	buffer-to-load)
    (when plist
      (message "Reading session")
      (setq
       desktop-current (plist-get plist :desktop-current)
       desktop-previous (plist-get plist :desktop-previous)
       desktop-alist (plist-get plist :desktop-alist))

      (loop for d across desktop-alist
	    do (loop for b in (desktop-buffer-list d)
		     do (when (desktop-window-file b)
			  (unless (get-buffer (desktop-window-buffer-name b))
			    (add-to-list 'buffer-to-load b)))))

      (run-at-time 2 nil 'desktop:lazy-load buffer-to-load))))
      
				     
				   
      

(provide 'desktops)

;; desktops.el ends here
