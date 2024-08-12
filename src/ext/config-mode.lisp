(defpackage :lem/config-mode
  (:use :cl :lem :lem-button)
  (:export
   ;; keymap
   :*config-mode-keymap*
   ;; functions
   :start-config-mode
   ;; editor variables
   ;; commands
   :config-mode
   :change-config
   :config-next
   :config-previous)
  #+sbcl
  (:lock t))
(in-package :lem/config-mode)

(define-minor-mode config-mode
    (:name "config"
     :keymap *config-mode-keymap*))

(define-key *config-mode-keymap* "Return" 'config-return)
(define-key *listener-mode-keymap* "C-p" 'config-previous)
(define-key *listener-mode-keymap* "C-n" 'config-next)

(defun start-config-mode ()
  (config-mode t))

(defun config-buffer-p (buffer)
  (mode-active-p buffer 'config-mode))

(defun config-buffer ()
  (or (get-buffer "*config*")
      (let ((buffer (make-buffer "*config*" :enable-undo-p nil)))
        (change-buffer-mode buffer 'config-mode)
        buffer)))

(defun all-configs ()
  (let ((res))
    (maphash (lambda (name conf)
               (let ((cat (symbol-package name)))
                 (push conf (getf res cat))))
             lem-core::*known-configs*)
    res))

(defun open-config ()
  (let ((buffer (config-buffer)))
    (flet ((body ()
             (let ((point (current-point))
                   (*inhibit-read-only* t))
               (erase-buffer buffer)
               (loop :for (cat confs . rest) := (all-configs) :then rest
                     :while cat :do
                        (insert-string point "~%----- ~a -----~%" (package-name cat) :bold t)
                        (dolist (conf confs)
                          (insert-but (lem-core::config-name)))))))
      (with-current-window (pop-to-buffer buffer)
        (body)))))

(defun default-switch-to-buffer (buffer)
  (switch-to-window (pop-to-buffer buffer)))

