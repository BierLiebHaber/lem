(in-package :lem)

(export '(*lem-error-file*
          exit-lem
          describe-key
          begin-macro
          end-macro
          execute-macro
          apply-macro-to-region-lines
          universal-argument
          input-key
          undefined-key
          lem
          lem-save-error))

(defvar *lem-error-file* "~/.lem-error")
(defvar *init-flag* nil)

(defvar *exit*)

(defvar *macro-recording-p* nil)
(defvar *macro-chars* nil)
(defvar *macro-running-p* nil)

(defun macro-running-p () *macro-running-p*)

(let ((queue (make-tlist)))
  (defun getch (&optional (abort-jump t))
    (let* ((code (if (not (tlist-empty-p queue))
                   (tlist-rem-left queue)
                   (cl-ncurses:wgetch (window-win))))
           (char (code-char code)))
      (when *macro-recording-p*
        (push char *macro-chars*))
      (cond
       ((= code 410)
        (mb-resize)
        (adjust-screen-size)
        (getch))
       ((and (char= char key::ctrl-g) abort-jump)
        (throw 'abort 'abort))
       (t char))))
  (defun ungetch (c)
    (tlist-add-right queue (char-code c)))
  (defun getch-queue-length ()
    (length (car queue))))

(define-key *global-keymap* "C-g" 'keyboard-quit)
(define-command keyboard-quit () ()
  (setq *universal-argument* nil)
  (setq *macro-recording-p* nil)
  (write-message "Quit"))

(define-key *global-keymap* "C-xC-c" 'exit-lem)
(define-command exit-lem () ()
  (when (or (not (any-modified-buffer-p))
            (y-or-n-p "Modified buffers exist. Leave anyway"))
    (setq *exit* t)))

(define-key *global-keymap* "C-x?" 'describe-key)
(define-command describe-key () ()
  (write-message "describe-key: ")
  (let* ((key (input-key))
         (cmd (mode-find-keybind key)))
    (write-message (format nil "describe-key: ~a ~a"
                     (kbd-to-string key)
                     cmd))))

(define-key *global-keymap* "C-x(" 'begin-macro)
(define-command begin-macro () ()
  (cond (*macro-recording-p*
         (write-message "Macro already active")
         nil)
        (t 
         (write-message "Start macro")
         (setq *macro-recording-p* t)
         (setq *macro-chars* nil)
         t)))

(define-key *global-keymap* "C-x)" 'end-macro)
(define-command end-macro () ()
  (cond (*macro-running-p* t)
        ((not *macro-recording-p*)
         (write-message "Macro not active"))
        (t
         (setq *macro-recording-p* nil)
         (setq *macro-chars* (nreverse *macro-chars*))
         (write-message "End macro")
         t)))

(define-key *global-keymap* "C-xe" 'execute-macro)
(define-command execute-macro (n) ("p")
  (let ((*macro-running-p* t)
        (*universal-argument* nil))
    (loop repeat n while *macro-running-p* do
      (let ((length (getch-queue-length)))
        (dolist (c *macro-chars*)
          (ungetch c))
        (do ()
            ((or (not *macro-running-p*)
                 (>= length (getch-queue-length))))
            (main-step))))))

(define-command apply-macro-to-region-lines () ()
  (apply-region-lines (region-beginning)
                      (region-end)
                      (lambda ()
                        (execute-macro 1)))
  t)

(define-key *global-keymap* "C-u" 'universal-argument)
(define-command universal-argument () ()
  (let ((numlist)
        n)
    (do ((c (read-char "C-u 4")
            (read-char
             (format nil "C-u ~{~a~}" numlist))))
        (nil)
      (cond
       ((char= c key::ctrl-u)
        (setq numlist
          (mapcar 'digit-char-p
            (coerce
             (format nil "~a"
              (* 4
                (if numlist
                  (parse-integer
                   (format nil "~{~a~}" numlist))
                  4)))
             'list))))
       ((and (char= c #\-) (null numlist))
        (setq numlist (append numlist (list #\-))))
       ((setq n (digit-char-p c))
        (setq numlist
          (append numlist (list n))))
       (t
        (ungetch c)
        (setq *universal-argument*
          (if numlist
            (parse-integer (format nil "~{~a~}" numlist))
            4))
        (return (main-step)))))))

(defun input-key ()
  (let ((c (getch nil)))
    (if (or (char= c key::ctrl-x)
          (char= c key::escape))
      (list c (getch nil))
      (let ((bytes (utf8-bytes (char-code c))))
        (if (= bytes 1)
          (list c)
          (let ((bytes (coerce
                        (mapcar 'char-code
                          (cons c
                            (loop repeat (1- bytes)
                              collect (getch nil))))
                        '(vector (unsigned-byte 8)))))
            (list (aref (bytes-to-string bytes) 0))))))))

(defun execute (key)
  (let* ((keymap (current-mode-keymap))
         (cmd (mode-find-keybind key)))
    (if cmd
      (unless (cmd-call cmd *universal-argument*)
        (setq *macro-running-p* nil))
      (key-undef-hook keymap key))))

(defun main-step ()
  (let ((key (input-key)))
    (clear-message-line)
    (delete-completion-window)
    (execute key)
    (setq *universal-argument* nil)))

(defun undefined-key (key)
  (let ((c (insertion-key-p key)))
    (if c
      (insert-char c
        (or *universal-argument* 1))
      (write-message (format nil
                             "Key not found: ~a"
                             (kbd-to-string key))))))

(defun load-init-file ()
  (flet ((test (path)
               (when (file-exist-p path)
                 (load path)
                 (write-message (format nil "Load file: ~a" path))
                 t)))
    (or (test (merge-pathnames "lem.rc" (truename ".")))
        (test (merge-pathnames ".lemrc" (user-homedir-pathname))))))

(defun lem-init (args)
  (cl-ncurses:initscr)
  (cl-ncurses:noecho)
  (cl-ncurses:cbreak)
  (cl-ncurses:raw)
  (cl-ncurses:nonl)
  (cl-ncurses:refresh)
  (unless *init-flag*
    (setq *init-flag* t)
    (window-init)
    (mb-init)
    (load-init-file))
  (dolist (arg args)
    (find-file arg)))

(defun lem-finallize ()
  (cl-ncurses:endwin))

(defun lem-main ()
  (do ((*exit* nil)
       (*curr-flags* (make-flags) (make-flags))
       (*last-flags* (make-flags) *curr-flags*))
      (*exit*)
    (window-update-all)
    (case (catch 'abort
            (main-step)
            nil)
      (readonly
       (write-message "Read Only"))
      (abort
       (keyboard-quit)))))

(defun lem (&rest args)
  (unwind-protect
    (progn
      (lem-init args)
      (lem-main))
    (lem-finallize)))

(defun lem-save-error (&rest args)
  (let ((*print-circle* t))
    (with-open-file (*error-output* *lem-error-file*
                                    :direction :output
                                    :if-exists :overwrite
                                    :if-does-not-exist :create)
      (apply #'lem args))))
