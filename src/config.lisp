(in-package :lem-core)

(defun lem-home ()
  (let ((xdg-lem (uiop:xdg-config-home "lem/"))
	(dot-lem (merge-pathnames ".lem/" (user-homedir-pathname))))
    (or (uiop:getenv "LEM_HOME")
	(and (probe-file dot-lem) dot-lem)
	xdg-lem)))

(defun lem-logdir-pathname ()
  (merge-pathnames "logs/" (lem-home)))

(defun config-pathname ()
  (merge-pathnames "config.lisp" (lem-home)))

(defun ensure-config-pathname ()
  (ensure-directories-exist (config-pathname)))

(defstruct (config-var (:constructor %make-config-var))
  name default doc-string value-changed-hook value-requester)

(defvar *known-configs* (make-hash-table)
  "Hashtable of all known config-vars")

(defmacro defconfig ((name &optional default doc-string)
                     &key (value-requester #'prompt-for-string) value-changed-hook)
  "Define a new config-variable called `name` that can be saved/restored from `{lem-home}/config.lisp`.
  - `value-changed-hook` is called with `new-val` whenever the value is changed with
    `(setf (config my-config) 'new-value)
  - `value-requester` is a function to request an options in config-mode"
  (alexandria:once-only (default)
  `(setf (gethash (quote ,name) *known-configs*)
         (%make-config-var :name (quote ,name) :default ,default
                           :doc-string ,doc-string
                           :value-changed-hook ,value-changed-hook
                           :value-requester ,value-requester))))

(defun config-plist ()
  (let ((pathname (ensure-config-pathname)))
    (if (uiop:file-exists-p pathname)
        (ignore-errors (uiop:read-file-form pathname))
        '())))

(defun config-var (key &optional default)
  (let ((plist (config-plist)))
    (getf plist key (let ((val (gethash key *known-configs*)))
                      (or (and (config-var-p val) (config-var-default val))
                          (warn "Trying to load unknown config: ~s, default: ~s" key default)
                          default)))))

(defun (setf config-var) (value key)
  ;; The hook gets called first so if the config breaks something immidiatly we
  ;; ideally don't save the config
  (print 'test)
  (alexandria:when-let ((hook (config-var-value-changed-hook
                               (gethash key *known-configs*))))
    (funcall hook value))
  (let ((plist (config-plist)))
    (if (null plist)
        (setf plist (list key value))
        (setf (getf plist key) value))
    (with-open-file (out (ensure-config-pathname)
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (let ((*package* (find-package :keyword)))
        (pprint plist out))))
  value)

(defmacro config (key &optional default)
  `(config-var (quote ,key) ,default))

(define-setf-expander config (name &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion name env)
    (declare (ignorable newval setter))
    (let ((store (gensym)))
      (values dummies
              vals
              `(,store)
              `(setf (config-var (quote ,getter)) ,store)
              `(config-var (quote ,getter))))))