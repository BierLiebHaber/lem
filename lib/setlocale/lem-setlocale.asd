;;don't edit
(defsystem "lem-setlocale"
  :depends-on("lem")
  :class :package-inferred-system
  :components((#-darwin(:FILE "cffi")
               #+darwin(:FILE "cffi_darwin")))
  :author "SANO Masatoshi"
  :mailto "snmsts@gmail.com")
