(defvar *templates-difference*
  "Variable `config` was renamed to `site`.
   `post` renamed to `content`
   `pubdate` renamed to `site.pubdate`.

For templates base on Closure Template, StatiCL defines these filters:

* date - formats the a timestamp in this as YYYY-MM-DD
* datetime - formats a timestamp as YYYY-MM-DD HH:MM

To define additional filters, inherit your template class from CLOSURE-TEMPLATE and define a method for REGISTER-USER-FILTERS generic-function.
")
