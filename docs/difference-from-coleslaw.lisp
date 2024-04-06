
(defvar *templates-difference*
  "
Staticl config is constructed from lisp function calls and you can benefit from IDE's code completion.

Variable `config` was renamed to `site`.
   `post` renamed to `content`
   `pubdate` renamed to `site.pubdate`.

    index -> content
    index.content -> content.items
    prev -> content.prev
    next -> content.next
    tags -> content.tags
    obj.date -> obj.created_at


For templates base on Closure Template, StatiCL defines these filters:

* date - formats the a timestamp in this as YYYY-MM-DD
* datetime - formats a timestamp as YYYY-MM-DD HH:MM

To define additional filters, inherit your template class from CLOSURE-TEMPLATE and define a method for REGISTER-USER-FILTERS generic-function.

Instead of makin 1.html and symlinking to it from index.html, StatiCL just generates first page as index.html and other pages as 2.html, 3.html, etc.
")
