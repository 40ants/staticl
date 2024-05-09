(uiop:define-package #:staticl-docs/contribution
  (:use #:cl)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax))
(in-package #:staticl-docs/contribution)


(in-readtable pythonic-string-syntax)

(defsection @contribution (:title "Contribution")
  """
I'll be happy to discuss your ideas in the issues and to review your pull-requests.

Follow these few rules to make it be merged faster:

* Write a detailed description of the changes made in the pull request.
* Keep the same style as in the rest of code.
* If unsure, check [Google's Common Lisp Styleguide](https://google.github.io/styleguide/lispguide.xml).
* Write a test on added functionality.
* Describe your changes in the `docs/changelog.lisp` file. This way documentation and a ChangeLog.md file will be updated after the merge.
* Make changes to the documentation files inside the `docs/` folder.
* Ensure all checks on the pull request are green.

"""
  (@contributors section))


(defsection @contributors (:title "Contributors")
  """
* Alexander Artemenko (initial author).
"""
  )
