#-asdf3.1 (error "staticl requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "staticl"
  :description "Flexible and customizable static site generator with a lot of plugins!"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/staticl/"
  :source-control (:git "https://github.com/40ants/staticl")
  :bug-tracker "https://github.com/40ants/staticl/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("staticl/core"
               "staticl/content/defaults"
               "staticl/plugins/sitemap"
               "staticl/themes/closure-template"
               "staticl/format/html"
               "staticl/format/md"
               "staticl/user-package"
               "staticl/site-url")
  :in-order-to ((test-op (test-op "staticl-tests"))))



(asdf:register-system-packages "log4cl" '("LOG"))
(asdf:register-system-packages "3bmd-ext-code-blocks" '("3BMD-CODE-BLOCKS"))
(asdf:register-system-packages "fuzzy-dates" '("ORG.SHIRAKUMO.FUZZY-DATES"))
(asdf:register-system-packages "feeder" '("ORG.SHIRAKUMO.FEEDER"))

(asdf:register-system-packages "mystic" '("MYSTIC.UTIL"))
(asdf:register-system-packages "mystic-file-mixin" '("MYSTIC.TEMPLATE.FILE"))
