(defsystem "staticl-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/staticl/"
  :class :package-inferred-system
  :description "Provides documentation for staticl."
  :source-control (:git "https://github.com/40ants/staticl")
  :bug-tracker "https://github.com/40ants/staticl/issues"
  :pathname "docs"
  :depends-on ("staticl"
               "staticl-docs/index"))
