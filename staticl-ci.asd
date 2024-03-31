(defsystem "staticl-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/staticl/"
  :class :package-inferred-system
  :description "Provides CI settings for staticl."
  :source-control (:git "https://github.com/40ants/staticl")
  :bug-tracker "https://github.com/40ants/staticl/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "staticl-ci/ci"))
