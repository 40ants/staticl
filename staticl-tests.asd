(defsystem "staticl-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/staticl/"
  :class :package-inferred-system
  :description "Provides tests for staticl."
  :source-control (:git "https://github.com/40ants/staticl")
  :bug-tracker "https://github.com/40ants/staticl/issues"
  :pathname "t"
  :depends-on ("staticl-tests/core")
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
