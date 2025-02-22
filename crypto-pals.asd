(defsystem "crypto-pals"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:ironclad :dialectic)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("encodings"))
                 (:file "encodings"))))
  :description ""
  :in-order-to ((test-op (test-op "crypto-pals/tests"))))

(defsystem "crypto-pals/tests"
  :author ""
  :license ""
  :depends-on ("crypto-pals"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for crypto-pals"
  :perform (test-op (op c) (symbol-call :rove :run c)))
