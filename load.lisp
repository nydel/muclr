;; this simple file enables the user to just do (load "load") instead of typing
;; the following two forms out for evaluation every single time

(asdf::load-asd "muclr.asd")
(asdf:load-system :muclr)
