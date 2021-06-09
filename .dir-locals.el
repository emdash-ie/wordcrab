;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode
  (haskell-indentation-left-offset . 2)
  (haskell-indentation-starter-offset . 2)
  (lsp-haskell-server-wrapper-function
   . (lambda (argv)
       (append
        (append (list "nix-shell" "-I" "." "--command")
                (list (mapconcat 'identity argv " ")))
        (list (concat (lsp-haskell--get-root) "/shell.nix"))))))
