;;; notmuch-multi-test.el --- Tests for notmuch-multi -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Philippe IVALDI
;;
;; Author: Philippe IVALDI <emacs@ivaldi.me>
;; Keywords: mail extensions lisp notmuch
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; ERT test suite for `notmuch-multi'.  Focuses on the per-account mail-fetch
;; feature (`:get-command', `notmuch-multi-index-command', the cursor->account
;; resolver, the async pipeline, and the "a <key-prefix> g" key install) plus a
;; few pure helpers used by the hello screen.
;;
;; These tests load `notmuch-multi', which `require's `notmuch' and
;; `notmuch-hello'.  They therefore need an Emacs where notmuch's Lisp is on the
;; `load-path' (e.g. your Doom Emacs instance); a bare `emacs -Q' fails with
;; "Cannot open load file: notmuch".  No notmuch *database* or CLI is required:
;; everything that would touch a real database is stubbed.
;;
;; Running them
;; ------------
;; Interactively (recommended, e.g. inside Doom):
;;
;;   M-x load-file RET .../test/notmuch-multi-test.el RET
;;   M-x ert RET t RET                  ; run every test
;;   ;; or narrow:  M-x ert RET "notmuch-multi-test-get-" RET
;;
;; Batch, if you know notmuch's Lisp directory NMDIR
;; (find it with  (file-name-directory (locate-library "notmuch"))  in your
;; running Emacs):
;;
;;   emacs -Q --batch -L . -L NMDIR \
;;     -l test/notmuch-multi-test.el -f ert-run-tests-batch-and-exit
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wid-edit)
(require 'notmuch-multi)

;;;; Helpers

(defun notmuch-multi-test--wait (proc &optional max-polls)
  "Block until PROC is no longer live, polling up to MAX-POLLS times.
MAX-POLLS defaults to 50 (~5s at 0.1s/poll); the cap keeps a hung
subprocess from wedging the whole test run."
  (let ((n (or max-polls 50)))
    (while (and (process-live-p proc) (> n 0))
      (accept-process-output proc 0.1)
      (setq n (1- n)))
    ;; The process has exited (or we timed out); drain once more so a
    ;; just-queued sentinel is guaranteed to have run before we assert.
    (accept-process-output nil 0.1)))

;;;; notmuch-multi-index-command (defcustom)

(ert-deftest notmuch-multi-test-index-command-default ()
  "`notmuch-multi-index-command' is defined and defaults to \"notmuch new\"."
  (should (boundp 'notmuch-multi-index-command))
  (should (equal notmuch-multi-index-command "notmuch new")))

;;;; notmuch-multi-account-plist (Customize widget)

(ert-deftest notmuch-multi-test-account-widget-converts ()
  "The account-plist widget converts without error."
  (should (widgetp (widget-convert 'notmuch-multi-account-plist))))

(ert-deftest notmuch-multi-test-account-widget-documents-get-command ()
  "The account-plist widget documentation mentions `:get-command'."
  (should (string-match-p
           ":get-command"
           (get 'notmuch-multi-account-plist 'widget-documentation))))

;;;; notmuch-multi-hello-filtered-query (pure)

(ert-deftest notmuch-multi-test-filtered-query-star ()
  "A \"*\" query returns the filter verbatim, not wrapped."
  (should (equal (notmuch-multi-hello-filtered-query "*" "tag:x") "tag:x")))

(ert-deftest notmuch-multi-test-filtered-query-combines ()
  "A non-\"*\" string query is ANDed with the filter."
  (should (equal (notmuch-multi-hello-filtered-query "tag:a" "tag:b")
                 "(tag:a) and (tag:b)")))

(ert-deftest notmuch-multi-test-filtered-query-function-filter ()
  "A function filter is called with the query."
  (should (equal (notmuch-multi-hello-filtered-query
                  "tag:a" (lambda (q) (concat "F:" q)))
                 "F:tag:a")))

(ert-deftest notmuch-multi-test-filtered-query-nil-filter ()
  "A nil (non-string, non-function) filter yields the bare query."
  (should (equal (notmuch-multi-hello-filtered-query "tag:a" nil) "tag:a")))

;;;; notmuch-multi--notmuch-remove-untags (pure)

(ert-deftest notmuch-multi-test-remove-untags-keeps-only-added ()
  "Only the `+'-prefixed tags survive; `-'-prefixed ones are dropped."
  (let ((r (notmuch-multi--notmuch-remove-untags
            '("+spam" "-inbox" "-unread" "+flagged"))))
    (should (member "+spam" r))
    (should (member "+flagged" r))
    (should-not (member "-inbox" r))
    (should-not (member "-unread" r))
    (should (= (length r) 2))))

;;;; notmuch-multi--ellipsid (pure)

(ert-deftest notmuch-multi-test-ellipsid-short-unchanged ()
  "A string shorter than LEN is returned without an ellipsis."
  (let ((s "Hello world"))
    (should (equal (notmuch-multi--ellipsid 85 s "…") s))))

(ert-deftest notmuch-multi-test-ellipsid-truncates-long ()
  "A string longer than LEN is truncated at a word and gains the ellipsis."
  (let* ((long (string-join (make-list 30 "word") " "))
         (out (notmuch-multi--ellipsid 20 long "…")))
    (should (string-suffix-p "…" out))
    (should (< (length out) (length long)))))

;;;; notmuch-multi--get-command (resolver)

(ert-deftest notmuch-multi-test-get-command-string ()
  "A string `:get-command' is returned as-is."
  (should (equal (notmuch-multi--get-command '(:name "FOO" :get-command "echo hi"))
                 "echo hi")))

(ert-deftest notmuch-multi-test-get-command-function ()
  "A function `:get-command' is called with the account."
  (should (equal (notmuch-multi--get-command
                  '(:name "FOO"
                    :get-command (lambda (a) (concat "echo " (plist-get a :name)))))
                 "echo FOO")))

(ert-deftest notmuch-multi-test-get-command-missing-errors ()
  "A missing `:get-command' signals a `user-error'."
  (should-error (notmuch-multi--get-command '(:name "FOO")) :type 'user-error))

(ert-deftest notmuch-multi-test-get-command-empty-errors ()
  "An empty-string `:get-command' signals a `user-error'."
  (should-error (notmuch-multi--get-command '(:name "FOO" :get-command ""))
                :type 'user-error))

;;;; notmuch-multi--account-at-point (cursor -> account)

(ert-deftest notmuch-multi-test-account-at-point-inside ()
  "Point inside a stamped section resolves to that section's account."
  (with-temp-buffer
    (let ((a1 '(:name "A1")))
      (insert "AAA")
      (put-text-property 1 (point) 'notmuch-multi-account a1)
      (goto-char 1)
      (should (eq (notmuch-multi--account-at-point) a1)))))

(ert-deftest notmuch-multi-test-account-at-point-gap ()
  "Point on the blank gap after a section resolves to the preceding account."
  (with-temp-buffer
    (let ((a1 '(:name "A1")) (a2 '(:name "A2")))
      (insert "AAA")
      (put-text-property 1 (point) 'notmuch-multi-account a1)
      (insert "\n")
      (let ((s (point))) (insert "BBB")
           (put-text-property s (point) 'notmuch-multi-account a2))
      (goto-char 4)                     ; the unstamped "\n"
      (should (eq (notmuch-multi--account-at-point) a1)))))

(ert-deftest notmuch-multi-test-account-at-point-second-section ()
  "Point inside the second section resolves to the second account."
  (with-temp-buffer
    (let ((a1 '(:name "A1")) (a2 '(:name "A2")))
      (insert "AAA")
      (put-text-property 1 (point) 'notmuch-multi-account a1)
      (insert "\n")
      (let ((s (point))) (insert "BBB")
           (put-text-property s (point) 'notmuch-multi-account a2))
      (goto-char 6)
      (should (eq (notmuch-multi--account-at-point) a2)))))

(ert-deftest notmuch-multi-test-account-at-point-none ()
  "With no stamped property anywhere, the resolver returns nil."
  (with-temp-buffer
    (insert "plain")
    (goto-char (point-min))
    (should (null (notmuch-multi--account-at-point)))))

;;;; notmuch-multi-hello-insert-account-searches (stamping)

(ert-deftest notmuch-multi-test-section-stamped ()
  "Inserting an account section stamps it with the `notmuch-multi-account' prop."
  (cl-letf (((symbol-function 'notmuch-multi-hello-insert-searches)
             (lambda (&rest _) (insert "SECTION-CONTENT"))))
    (with-temp-buffer
      (notmuch-multi-hello-insert-account-searches
       '(:account (:name "STAMP" :query "*") :searches nil))
      (goto-char (point-min))
      (should (equal (get-text-property (point) 'notmuch-multi-account)
                     '(:name "STAMP" :query "*"))))))

;;;; notmuch-multi--get-account-mail + notmuch-multi-get-mail-at-point (async)

(ert-deftest notmuch-multi-test-get-account-mail-success-refreshes ()
  "On a zero exit the originating hello buffer is refreshed."
  (let ((notmuch-multi-index-command "true")
        (refreshed nil))
    (cl-letf (((symbol-function 'notmuch-refresh-this-buffer)
               (lambda () (setq refreshed t))))
      (with-temp-buffer
        (notmuch-multi--get-account-mail '(:name "TESTOK" :get-command "true"))
        (let ((proc (get-process "notmuch-multi-get-TESTOK")))
          (should (processp proc))
          (notmuch-multi-test--wait proc)
          (should refreshed))))))

(ert-deftest notmuch-multi-test-get-account-mail-failure-shows-buffer ()
  "On a non-zero exit there is no refresh and the output buffer is displayed."
  (let ((notmuch-multi-index-command "true")
        (refreshed nil) (shown nil))
    (cl-letf (((symbol-function 'notmuch-refresh-this-buffer)
               (lambda () (setq refreshed t)))
              ((symbol-function 'display-buffer)
               (lambda (buf &rest _) (setq shown buf) nil)))
      (with-temp-buffer
        (notmuch-multi--get-account-mail '(:name "TESTKO" :get-command "false"))
        (let ((proc (get-process "notmuch-multi-get-TESTKO")))
          (notmuch-multi-test--wait proc)
          (should-not refreshed)
          (should shown))))))

(ert-deftest notmuch-multi-test-get-account-mail-missing-command-errors ()
  "Fetching an account with no `:get-command' signals a `user-error'."
  (should-error (notmuch-multi--get-account-mail '(:name "NOCMD"))
                :type 'user-error))

(ert-deftest notmuch-multi-test-get-mail-at-point-no-account-errors ()
  "`M-g' with no account at point signals a `user-error'."
  (with-temp-buffer
    (insert "x")
    (goto-char (point-min))
    (should-error (notmuch-multi-get-mail-at-point) :type 'user-error)))

;;;; notmuch-multi-accounts-saved-searches-set (a <key-prefix> g install)

(ert-deftest notmuch-multi-test-keys-installed ()
  "Configuring an account with a `:key-prefix' binds \"a <prefix> g\"."
  (let ((notmuch-hello-mode-map (make-sparse-keymap))
        (notmuch-multi--installed-get-keys nil)
        (notmuch-saved-searches nil)
        (notmuch-multi-accounts-saved-searches nil))
    (notmuch-multi-accounts-saved-searches-set
     '((:account (:name "ZED" :query "tag:z" :key-prefix "z" :get-command "true")
        :searches nil)))
    (should (commandp (lookup-key notmuch-hello-mode-map (kbd "a z g"))))))

(ert-deftest notmuch-multi-test-keys-stale-removed ()
  "Reconfiguring removes the previous account's binding and adds the new one."
  (let ((notmuch-hello-mode-map (make-sparse-keymap))
        (notmuch-multi--installed-get-keys nil)
        (notmuch-saved-searches nil)
        (notmuch-multi-accounts-saved-searches nil))
    (notmuch-multi-accounts-saved-searches-set
     '((:account (:name "ZED" :query "tag:z" :key-prefix "z" :get-command "true")
        :searches nil)))
    (should (commandp (lookup-key notmuch-hello-mode-map (kbd "a z g"))))
    (notmuch-multi-accounts-saved-searches-set
     '((:account (:name "WYE" :query "tag:y" :key-prefix "y" :get-command "true")
        :searches nil)))
    (should-not (commandp (lookup-key notmuch-hello-mode-map (kbd "a z g"))))
    (should (commandp (lookup-key notmuch-hello-mode-map (kbd "a y g"))))))

(ert-deftest notmuch-multi-test-keys-no-prefix-no-binding ()
  "An account with no `:key-prefix' (e.g. the default MAIN) installs no key."
  (let ((notmuch-hello-mode-map (make-sparse-keymap))
        (notmuch-multi--installed-get-keys nil)
        (notmuch-saved-searches nil)
        (notmuch-multi-accounts-saved-searches nil))
    (notmuch-multi-accounts-saved-searches-set
     '((:account (:name "MAIN" :query "*") :searches nil)))
    (should (null notmuch-multi--installed-get-keys))))

(provide 'notmuch-multi-test)
;;; notmuch-multi-test.el ends here
