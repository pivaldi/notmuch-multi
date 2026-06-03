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

(ert-deftest notmuch-multi-test-schedule-verbose-default ()
  "`notmuch-multi-schedule-verbose' is defined and defaults to t."
  (should (boundp 'notmuch-multi-schedule-verbose))
  (should (eq notmuch-multi-schedule-verbose t)))

;;;; notmuch-multi-account-plist (Customize widget)

(ert-deftest notmuch-multi-test-account-widget-converts ()
  "The account-plist widget converts without error."
  (should (widgetp (widget-convert 'notmuch-multi-account-plist))))

(ert-deftest notmuch-multi-test-account-widget-documents-get-command ()
  "The account-plist widget documentation mentions `:get-command'."
  (should (string-match-p
           ":get-command"
           (get 'notmuch-multi-account-plist 'widget-documentation))))

(ert-deftest notmuch-multi-test-account-widget-documents-get-interval ()
  "The account-plist widget documentation mentions `:get-interval'."
  (should (string-match-p
           ":get-interval"
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

;;;; Scheduler — interval validation

(ert-deftest notmuch-multi-test-valid-interval-p ()
  "`notmuch-multi--valid-interval-p' accepts positive numbers and non-empty
strings, and rejects nil, zero, negatives and the empty string."
  (should (notmuch-multi--valid-interval-p 300))
  (should (notmuch-multi--valid-interval-p 0.5))
  (should (notmuch-multi--valid-interval-p "5 min"))
  (should-not (notmuch-multi--valid-interval-p nil))
  (should-not (notmuch-multi--valid-interval-p 0))
  (should-not (notmuch-multi--valid-interval-p -10))
  (should-not (notmuch-multi--valid-interval-p "")))

(ert-deftest notmuch-multi-test-schedulable-account-p ()
  "An account is schedulable only with a usable :get-command and a valid
:get-interval."
  (should (notmuch-multi--schedulable-account-p
           '(:name "OK" :get-command "true" :get-interval 300)))
  (should (notmuch-multi--schedulable-account-p
           '(:name "FN" :get-command (lambda (_a) "true") :get-interval "5 min")))
  (should-not (notmuch-multi--schedulable-account-p
               '(:name "NOINT" :get-command "true")))
  (should-not (notmuch-multi--schedulable-account-p
               '(:name "NOCMD" :get-interval 300)))
  (should-not (notmuch-multi--schedulable-account-p
               '(:name "EMPTYCMD" :get-command "" :get-interval 300)))
  (should-not (notmuch-multi--schedulable-account-p
               '(:name "BADINT" :get-command "true" :get-interval 0))))

(ert-deftest notmuch-multi-test-schedule-start-arms-one-per-account ()
  "`-start' arms one timer per schedulable account, each calling the core
fetch with scheduled = t."
  (let ((notmuch-multi--schedule-timers nil)
        (notmuch-multi-accounts-saved-searches
         '((:account (:name "A" :query "*" :get-command "true" :get-interval 300)
            :searches nil)
           (:account (:name "B" :query "*" :get-command "true" :get-interval 600)
            :searches nil)))
        (calls '()))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (first repeat fn &rest args)
                 (push (list first repeat fn args) calls)
                 'fake-timer)))
      (notmuch-multi-schedule-start)
      (should (= (length notmuch-multi--schedule-timers) 2))
      (should (= (length calls) 2))
      (dolist (c calls)
        (should (eq (nth 2 c) #'notmuch-multi--get-account-mail))
        (should (eq (nth 1 (nth 3 c)) t))
        (should (memq (nth 1 c) '(300 600)))))))

(ert-deftest notmuch-multi-test-schedule-start-staggers-first-fire ()
  "Numeric first-fire times include the per-account stagger offset."
  (let ((notmuch-multi--schedule-timers nil)
        (notmuch-multi--schedule-stagger 7)
        (notmuch-multi-accounts-saved-searches
         '((:account (:name "A" :query "*" :get-command "true" :get-interval 300)
            :searches nil)
           (:account (:name "B" :query "*" :get-command "true" :get-interval 300)
            :searches nil)))
        (firsts '()))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (first _repeat _fn &rest _) (push first firsts) 'fake)))
      (notmuch-multi-schedule-start)
      (should (equal (nreverse firsts) '(300 307))))))

(ert-deftest notmuch-multi-test-schedule-start-skips-ineligible ()
  "Accounts lacking :get-command or :get-interval are not armed."
  (let ((notmuch-multi--schedule-timers nil)
        (notmuch-multi-accounts-saved-searches
         '((:account (:name "OK" :query "*" :get-command "true" :get-interval 300)
            :searches nil)
           (:account (:name "NOINT" :query "*" :get-command "true") :searches nil)
           (:account (:name "NOCMD" :query "*" :get-interval 300) :searches nil)))
        (n 0))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) (setq n (1+ n)) 'fake)))
      (notmuch-multi-schedule-start)
      (should (= n 1)))))

(ert-deftest notmuch-multi-test-schedule-start-idempotent ()
  "Re-running `-start' cancels the prior batch; no timer accumulation."
  (let ((notmuch-multi--schedule-timers nil)
        (cancelled 0)
        (notmuch-multi-accounts-saved-searches
         '((:account (:name "A" :query "*" :get-command "true" :get-interval 300)
            :searches nil))))
    (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'fake))
              ((symbol-function 'cancel-timer)
               (lambda (_) (setq cancelled (1+ cancelled)))))
      (notmuch-multi-schedule-start)
      (should (= (length notmuch-multi--schedule-timers) 1))
      (notmuch-multi-schedule-start)
      (should (= (length notmuch-multi--schedule-timers) 1))
      (should (= cancelled 1)))))

(ert-deftest notmuch-multi-test-schedule-stop-cancels-all ()
  "`-stop' cancels every timer and clears the list."
  (let ((notmuch-multi--schedule-timers (list 't1 't2 't3))
        (cancelled '()))
    (cl-letf (((symbol-function 'cancel-timer)
               (lambda (tm) (push tm cancelled))))
      (notmuch-multi-schedule-stop)
      (should (null notmuch-multi--schedule-timers))
      (should (= (length cancelled) 3)))))

(ert-deftest notmuch-multi-test-schedule-stop-empty-noop ()
  "`-stop' with no timers does nothing and leaves the list nil."
  (let ((notmuch-multi--schedule-timers nil))
    (notmuch-multi-schedule-stop)
    (should (null notmuch-multi--schedule-timers))))

(ert-deftest notmuch-multi-test-schedule-status-not-running ()
  "`-status' reports not running when no timers are armed."
  (let ((notmuch-multi--schedule-timers nil))
    (should (string-match-p "not running" (notmuch-multi-schedule-status)))))

(ert-deftest notmuch-multi-test-schedule-status-lists-accounts ()
  "`-status' lists schedulable account names and intervals when running."
  (let ((notmuch-multi--schedule-timers (list 'fake))
        (notmuch-multi-accounts-saved-searches
         '((:account (:name "ALPHA" :query "*" :get-command "true" :get-interval 300)
            :searches nil)
           (:account (:name "NOCMD" :query "*" :get-interval 300) :searches nil))))
    (let ((out (notmuch-multi-schedule-status)))
      (should (string-match-p "ALPHA" out))
      (should (string-match-p "300" out))
      (should-not (string-match-p "NOCMD" out)))))

(ert-deftest notmuch-multi-test-refresh-hello-buffers-refreshes-hello ()
  "`notmuch-multi--refresh-hello-buffers' refreshes a hello-mode buffer."
  (let ((count 0)
        (buf (generate-new-buffer "*nm-hello-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'notmuch-refresh-this-buffer)
                   (lambda () (setq count (1+ count)))))
          (with-current-buffer buf (setq major-mode 'notmuch-hello-mode))
          (notmuch-multi--refresh-hello-buffers)
          (should (= count 1)))
      (kill-buffer buf))))

(ert-deftest notmuch-multi-test-refresh-hello-buffers-skips-others ()
  "Non-hello buffers are not refreshed."
  (let ((count 0)
        (buf (generate-new-buffer "*nm-plain-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'notmuch-refresh-this-buffer)
                   (lambda () (setq count (1+ count)))))
          ;; buf stays in fundamental-mode
          (notmuch-multi--refresh-hello-buffers)
          (should (= count 0)))
      (kill-buffer buf))))

(ert-deftest notmuch-multi-test-scheduled-refreshes-hello-buffers ()
  "On a zero exit a scheduled fetch refreshes all hello buffers."
  (let ((notmuch-multi-index-command "true")
        (refreshed nil))
    (cl-letf (((symbol-function 'notmuch-multi--refresh-hello-buffers)
               (lambda () (setq refreshed t))))
      (notmuch-multi--get-account-mail '(:name "SOK" :get-command "true") t)
      (let ((proc (get-process "notmuch-multi-get-SOK")))
        (should (processp proc))
        (notmuch-multi-test--wait proc)
        (should refreshed)))))

(ert-deftest notmuch-multi-test-scheduled-skip-when-running ()
  "A scheduled fetch is skipped (no new process, no error) when the account
is already fetching."
  (let ((notmuch-multi-index-command "true")
        (made nil)
        (dummy (start-process "notmuch-multi-get-BUSY" nil "sleep" "2")))
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest _) (setq made t) nil)))
          (notmuch-multi--get-account-mail '(:name "BUSY" :get-command "true") t)
          (should-not made))
      (when (process-live-p dummy) (delete-process dummy)))))

(ert-deftest notmuch-multi-test-interactive-errors-when-running ()
  "Interactively fetching an account already fetching signals a `user-error'."
  (let ((notmuch-multi-index-command "true")
        (dummy (start-process "notmuch-multi-get-BUSY2" nil "sleep" "2")))
    (unwind-protect
        (should-error
         (notmuch-multi--get-account-mail '(:name "BUSY2" :get-command "true"))
         :type 'user-error)
      (when (process-live-p dummy) (delete-process dummy)))))

(ert-deftest notmuch-multi-test-scheduled-quiet-suppresses-messages ()
  "With `notmuch-multi-schedule-verbose' nil, a scheduled success is silent."
  (let ((notmuch-multi-index-command "true")
        (notmuch-multi-schedule-verbose nil)
        (msgs '()))
    (cl-letf (((symbol-function 'notmuch-multi--refresh-hello-buffers)
               (lambda () nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) msgs)) nil)))
      (notmuch-multi--get-account-mail '(:name "QUIET" :get-command "true") t)
      (let ((proc (get-process "notmuch-multi-get-QUIET")))
        (notmuch-multi-test--wait proc)
        (should-not (cl-some (lambda (m) (string-match-p "QUIET" m)) msgs))))))

(provide 'notmuch-multi-test)
;;; notmuch-multi-test.el ends here
