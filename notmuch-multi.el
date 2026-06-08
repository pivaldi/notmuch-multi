;;; notmuch-multi.el --- Prettified Notmuch UI For Multiple Accounts -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Philippe IVALDI
;;
;; Author: Philippe IVALDI <emacs@ivaldi.me>
;; Maintainer: Philippe IVALDI <emacs@ivaldi.me>
;; Created: January 09, 2025
;; Version: 0.2.2
;; Keywords: mail notmuch
;; Homepage: https://github.com/pivaldi/notmuch-multi
;; Package-Requires: ((emacs "29") (notmuch "0.38.3"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Most code is inspired from
;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-emacs-modules/prot-emacs-notmuch.el?ref_type=heads
;; https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el
;;
;;; Code:

(require 'notmuch)
(require 'notmuch-hello)
(require 'notmuch-address)
(require 'notmuch-mua)
(require 'mailabbrev)
(require 'mail-extr)

(defgroup notmuch-multi ()
  "Extensions for notmuch.el."
  :group 'notmuch)

(defcustom notmuch-multi-delete-tag "delete"
  "Single tag that applies to mail marked for deletion.
This is used by `notmuch-multi-delete-mail'."
  :type 'string
  :group 'notmuch-multi)

(defcustom notmuch-multi-expire-tag "expire"
  "Single tag that applies to mail marked for expiration.

A message tagged as expirable will be removed after
`notmuch-multi-expire-delay' days.
This is used by `notmuch-multi-expire-mail'."
  :type 'string
  :group 'notmuch-multi)

(defcustom notmuch-multi-expire-delay 90
  "The age in days after which messages marked as expirable will be deleted.
See `notmuch-multi-expire-tag' and `notmuch-multi-expire-mail'."
  :type 'integer
  :group 'notmuch-multi)


(defcustom notmuch-multi-mark-delete-tags
  `(,(format "+%s" notmuch-multi-delete-tag) "-inbox" "-unread")
  "List of tags to mark for deletion.
To actually delete email, refer to `notmuch-multi-delete-mail'."
  :type '(repeat string)
  :group 'notmuch-multi)

(defcustom notmuch-multi-mark-expire-tags
  `(,(format "+%s" notmuch-multi-expire-tag) "-inbox" "-unread")
  "List of tags to mark for expiration.
To delete expired emails, refer to `notmuch-multi-expire-mail'."
  :type '(repeat string)
  :group 'notmuch-multi)

(defcustom notmuch-multi-mark-flag-tags
  `(
    "+flagged" "-unread" "-spam"
    ,(format "-%s" notmuch-multi-delete-tag)
    ,(format "-%s" notmuch-multi-expire-tag))
  "List of tags to mark as important (flagged).
This gets the `notmuch-tag-flagged' face, if that is specified in
`notmuch-tag-formats'."
  :type '(repeat string)
  :group 'notmuch-multi)

(defcustom notmuch-multi-mark-spam-tags '("+spam" "-inbox" "-unread" "-archive")
  "List of tags to mark as spam."
  :type '(repeat string)
  :group 'notmuch-multi)

(defcustom notmuch-multi-index-command "notmuch new"
  "Shell command run after an account's `:get-command' to index fetched mail.
Run by `notmuch-multi-get-mail-at-point' and the per-account
\"a <key-prefix> g\" bindings.  Set to e.g. \"notmuch new --no-hooks\" to
skip notmuch hooks."
  :type 'string
  :group 'notmuch-multi)

(defcustom notmuch-multi-schedule-verbose t
  "Whether scheduled fetches report progress in the echo area.
When non-nil, a timer-driven fetch emits the same \"Fetching…\" and
\"fetched and indexed\" messages as the interactive
`notmuch-multi-get-mail-at-point'.  When nil, successful scheduled
fetches run silently.  Fetch *failures* are always reported regardless
of this setting.  Has no effect on interactive fetches, which always
message.  See `notmuch-multi-schedule-start'."
  :type 'boolean
  :group 'notmuch-multi)

(defun notmuch-multi-address-default-prefix-matcher (prefix)
  "Return the default notmuch query clause matching PREFIX in From or Cc.
PREFIX is the text typed in the recipient header.  The clause is wrapped in
parentheses by the caller (`notmuch-multi--address-query'), so it returns the
bare disjunction.  Double quotes are stripped from PREFIX first: it is
interpolated into a quoted notmuch phrase (\"PREFIX*\"), so an embedded quote
\(e.g. from a half-typed display name) would otherwise produce a malformed
query."
  (let ((prefix (replace-regexp-in-string "\"" "" prefix)))
    (format "from:\"%s*\" or cc:\"%s*\"" prefix prefix)))

(defcustom notmuch-multi-address-prefix-matcher
  #'notmuch-multi-address-default-prefix-matcher
  "Function building the notmuch query clause that filters candidates by PREFIX.
Called with the typed PREFIX string; must return a notmuch query string.  The
default, `notmuch-multi-address-default-prefix-matcher', returns
\"from:\\\"PREFIX*\\\" or cc:\\\"PREFIX*\\\"\".  See
`notmuch-multi-address-complete'."
  :type 'function
  :group 'notmuch-multi)

(defcustom notmuch-multi-address-command-flags
  '("--output=sender" "--output=recipients" "--output=count" "--deduplicate=address")
  "Flags passed to `notmuch address' when harvesting completion candidates.
Both `--output=sender' and `--output=recipients' are included by default,
so candidates come from the From and the To/Cc/Bcc headers of matching
messages; remove `--output=recipients' to restrict candidates to senders.
`--output=count' makes notmuch report a per-address message count, used
to order candidates (descending).  `--format=sexp' is always added by the
package and must not be set here.  See `notmuch-multi-address-complete'."
  :type '(repeat string)
  :group 'notmuch-multi)

(defun notmuch-multi--address-query (account prefix)
  "Build the `notmuch address' argument list for ACCOUNT and typed PREFIX.
ACCOUNT is an account plist with a non-nil `:address-term'.  PREFIX is the
recipient token typed so far (may be empty).  Returns the list of string
arguments to pass after the notmuch program name: \"address\",
\"--format=sexp\", the flags from `notmuch-multi-address-command-flags',
and a single combined search-terms argument.  When PREFIX is non-empty,
the clause from `notmuch-multi-address-prefix-matcher' is parenthesized
and ANDed (by juxtaposition) with the account's `:address-term'."
  (let* ((address-term (plist-get account :address-term))
         (clause (and prefix (not (string= "" prefix))
                      (funcall notmuch-multi-address-prefix-matcher prefix)))
         (query (if clause
                    (format "%s ( %s )" address-term clause)
                  address-term)))
    (append '("address" "--format=sexp")
            notmuch-multi-address-command-flags
            (list query))))

(define-widget 'notmuch-multi-account-plist 'list
  "An email account.

An account is a plist. Supported properties are

  :name         The name of the account (required).
  :query        Search for all the email for this account (required).
  :key-prefix   Optional shortcut key prefix for `notmuch-jump-search' and the
                \"a <key-prefix> g\" mail-fetch binding, relative to this account.
  :get-command  Optional shell command (string) run to fetch this account's mail
                via `notmuch-multi-get-mail-at-point' or \"a <key-prefix> g\".
  :get-interval Optional fetch cadence for the scheduler: a number of seconds, or
                any value `run-at-time' accepts as REPEAT (e.g. \"5 min\").  An
                account is auto-fetched only when it has both :get-command and a
                positive :get-interval.  See `notmuch-multi-schedule-start'.
  :address-term A notmuch query selecting this account's mail, used to scope
                address completion (see `notmuch-multi-address-complete').
                notmuch wildcards are trailing-only: use \"to:@ivaldi.me\"
                (matches all aliases on the domain), not \"to:*@ivaldi.me\".
  :send-as      A regexp, or list of regexps, matched against the composed
                From: address to recognize this account when composing.
"
  :tag "Account Definition"
  :args '((list :inline t
           :format "%v"
           (group :format "%v" :inline t
                  (const :format "  Name: " :name)
                  (string :format "%v"))
           (group :format "%v" :inline t
                  (const :format "  Query: " :query)
                  (string :format "%v"))
           (group :format "%v" :inline t
                  (const :format "  Shortcut key prefix: " :key-prefix)
                  (string :format "%v"))
           (group :format "%v" :inline t
                  (const :format "  Get-mail command: " :get-command)
                  (string :format "%v"))
           (group :format "%v" :inline t
                  (const :format "  Get-mail interval: " :get-interval)
                  (sexp :format "%v"))
           (group :format "%v" :inline t
                  (const :format "  Address-term (completion scope): " :address-term)
                  (string :format "%v"))
           (group :format "%v" :inline t
                  (const :format "  Send-as (From: match): " :send-as)
                  (sexp :format "%v")))))

(define-widget 'notmuch-multi-accounts-saved-searches-plist 'list
  "A set of accounts associated with his saved searches list."
  :tag "Associated Account Searches"
  :args '((list :inline t
           :format "%v"
           (group :format "%v" :inline t
                  (const :format "" :account)
                  notmuch-multi-account-plist)
           (group :format "%v" :inline t
                  (const :format "Account Searches:\n" :searches)
                  (repeat :tag "Search" notmuch-saved-search-plist)))))

(defun notmuch-multi-hello-filtered-query (query filter)
  "Constructs a query to search all messages matching QUERY and FILTER.
Modified version of `notmuch-hello-filtered-query' to handle query equal to *."
  (cond
   ((functionp filter) (funcall filter query))
   ((stringp filter)
    (if (string= query "*") filter
      (concat "(" query ") and (" filter ")")))
   (t query)))

(defvar notmuch-multi--installed-get-keys nil
  "Key sequences bound in `notmuch-hello-mode-map' for per-account mail fetch.
Maintained by `notmuch-multi-accounts-saved-searches-set' so stale bindings
are removed when accounts are reconfigured.")

;; Forward declaration: the setter below assigns this variable, while the
;; defcustom that defines it follows the setter (so the defcustom's :set can
;; call the setter at load time).  A bare `defvar' silences the byte-compiler's
;; free-variable warning without making the symbol `boundp', so the defcustom
;; still initializes through its :set.
(defvar notmuch-multi-accounts-saved-searches)

;;;###autoload
(defun notmuch-multi-accounts-saved-searches-set (accounts-searches)
  "Set `notmuch-multi-accounts-saved-searches' to ACCOUNTS-SEARCHES.
Push the searches from each account in ACCOUNTS-SEARCHES into
`notmuch-saved-searches' with computed name, key and query.

Also (re)install the per-account \"a <key-prefix> g\" mail-fetch bindings in
`notmuch-hello-mode-map' for accounts that define a non-empty `:key-prefix'.

Must be used instead of setq."
  (setq notmuch-multi-accounts-saved-searches accounts-searches)
  (dolist (key notmuch-multi--installed-get-keys)
    (define-key notmuch-hello-mode-map key nil))
  (setq notmuch-multi--installed-get-keys nil)
  (dolist (account-searches accounts-searches)
    (let* ((searches (plist-get account-searches :searches))
           (account (plist-get account-searches :account))
           (account-name (plist-get account :name))
           (account-query (plist-get account :query))
           (kprefix (plist-get account :key-prefix)))
      (when (and kprefix (not (string= kprefix "")))
        (let ((key (kbd (concat "a " kprefix " g")))
              (acct account))
          (define-key notmuch-hello-mode-map key
                      (lambda () (interactive) (notmuch-multi--get-account-mail acct)))
          (push key notmuch-multi--installed-get-keys)))
      (dolist (search searches)
        (let ((s (copy-sequence search)))
          (when search
            (when (not (string= kprefix ""))
              (let ((keydesc (key-description (plist-get search :key))))
                (plist-put s :key (kbd (concat kprefix keydesc))))))
          (plist-put s :name (concat account-name " - " (plist-get search :name)))
          (plist-put s :query (notmuch-multi-hello-filtered-query account-query (plist-get search :query)))
          (add-to-list 'notmuch-saved-searches s))))) notmuch-saved-searches)

(defcustom notmuch-multi-accounts-saved-searches
  `((:account (:name "MAIN" :query "*")
     :searches ,notmuch-saved-searches))
  "A list of email account associated with `notmuch-saved-searches'.

The saved accounts searches is a list of plist.
Supported properties of the plist are:

  :account         A `notmuch-multi-account-plist (required)'.
  :searches        A `notmuch-saved-searches' (required)."
  :type '(repeat :tag "Account" notmuch-multi-accounts-saved-searches-plist)
  :tag "List of Accounts"
  :set (lambda (symbol value)
         (set-default symbol value)
         (notmuch-multi-accounts-saved-searches-set value))
  :group 'notmuch-multi)


(defun notmuch-multi--send-as-match-p (send-as addr)
  "Return non-nil when ADDR matches SEND-AS.
SEND-AS is a regexp, a list of regexps (matches if any element matches), or
nil (never matches)."
  (cond
   ((null send-as) nil)
   ((listp send-as)
    (let ((match nil))
      (dolist (re send-as)
        (when (and (not match) (string-match-p re addr))
          (setq match t)))
      match))
   (t (and (string-match-p send-as addr) t))))

(defun notmuch-multi--address-account ()
  "Return the account plist whose `:send-as' matches the composed From:, or nil.
Read the From: header of the current message buffer, take the bare address,
and return the first account in `notmuch-multi-accounts-saved-searches' that
defines both `:address-term' and a matching `:send-as'."
  (let* ((from (message-field-value "from"))
         (addr (and from (cadr (mail-extract-address-components from))))
         (found nil))
    (when addr
      (dolist (account-searches notmuch-multi-accounts-saved-searches)
        (let ((account (plist-get account-searches :account)))
          (when (and (not found)
                     (plist-get account :address-term)
                     (notmuch-multi--send-as-match-p
                      (plist-get account :send-as) addr))
            (setq found account)))))
    found))

(defun notmuch-multi--address-candidates (account prefix)
  "Return ACCOUNT's address candidates for typed PREFIX, ordered by frequency.
Run `notmuch address' (see `notmuch-multi--address-query') synchronously, read
its `--format=sexp' output (a list of address plists), sort the plists by
`:count' descending, and return the `:name-addr' strings.  Signal an error when
notmuch exits non-zero."
  (with-temp-buffer
    (let ((status (apply #'call-process notmuch-command nil t nil
                         (notmuch-multi--address-query account prefix))))
      (unless (eq status 0)
        (error "Notmuch address failed (exit %s)" status))
      (goto-char (point-min))
      (let ((data (unless (eobp) (read (current-buffer)))))
        (mapcar (lambda (plist) (plist-get plist :name-addr))
                (sort (copy-sequence data)
                      (lambda (a b)
                        (> (or (plist-get a :count) 0)
                           (or (plist-get b :count) 0)))))))))

(defun notmuch-multi--address-bounds ()
  "Return (BEG . END) bounding the recipient token at point, or nil.
Return non-nil only when point is within a header matched by the variable
`notmuch-address-completion-headers-regexp'.  The token runs from after the
previous comma or the header colon (past leading whitespace) to the next comma
or end of line (excluding trailing whitespace).
An empty or whitespace-only slot yields zero-width bounds at point (BEG = END),
so completion can offer all of the account's correspondents.
A comma inside a quoted display name (e.g. \"Doe, John\" <j@x>) is treated as a
token boundary, matching upstream notmuch address completion."
  (when (let ((mail-abbrev-mode-regexp notmuch-address-completion-headers-regexp))
          (mail-abbrev-in-expansion-header-p))
    (let ((beg (save-excursion
                 (skip-chars-backward "^:,\n")
                 (skip-chars-forward " \t")
                 (point)))
          (end (save-excursion
                 (skip-chars-forward "^,\n")
                 (skip-chars-backward " \t")
                 (point))))
      (if (<= beg end)
          (cons beg end)
        (cons (point) (point))))))

(defun notmuch-multi--address-capf ()
  "Account-scoped `completion-at-point' function for recipient headers.
Return nil (so it composes harmlessly) unless both an active account and a
recipient token are found.  The returned collection keeps the descending-count
order from `notmuch-multi--address-candidates' by declaring `identity' sort
functions in its metadata, and appends \", \" after a finished selection."
  (let ((account (notmuch-multi--address-account))
        (bounds (notmuch-multi--address-bounds)))
    (when (and account bounds)
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (prefix (buffer-substring-no-properties beg end))
             (candidates (notmuch-multi--address-candidates account prefix)))
        (list beg end
              (lambda (string pred action)
                (if (eq action 'metadata)
                    '(metadata (display-sort-function . identity)
                      (cycle-sort-function . identity))
                  (complete-with-action action candidates string pred)))
              :exclusive 'no
              :exit-function
              (lambda (_string status)
                (when (memq status '(finished sole))
                  (insert ", "))))))))

;;;###autoload
(defun notmuch-multi-address-complete ()
  "Complete the recipient at point using the active account's addresses.
The active account is the one whose `:send-as' matches the message From:
header (see `notmuch-multi--address-account').  Candidates come from
`notmuch address' scoped by the account's `:address-term', ordered by
descending message count -- account-scoped, unlike notmuch's native global
completion.  When point is outside a recipient header or no account matches,
report and do nothing."
  (interactive)
  (cond
   ((not (notmuch-multi--address-bounds))
    (message "Not in a recipient header"))
   ((not (notmuch-multi--address-account))
    (message "No notmuch-multi account matches From:; use <TAB> for global completion"))
   (t
    (let ((completion-ignore-case t)
          (completion-at-point-functions (list #'notmuch-multi--address-capf)))
      (completion-at-point)))))

(define-key notmuch-message-mode-map (kbd "C-c TAB") #'notmuch-multi-address-complete)

(defface notmuch-multi-hello-header-face
  '((t :foreground "white"
     :background "blue"
     :weight bold))
  "Font for the header in `notmuch-multi-hello-insert-searches`."
  :group 'notmuch-faces)

(defface notmuch-multi-hello-buttons-unread-face
  '((t
     :inherit notmuch-tag-unread
     :weight bold))
  "Face used for unread hello buttons creation.
See `notmuch-multi-hello-insert-buttons`."
  :group 'notmuch-faces)

(defun notmuch-multi--notmuch-remove-untags (tags)
  "Return the elements of TAGS that do not begin with a minus sign.
These are the tag additions (e.g. \"+delete\"); removal tags such as
\"-inbox\" are dropped.  Used to reverse a tagging operation by keeping
only the tags it would add."
  (let ((rmtags '()))
    (mapc
     (lambda (str)
       (when (string-match "^[^-]" str)
         (add-to-list 'rmtags str))) tags) rmtags))

(defmacro notmuch-multi-search-tag-thread (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name  (&optional untag beg end)
     ,(format
       "Mark with `%s' the currently selected thread.

Operate on each message in the currently selected thread.  With
optional BEG and END as points delimiting a region that
encompasses multiple threads, operate on all those messages
instead.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the *added* tags.

This function advances to the next thread when finished."
       tags)
     (interactive current-prefix-arg)
     (let ((rmtags (if untag (notmuch-multi--notmuch-remove-untags ,tags) ,tags)))
       (when rmtags
         (notmuch-search-tag (notmuch-tag-change-list rmtags untag) beg end)
         (when (eq beg end)
           (notmuch-search-next-thread))))))

(notmuch-multi-search-tag-thread
  notmuch-multi-search-delete-thread
  notmuch-multi-mark-delete-tags)

(notmuch-multi-search-tag-thread
  notmuch-multi-search-expire-thread
  notmuch-multi-mark-expire-tags)

(notmuch-multi-search-tag-thread
  notmuch-multi-search-flag-thread
  notmuch-multi-mark-flag-tags)

(notmuch-multi-search-tag-thread
  notmuch-multi-search-spam-thread
  notmuch-multi-mark-spam-tags)

(defmacro notmuch-multi-search-tag-all (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag)
     ,(format
       "Mark with `%s' all messages in the search buffer.

Operate on each message in the current search buffer.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the *added* tags."
       tags)
     (interactive current-prefix-arg)
     (let ((rmtags (if untag (notmuch-multi--notmuch-remove-untags ,tags) ,tags)))
       (when rmtags
         (notmuch-search-tag-all (notmuch-tag-change-list rmtags untag))))))


(notmuch-multi-search-tag-all
  notmuch-multi-search-delete-all
  notmuch-multi-mark-delete-tags)

(notmuch-multi-search-tag-all
  notmuch-multi-search-expire-all
  notmuch-multi-mark-expire-tags)

(notmuch-multi-search-tag-all
  notmuch-multi-search-flag-all
  notmuch-multi-mark-flag-tags)

(notmuch-multi-search-tag-all
  notmuch-multi-search-spam-all
  notmuch-multi-mark-spam-tags)

(defmacro notmuch-multi-tree-tag-message (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional repeat)
     ,(format
       "Mark with `%s' the currently selected message.
Operate in `notmuch-tree-mode'.

With optional prefix argument as REPEAT, a number:
- if <= 0: reverse the application of the *added* tags
- if >= 0: apply the tag for the REPEAT next message.

This function advances to the next message when finished."
       tags)
     (interactive "P")
     (setq repeat (or repeat 1))
     (let* ((untag (<= repeat 0))
            (rmtags (if untag (notmuch-multi--notmuch-remove-untags ,tags) ,tags))
            (count (max 1 repeat)))
       (when rmtags
         (let ((i 0))
           (while (< i count)
             (setq i (1+ i))
             (notmuch-tree-tag (notmuch-tag-change-list rmtags untag))
             (notmuch-tree-next-message)))))))

(notmuch-multi-tree-tag-message
  notmuch-multi-tree-delete-message
  notmuch-multi-mark-delete-tags)

(notmuch-multi-tree-tag-message
  notmuch-multi-tree-expire-message
  notmuch-multi-mark-expire-tags)

(notmuch-multi-tree-tag-message
  notmuch-multi-tree-flag-message
  notmuch-multi-mark-flag-tags)

(notmuch-multi-tree-tag-message
  notmuch-multi-tree-spam-message
  notmuch-multi-mark-spam-tags)


(defmacro notmuch-multi-tree-tag-thread (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag _beg _end)
     ,(format
       "Mark with `%s' all message of the current thread
in notmuch-tree-mode.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the *added* tags."
       tags)
     (interactive (cons current-prefix-arg (notmuch-interactive-region)))
     (let ((rmtags (notmuch-multi--notmuch-remove-untags ,tags)))
       (when rmtags
         (notmuch-tree-tag-thread (notmuch-tag-change-list rmtags untag))
         (notmuch-tree-next-thread)))))

(notmuch-multi-tree-tag-thread
  notmuch-multi-tree-delete-thread
  notmuch-multi-mark-delete-tags)

(notmuch-multi-tree-tag-thread
  notmuch-multi-tree-expire-thread
  notmuch-multi-mark-expire-tags)

(notmuch-multi-tree-tag-thread
  notmuch-multi-tree-flag-thread
  notmuch-multi-mark-flag-tags)

(notmuch-multi-tree-tag-thread
  notmuch-multi-tree-spam-thread
  notmuch-multi-mark-spam-tags)

(defmacro notmuch-multi-show-tag-message (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag)
     ,(format
       "Apply `%s' to message.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the tags."
       tags)
     (interactive "P")
     (when ,tags
       (apply 'notmuch-show-tag-message
              (notmuch-tag-change-list ,tags untag)))))

(notmuch-multi-show-tag-message
  notmuch-multi-show-delete-message
  notmuch-multi-mark-delete-tags)

(notmuch-multi-show-tag-message
  notmuch-multi-show-expire-message
  notmuch-multi-mark-expire-tags)

(notmuch-multi-show-tag-message
  notmuch-multi-show-flag-message
  notmuch-multi-mark-flag-tags)

(notmuch-multi-show-tag-message
  notmuch-multi-show-spam-message
  notmuch-multi-mark-spam-tags)


(defun notmuch-multi-hello-query-insert (cnt query elem)
  "Insert a button widget showing count CNT for saved search ELEM.
CNT is the message count to display (when nil, blank padding is inserted
instead); QUERY is the search the button runs when pushed; ELEM is the
saved-search plist, read for its `:sort-order'.
Source: https://holgerschurig.github.io/en/emacs-notmuch-hello/"
  (if cnt
      (let* ((str (format "%s" cnt))
             (widget-push-button-prefix "")
             (widget-push-button-suffix "")
             (oldest-first (cl-case (plist-get elem :sort-order)
                             (newest-first nil)
                             (oldest-first t)
                             (otherwise notmuch-search-oldest-first))))
        (widget-create 'push-button
                       :notify #'notmuch-hello-widget-search
                       :notmuch-search-terms query
                       :notmuch-search-oldest-first oldest-first
                       :notmuch-search-type 'tree
                       str)
        (widget-insert (make-string (- 8 (length str)) ? )))
    (widget-insert "        ")))

(defun notmuch-multi-hello-query-counts (query-list &rest options)
  "Return one plist per saved search in QUERY-LIST, each carrying its counts.
Modified version of `notmuch-hello-query-counts' that adds an
`:unread-count' property alongside the usual `:count'.  OPTIONS are the
same keyword options accepted by the original (e.g. `:filter',
`:show-empty-searches', `:disable-excludes')."
  (with-temp-buffer
    (dolist (elem query-list nil)
      (let* ((count-query (or (notmuch-saved-search-get elem :count-query)
                              (notmuch-saved-search-get elem :query)))
             (consolidated-count-query (replace-regexp-in-string
                                        "\n" " "
                                        (notmuch-multi-hello-filtered-query count-query
                                                                            (or (plist-get options :filter-count)
                                                                                (plist-get options :filter))))))
        (insert
         consolidated-count-query "\n"
         (notmuch-multi-hello-filtered-query consolidated-count-query "tag:unread") "\n")))

    (unless (= (notmuch--call-process-region (point-min) (point-max) notmuch-command
                                             t t nil "count"
                                             (if (plist-get options :disable-excludes)
                                                 "--exclude=false"
                                               "--exclude=true")
                                             "--batch") 0)
      (notmuch-logged-error
       "Notmuch count --batch failed"
       "Please check that the notmuch CLI is new enough to support `count
--batch'. In general we recommend running matching versions of
the CLI and emacs interface."))
    (goto-char (point-min))
    (cl-mapcan
     (lambda (elem)
       (let* ((elem-plist (notmuch-hello-saved-search-to-plist elem))
              (search-query (plist-get elem-plist :query))
              (filtered-query (notmuch-multi-hello-filtered-query
                               search-query (plist-get options :filter)))
              (message-count (prog1 (read (current-buffer))
                               (forward-line 1)))
              (unread-count (prog1 (read (current-buffer))
                              (forward-line 1))))
         (when (and filtered-query (or (plist-get options :show-empty-searches)
                                       (> message-count 0)))
           (setq elem-plist (plist-put elem-plist :query filtered-query))
           (list (plist-put (plist-put elem-plist :count message-count) :unread-count unread-count)))))
     query-list)))

(defun notmuch-multi-hello-insert-buttons (searches)
  "Modified version of `notmuch-hello-insert-buttons'.

SEARCHES must be a list of plists each of which should contain at
least the properties :name NAME :query QUERY and :count COUNT,
where QUERY is the query to start when the button for the
corresponding entry is activated, and COUNT should be the number
of messages matching the query.  Such a plist can be computed
with `notmuch-multi-hello-query-counts'."
  (let* ((widest (notmuch-hello-longest-label searches))
         (tags-and-width (notmuch-hello-tags-per-line widest))
         (tags-per-line (car tags-and-width))
         (column-width (cdr tags-and-width))
         (column-indent 0)
         (count 0)
         (reordered-list (notmuch-hello-reflect searches tags-per-line))
         ;; Hack the display of the buttons used.
         (widget-push-button-prefix "")
         (widget-push-button-suffix ""))
    ;; dme: It feels as though there should be a better way to
    ;; implement this loop than using an incrementing counter.
    (mapc (lambda (elem)
            ;; (not elem) indicates an empty slot in the matrix.
            (when elem
              (when (> column-indent 0)
                (widget-insert (make-string column-indent ? )))
              (let* ((name (plist-get elem :name))
                     (query (plist-get elem :query))
                     (oldest-first (cl-case (plist-get elem :sort-order)
                                     (newest-first nil)
                                     (oldest-first t)
                                     (otherwise notmuch-search-oldest-first)))
                     (search-type (plist-get elem :search-type))
                     (msg-count (plist-get elem :count))
                     (unread-count (plist-get elem :unread-count))
                     (title (if (eq 0 unread-count) name
                              (propertize name 'face 'notmuch-multi-hello-buttons-unread-face))))
                (widget-insert (format "%8s/%s "
                                       (if (eq 0 unread-count) (notmuch-hello-nice-number unread-count)
                                         (propertize
                                          (notmuch-hello-nice-number unread-count)
                                          'face 'notmuch-multi-hello-buttons-unread-face))
                                       (notmuch-hello-nice-number msg-count)))
                (widget-create 'push-button
                               :notify #'notmuch-hello-widget-search
                               :notmuch-search-terms query
                               :notmuch-search-oldest-first oldest-first
                               :notmuch-search-type search-type
                               title)
                (setq column-indent
                      (1+ (max 0 (- column-width (length name)))))))
            (cl-incf count)
            (when (eq (% count tags-per-line) 0)
              (setq column-indent 0)
              (widget-insert "\n")))
          reordered-list)
    ;; If the last line was not full (and hence did not include a
    ;; carriage return), insert one now.
    (unless (eq (% count tags-per-line) 0)
      (widget-insert "\n"))))

(defun notmuch-multi-hello-insert-searches (title query-list &rest options)
  "Insert a section with TITLE showing buttons made from QUERY-LIST.

QUERY-LIST should ideally be a plist but for backwards
compatibility other forms are also accepted (see
`notmuch-saved-searches' for details).  The plist should
contain keys :name and :query; if :count-query is also present
then it specifies an alternate query to be used to generate the
count for the associated search.

Supports the following entries in OPTIONS as a plist:
:initially-hidden - if non-nil, section will be hidden on startup
:show-empty-searches - show buttons with no matching messages
:hide-if-empty - hide if no buttons would be shown
   (only makes sense without :show-empty-searches)
:filter - This can be a function that takes the search query as
   its argument and returns a filter to be used in conjunction
   with the query for that search or nil to hide the
   element.  This can also be a string that is used as a combined
   with each query using \"and\".
:filter-count - Separate filter to generate the count displayed
   each search.  Accepts the same values as :filter.  If :filter
   and :filter-count are specified, this will be used instead of
   :filter, not in conjunction with it."

  ;; (widget-insert "       ")
  (when (and notmuch-hello-first-run (plist-get options :initially-hidden))
    (add-to-list 'notmuch-hello-hidden-sections title))
  (let ((is-hidden (member title notmuch-hello-hidden-sections)))
    (if is-hidden
        (widget-create 'push-button
                       :notify (lambda (&rest _ignore)
                                 (setq notmuch-hello-hidden-sections
                                       (delete title notmuch-hello-hidden-sections))
                                 (notmuch-hello-update))
                       (concat title "..."))
      (widget-create 'push-button
                     :notify (lambda (&rest _ignore)
                               (add-to-list 'notmuch-hello-hidden-sections
                                            title)
                               (notmuch-hello-update))
                     title))
    (widget-insert "\n")
    (unless is-hidden
      (let ((searches (apply 'notmuch-multi-hello-query-counts query-list options)))
        (when (or (not (plist-get options :hide-if-empty))
                  searches)
          (widget-insert "\n")
          (notmuch-multi-hello-insert-buttons searches))))))

(defun notmuch-multi-hello-insert-account-searches (account-searches)
  "Insert a section pairing the account in ACCOUNT-SEARCHES with its search list.

Stamp the inserted region with the `notmuch-multi-account' text property
so `notmuch-multi-get-mail-at-point' can recover the account at point.

See `notmuch-multi-accounts-saved-searches'."
  (let* ((searches (plist-get account-searches :searches))
         (account (plist-get account-searches :account))
         (account-query (plist-get account :query))
         (start (point)))
    (notmuch-multi-hello-insert-searches
     (plist-get account :name) searches :filter account-query :show-empty-searches t)
    (put-text-property start (point) 'notmuch-multi-account account)))

(defun notmuch-multi-hello-insert-accounts-searches ()
  "Insert all `notmuch-multi-accounts-saved-searches' widgets."
  (dolist (account-searches notmuch-multi-accounts-saved-searches)
    (when account-searches
      (notmuch-multi-hello-insert-account-searches account-searches)
      (widget-insert "\n"))))

(defun notmuch-multi--get-command (account)
  "Return the shell fetch command string for ACCOUNT.
ACCOUNT's `:get-command' may be a string, or a function called with
ACCOUNT that returns a string.  Signal a `user-error' when ACCOUNT has
no usable `:get-command'."
  (let* ((name (plist-get account :name))
         (gc (plist-get account :get-command))
         (cmd (cond ((functionp gc) (funcall gc account))
                    ((stringp gc) gc)
                    (t nil))))
    (unless (and (stringp cmd) (not (string= cmd "")))
      (user-error "No :get-command configured for account %s" name))
    cmd))

(defun notmuch-multi--account-at-point ()
  "Return the account plist of the hello section at point, or nil.
Account sections are stamped with the `notmuch-multi-account' text
property by `notmuch-multi-hello-insert-account-searches'.  When point
sits on the blank line separating two sections, the property is read
from the preceding character, so the section just above point wins."
  (or (get-text-property (point) 'notmuch-multi-account)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'notmuch-multi-account))))

(defun notmuch-multi--refresh-hello-buffers ()
  "Refresh every live `notmuch-hello-mode' buffer.
Used by scheduled fetches, which have no single \"current\" hello buffer."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'notmuch-hello-mode)
          (notmuch-refresh-this-buffer))))))

(defun notmuch-multi--get-account-mail (account &optional scheduled)
  "Fetch and index mail for ACCOUNT, then refresh hello buffer(s).
Run ACCOUNT's `:get-command' followed by `notmuch-multi-index-command'
asynchronously.

When SCHEDULED is nil (the interactive `M-g' / \"a <key-prefix> g\" path):
refresh the originating hello buffer on success, signal a `user-error' when a
fetch for ACCOUNT is already running, and always message.

When SCHEDULED is non-nil (timer-driven): refresh all live
`notmuch-hello-mode' buffers on success, silently skip when a fetch for
ACCOUNT is already running, and gate progress messages on
`notmuch-multi-schedule-verbose'.  Never signals.

On failure the process output buffer is shown regardless of SCHEDULED.
See `notmuch-multi-index-command' and `notmuch-multi-schedule-verbose'."
  (let* ((name (plist-get account :name))
         (cmd (notmuch-multi--get-command account))
         (procname (format "notmuch-multi-get-%s" name))
         (bufname (format "*notmuch-multi-get: %s*" name))
         (full (concat cmd " && " notmuch-multi-index-command))
         (verbose (or (not scheduled) notmuch-multi-schedule-verbose))
         (refresh (if scheduled
                      #'notmuch-multi--refresh-hello-buffers
                    (let ((buf (current-buffer)))
                      (lambda ()
                        (when (buffer-live-p buf)
                          (with-current-buffer buf
                            (notmuch-refresh-this-buffer))))))))
    (if (process-live-p (get-process procname))
        (if scheduled
            (when verbose
              (message "%s: previous fetch still running, skipping" name))
          (user-error "Already fetching mail for %s" name))
      (when verbose (message "Fetching mail for %s…" name))
      (make-process
       :name procname
       :buffer bufname
       :connection-type 'pipe
       :command (list shell-file-name shell-command-switch full)
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (if (and (eq (process-status proc) 'exit)
                    (zerop (process-exit-status proc)))
               (progn
                 (when verbose (message "%s: fetched and indexed" name))
                 (funcall refresh))
             (message "%s: fetch failed (see %s)" name bufname)
             (display-buffer bufname))))))))

;;;###autoload
(defun notmuch-multi-get-mail-at-point ()
  "Fetch and index mail for the account whose hello section is at point.
Only meaningful in the `notmuch-hello' screen.  Bound to \\[notmuch-multi-get-mail-at-point]
in `notmuch-hello-mode'.  See `notmuch-multi--get-account-mail'."
  (interactive)
  (let ((account (notmuch-multi--account-at-point)))
    (unless account
      (user-error "No account at point"))
    (notmuch-multi--get-account-mail account)))

(define-key notmuch-hello-mode-map (kbd "M-g") #'notmuch-multi-get-mail-at-point)

(defun notmuch-multi--valid-interval-p (iv)
  "Return non-nil when IV is usable as a `run-at-time' REPEAT value.
A positive number of seconds, or a non-empty relative-time string."
  (or (and (numberp iv) (> iv 0))
      (and (stringp iv) (not (string= iv "")))))

(defun notmuch-multi--schedulable-account-p (account)
  "Return non-nil when ACCOUNT can be auto-fetched on a timer.
True when ACCOUNT has a usable `:get-command' (a non-empty string or a
function) and a valid `:get-interval' (see `notmuch-multi--valid-interval-p').
Unlike `notmuch-multi--get-command' this never signals, so it is safe for
filtering."
  (let ((gc (plist-get account :get-command))
        (iv (plist-get account :get-interval)))
    (and (or (functionp gc)
             (and (stringp gc) (not (string= gc ""))))
         (notmuch-multi--valid-interval-p iv))))

(defvar notmuch-multi--schedule-stagger 7
  "Seconds added between successive accounts' first scheduled fire.
Spreads initial fetches out so their `notmuch new' index steps are less
likely to run concurrently.  Internal; not a user option.")

(defvar notmuch-multi--schedule-timers nil
  "List of repeating timers armed by `notmuch-multi-schedule-start'.")

;;;###autoload
(defun notmuch-multi-schedule-start ()
  "Arm a repeating fetch timer for each schedulable account.
Cancel any timers from a previous call first, so this is safe to re-run
after editing `notmuch-multi-accounts-saved-searches'.  Each timer's first
fire is one interval out, with accounts staggered by
`notmuch-multi--schedule-stagger' seconds.  Timers run only while Emacs is
open.  See `notmuch-multi-schedule-verbose' and
`notmuch-multi--get-account-mail'."
  (interactive)
  (notmuch-multi-schedule-stop)
  (let ((offset 0)
        (count 0))
    (dolist (account-searches notmuch-multi-accounts-saved-searches)
      (let* ((account (plist-get account-searches :account))
             (interval (plist-get account :get-interval)))
        (when (notmuch-multi--schedulable-account-p account)
          (let ((first (if (numberp interval) (+ interval offset) interval)))
            (push (run-at-time first interval
                               #'notmuch-multi--get-account-mail account t)
                  notmuch-multi--schedule-timers))
          (setq offset (+ offset notmuch-multi--schedule-stagger))
          (setq count (1+ count)))))
    (message "notmuch-multi: scheduled %d account%s"
             count (if (= count 1) "" "s"))))

;;;###autoload
(defun notmuch-multi-schedule-stop ()
  "Cancel all fetch timers armed by `notmuch-multi-schedule-start'."
  (interactive)
  (mapc #'cancel-timer notmuch-multi--schedule-timers)
  (setq notmuch-multi--schedule-timers nil))

;;;###autoload
(defun notmuch-multi-schedule-status ()
  "Report which accounts have an armed fetch timer and at what interval.
The set is recomputed from the current configuration; re-run
`notmuch-multi-schedule-start' after editing accounts so the armed timers
and this report stay in sync."
  (interactive)
  (let ((msg
         (if (null notmuch-multi--schedule-timers)
             "notmuch-multi: scheduler not running"
           (let (lines)
             (dolist (account-searches notmuch-multi-accounts-saved-searches)
               (let ((account (plist-get account-searches :account)))
                 (when (notmuch-multi--schedulable-account-p account)
                   (push (format "%s (%s)"
                                 (plist-get account :name)
                                 (plist-get account :get-interval))
                         lines))))
             (concat "notmuch-multi: fetching "
                     (mapconcat #'identity (nreverse lines) ", "))))))
    (message "%s" msg)
    msg))

;;;###autoload
(defun notmuch-multi-delete-mail ()
  "Permanently delete mail tagged with `notmuch-multi-delete-tag'.

Do not attempt to refresh the index.  This will be done upon the
next invocation of \"notmuch new\"."
  (interactive)
  (notmuch-multi--notmuch-delete-mail-by-query (format "tag:%s" notmuch-multi-delete-tag)))

;;;###autoload
(defun notmuch-multi-delete-expirable-mail ()
  "Permanently delete expired email.

Expired mails are the ones marked as expirable and older
than `notmuch-multi-expire-delay'.

Do not attempt to refresh the index.  This will be done upon the
next invocation of \"notmuch new\".

See `notmuch-multi-expire-tag'."
  (interactive)
  (notmuch-multi--notmuch-delete-mail-by-query
   (format "tag:%s and date:@0..-%dd" notmuch-multi-expire-tag notmuch-multi-expire-delay)))

;;;###autoload
(defun notmuch-multi-delete-d+e-mail ()
  "Delete permanently the expired and the deletable emails.
See `notmuch-multi-delete-expirable-mail' and `notmuch-multi-delete-mail'."
  (interactive)
  (notmuch-multi-delete-mail)
  (notmuch-multi-delete-expirable-mail))

;;;###autoload
(defun notmuch-multi-count-query (query)
  "Return and echo the number of messages matching QUERY.
Simplified version of `notmuch-hello-query-counts'.
Usage: (notmuch-multi-count-query \"folder:here and tag:unread\")"
  (interactive "sQuery: ")
  (let ((count (string-to-number
                (with-temp-buffer
                  (shell-command
                   (format "notmuch count --exclude=false tag:deleted %s" query) t)
                  (buffer-substring-no-properties (point-min) (1- (point-max)))))))
    (message (format "%d" count))
    count))

(defun notmuch-multi--notmuch-delete-mail-by-query (query)
  "Permanently delete mails matching the notmuch QUERY.
Prompt for confirmation before carrying out the operation."
  (interactive)
  (let* ((count (notmuch-multi-count-query query))
         (mail (if (> count 1) "mails" "mail")))
    (if (> count 0)
        (message "No mail matching `%s'" query)
      (when (yes-or-no-p
             (format "Delete permanently %d %s matching `%s' ?" count mail query))
        (shell-command
         (format "notmuch search --output=files --format=text0 %s | xargs -r0 rm" query)
         t)))))

;;;###autoload
(defun notmuch-multi-refresh-buffer (&optional arg)
  "Run `notmuch-refresh-this-buffer'.
With optional prefix ARG (\\[universal-argument]) call
`notmuch-refresh-all-buffers'."
  (interactive "P")
  (if arg
      (notmuch-refresh-all-buffers)
    (notmuch-refresh-this-buffer)))

(defun notmuch-multi-strip-emoji (str)
  "Remove characters which are part of the `emoji' script from STR.
Source : https://emacs.stackexchange.com/a/79017"
  (replace-regexp-in-string
   "📽" "" ;; Special unwanted utf8 char.
   (cl-remove-if
    (lambda (c)
      (equal (aref char-script-table c) 'emoji))
    str)))

(defun notmuch-multi--ellipsid (len s &optional ellipsis)
  "If S is longer than LEN, truncate at last word and add ELLIPSIS.

When not specified, ELLIPSIS defaults to ‘...’.
Inspiration : https://github.com/magnars/s.el"
  (declare (side-effect-free t))
  (save-match-data
    (with-temp-buffer
      (insert s)
      (let ((fill-column len))
        (fill-region (point-min) (point-max)))
      (unless ellipsis
        (setq ellipsis "..."))
      (let*
          ((bs (buffer-substring (point-min) (point-max)))
           (shorted (replace-regexp-in-string "\\(\n.*\\)+" "" bs))
           (elid (if (eq (length bs ) (length shorted)) "" ellipsis)))
        (concat shorted elid)))))

(defun notmuch-multi--get-subject (msg)
  "Return MSG's subject with emoji stripped and truncated with an ellipsis.
MSG is a notmuch message plist; its subject is read from `:subject' (or
the `:Subject' header), cleaned with `notmuch-multi-strip-emoji' and
`notmuch-show-strip-re', then shortened by `notmuch-multi--ellipsid'."
  (let* ((subject (string-trim
                   (notmuch-multi-strip-emoji
                    (notmuch-show-strip-re
                     (or (plist-get msg :subject)
                         (plist-get (plist-get msg :headers) :Subject)))))))
    (notmuch-multi--ellipsid 85 subject "…")))

;;;###autoload
(defun notmuch-multi-tree-format-subject (format-string msg)
  "Render MSG's subject through FORMAT-STRING for `notmuch-tree-result-format'.
FORMAT-STRING is the subject column's format spec and MSG is the notmuch
message plist.  The subject is cleaned by `notmuch-multi--get-subject'
and faced according to its read or unread state."
  (let* ((tags (plist-get msg :tags ))
         (unread? (member "unread" tags))
         (subject (format format-string (notmuch-multi--get-subject msg)))
         (face (if unread?
                   'notmuch-search-unread-face
                 'notmuch-tree-no-match-subject-face)))
    (propertize subject 'face face )))

;;;###autoload
(defun notmuch-multi-search-format-subject (format-string msg)
  "Render MSG's subject through FORMAT-STRING for `notmuch-search-result-format'.
FORMAT-STRING is the subject column's format spec and MSG is the notmuch
message plist.  The subject is cleaned by `notmuch-multi--get-subject'
and faced according to its match state."
  (let* ((match? (plist-get msg :match))
         (subject (format format-string (notmuch-multi--get-subject msg)))
         (face (if match?
                   'notmuch-tree-match-face
                 'notmuch-tree-no-match-subject-face)))
    (propertize subject 'face face )))

(provide 'notmuch-multi)
;;; notmuch-multi.el ends here
