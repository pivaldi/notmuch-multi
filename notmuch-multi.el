;;; notmuch-multi.el --- Prettified Emacs Notmuch UI For Multiple Accounts -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Philippe IVALDI
;;
;; Author: Philippe IVALDI <emacs@ivaldi.me>
;; Maintainer: Philippe IVALDI <emacs@ivaldi.me>
;; Created: January 09, 2025
;; Version: 0.0.1
;; Keywords: mail extensions lisp notmuch
;; Homepage: https://github.com/pivaldi/notmuch-multi
;; Package-Requires: ((emacs "29") (notmuch "0.38.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Most code is inspired from
;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-emacs-modules/prot-emacs-notmuch.el?ref_type=heads
;; https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el
;;
;;; Code:

(require 'notmuch)

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
A message tagged as expirable will be removed after `'
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
    ,(format "-%s" notmuch-multi-expire-tag)
    )
  "List of tags to mark as important (flagged).
This gets the `notmuch-tag-flagged' face, if that is specified in
`notmuch-tag-formats'."
  :type '(repeat string)
  :group 'notmuch-multi)

(defcustom notmuch-multi-mark-spam-tags '("+spam" "-inbox" "-unread" "-archive")
  "List of tags to mark as spam."
  :type '(repeat string)
  :group 'notmuch-multi)

(define-widget 'notmuch-multi-account-plist 'list
  "An email account.

An account is a plist. Supported properties are

  :name         The name of the account (required).
  :query        Search for all the email for this account (required).
  :key          Optional prefix shortcut key open `notmuch-jump-search' relatively to this account.
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
                  (string :format "%v")))
          ))

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
                  (repeat :tag "Search" notmuch-saved-search-plist))
           )
          ))

(defun notmuch-multi-hello-filtered-query (query filter)
  "Constructs a query to search all messages matching QUERY and FILTER.
Modified version of `notmuch-hello-filtered-query' to handle query equal to *."
  (cond
   ((functionp filter) (funcall filter query))
   ((stringp filter)
    (if (string= query "*") filter
      (concat "(" query ") and (" filter ")")))
   (t query)))

;;;###autoload
(defun notmuch-multi-accounts-saved-searches-set (accounts-searches)
  "Setter of `notmuch-multi-accounts-saved-searches'.
Push the searches from accounts into `notmuch-saved-searches'
with computed nane, key and query.

Must be used instead of setq."
  (setq notmuch-multi-accounts-saved-searches accounts-searches)
  (dolist (account-searches accounts-searches)
    (let* ((searches (plist-get account-searches :searches))
           (account (plist-get account-searches :account))
           (account-name (plist-get account :name))
           (account-query (plist-get account :query))
           (kprefix (plist-get account :key-prefix)))
      (dolist (search searches)
        (let ((s (copy-sequence search)))
          (when search
            (when (not (string= kprefix ""))
              (let ((keydesc (key-description (plist-get search :key))))
                (plist-put s :key (kbd (concat kprefix keydesc))))
              ))
          (plist-put s :name (concat account-name " - " (plist-get search :name)))
          (plist-put s :query (notmuch-multi-hello-filtered-query account-query (plist-get search :query)))
          (add-to-list 'notmuch-saved-searches s)))
      )) notmuch-saved-searches)

(defcustom notmuch-multi-accounts-saved-searches
  `((:account (:name "MAIN" :query "*" :key ,(kbd "m"))
     :searches ,notmuch-saved-searches))
  "A list of email account associated with `notmuch-saved-searches'.

The saved accounts searches is a list of plist.
Supported properties of the plist are :

  :account         A `notmuch-multi-account-plist (required)'.
  :searches        A `notmuch-saved-searches' (required).
"
  :type '(repeat :tag "Account" notmuch-multi-accounts-saved-searches-plist)
  :tag "List of Accounts"
  :set (lambda (symbol value)
         (set-default symbol value)
         (notmuch-multi-accounts-saved-searches-set value))
  :group 'notmuch-multi)

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
  (let ((rmtags '()))
    (mapc
     (lambda (str)
       (when (string-match "^[^-]" str)
         (add-to-list 'rmtags str)
         )) tags)
    rmtags
    ))

(defmacro notmuch-multi-search-tag-thread (name tags)
  "Produce NAME function parsing TAGS.
Modified version of
https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el"
  (declare (indent defun))
  `(defun ,name (&optional untag)
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
       "Mark with `%s' all the messages in the search buffer.

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
  "Produce NAME function parsing TAGS.
Modified version of
https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el"
  (declare (indent defun))
  `(defun ,name (&optional repeat)
     ,(format
       "Mark with `%s' the currently selected message in notmuch-tree-mode.

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
             (message "Code block is running")
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
  `(defun ,name (&optional untag beg end)
     ,(format
       "Mark with `%s' all message of the the current thread in notmuch-tree-mode.

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
  "Produce NAME function parsing TAGS.
Source : https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el"
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
  "Create a notmuch query widget.
Source : https://holgerschurig.github.io/en/emacs-notmuch-hello/"
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
  "Modified version of `notmuch-hello-query-counts' to add unread the property :unread:count."
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
       "notmuch count --batch failed"
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
  "Insert a section with TITLE showing a list of buttons made from
QUERY-LIST.

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
   element. This can also be a string that is used as a combined
   with each query using \"and\".
:filter-count - Separate filter to generate the count displayed
   each search. Accepts the same values as :filter. If :filter
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
  "Insert a section of account associated with saved-searches.

See `notmuch-multi-accounts-saved-searches'."
  (let* ((searches (plist-get account-searches :searches))
         (account (plist-get account-searches :account))
         (account-query (plist-get account :query)))
    (notmuch-multi-hello-insert-searches
     (plist-get account :name) searches :filter account-query :show-empty-searches t)))

(defun notmuch-multi-hello-insert-accounts-searches ()
  "Insert all `notmuch-multi-accounts-saved-searches' widgets."
  (dolist (account-searches notmuch-multi-accounts-saved-searches)
    (when account-searches
      (notmuch-multi-hello-insert-account-searches account-searches)
      (widget-insert "\n"))))

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
  "Simplified version of `notmuch-hello-query-counts'.
Usage : (notmuch-multi-count-query \"folder:here and tag:unread\")"
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
Prompt for confirmation before carrying out the operation.
Inspired from : https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el"
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
`notmuch-refresh-all-buffers'.
Source : https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-notmuch.el"
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
           (elid (if (eq (length bs ) (length shorted)) "" ellipsis))
           )
        (concat shorted elid)))))

(defun notmuch-multi--get-subject (msg)
  (let* ((subject (string-trim
                   (notmuch-multi-strip-emoji
                    (notmuch-show-strip-re
                     (or (plist-get msg :subject)
                         (plist-get (plist-get msg :headers) :Subject)))))))
    (notmuch-multi--ellipsid 85 subject "…")))

;;;###autoload
(defun notmuch-multi-tree-format-subject (format-string msg)
  (let* ((tags (plist-get msg :tags ))
         (unread? (member "unread" tags))
         (subject (format format-string (notmuch-multi--get-subject msg)))
         (face (if unread?
                   'notmuch-search-unread-face
                 'notmuch-tree-no-match-subject-face)))
    (propertize subject 'face face )))

;;;###autoload
(defun notmuch-multi-search-format-subject (format-string msg)
  (let* ((match? (plist-get msg :match))
         (subject (format format-string (notmuch-multi--get-subject msg)))
         (face (if match?
                   'notmuch-tree-match-face
                 'notmuch-tree-no-match-subject-face)))
    (propertize subject 'face face )))

(provide 'notmuch-multi)
;;; notmuch-multi.el ends here
