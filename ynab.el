;;; ynab.el --- YNAB Mode -*- lexical-binding:t -*-

;; Copyright © 2023

;; Author: Ben MacLaurin <benmaclaurin@icloud.com>
;; URL: https://github.com/ben-maclaurin/ynab-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "29"))
;; Created: 2023-12-10
;; Keywords: ynab, finance

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Manage your YNAB budget in Emacs.
;; Requires a [[https://www.ynab.com/pricing][YNAB]] subscription and verified email.

(defconst ynab--budget-list-format
  [("Category" 60 t)
   ("Assigned" 30 t)
   ("Activity" 30 t)
   ("Available" 30 t)]
  "Defines a list of column specifications for YNAB budget tabulated-lists")

(defconst ynab--endpoint "https://api.ynab.com/v1/budgets/")

(defgroup ynab nil
  "Customization group for YNAB (You Need A Budget) integration."
  :prefix "ynab-"
  :group 'applications)

(defcustom ynab-budget-id ""
  "Sets your YNAB budget ID.
   Requirements:
   - A valid YNAB budget

   You can quickly find your budget ID from the URL of your budget view.
   e.g. given the following URL:

   https://app.ynab.com/64dfafd8-500e-4383-8f81-1822475830ec/budget/202312

   ..your budget ID would be: 64dfafd8-500e-4383-8f81-1822475830ec"
  :group 'ynab
  :type 'string)

(defcustom ynab-api-key ""
  "Sets your YNAB API key.
   Requirements:
   - A YNAB subscription
   - Verified email on YNAB

   If the above requirements are met, you may create a new API key from:
   Account Settings -> Developer Settings -> New Token

   You can also visit: https://app.ynab.com/settings/developer"
  :group 'ynab
  :type 'string)

(defun ynab--update-category (category)
  "Update a category with data"
  (let ((headers
         (list (cons "Authorization" (format "%s" ynab-api-key)))))
    (request
     (concat
      ynab--endpoint
      ynab-budget-id
      "/categories/"
      (ynabb-assoc-element 'id category)
      :headers headers
      :sync t
      :type "PATCH"
      :parse 'json-read
      :complete
      (cl-function
       (lambda (&key data &allow-other-keys) (print response)))))))

(defun ynab--fetch-current-month ()
  "Fetches current month`'s budget data from YNAB API, using `'ynab--api-key`'
   for authorization and returns parsed month information"
  (let ((month nil)
        (headers
         (list
          (cons "Authorization" (format "Bearer %s" ynab-api-key)))))
    (request
     (concat ynab--endpoint ynab-budget-id "/months/current")
     :headers headers
     :sync t
     :parse 'json-read
     :complete
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let* ((response (json-read-from-string data))
               (data (assoc 'data response)))
          (setq month (assoc 'month data))))))
    (cdr month)))

(defvar ynab--month nil)
(defvar ynab--categories nil)
(defvar ynab--to-be-budgeted nil)

;; TODO handle exclusion of category "Internal Master Category" before printing data to ynab-data.lisp
(defun ynab--get-category-groups-from-categories (categories)
  "Collects and returns unique names of category groups
  from `'categories`', excluding any named `'Internal Master Category`'"
  (mapcar
   (lambda (arg)
     (ynab--get-assoc-element 'category_group_name arg))
   categories))

(defun ynab--init-tabulated-list (list-format list-entries)
  "Creates or gets a buffer named `'YNAB`', sets up and displays a tabulated
  list with specified format and entries. Returns new buffer"
  (let ((buffer (get-buffer-create "YNAB")))
    (with-current-buffer buffer
      (setq tabulated-list-format list-format)
      (setq tabulated-list-entries list-entries)
      (tabulated-list-mode)
      (tabulated-list-init-header)
      (tabulated-list-print))
    buffer))

;; TODO handle exclusion of category before printing data to ynab-data.lisp
(defun ynab--calculate-sum-for-value (value categories)
  "Calculates and returns the sum of `'value`' fields across
  all categories in `'categories`', excluding the category
  named `'Inflow: Ready to Assign`'"
  (let ((sum 0))
    (cl-loop
     for category across categories do
     (if (not
          (string=
           (ynab--get-assoc-element 'name category)
           "Inflow: Ready to Assign"))
         (setq sum (+ sum (ynab--get-assoc-element value category)))))
    sum))

(defun ynab--display-to-be-budgeted-message (to-be-budgeted)
  "Sets `'message`' based on the value of `'to-be-budgeted`': `'Ready to Assign`'
  in green if positive, `'All Money Assigned`' if zero, or
  `'You assigned more than you have`' in red if negative.
  Returns the formatted message"
  (let ((message ""))
    (if (> to-be-budgeted 0)
        (setq message
              (format "%s"
                      (propertize "Ready to Assign"
                                  'face
                                  'diff-added)))
      (if (= to-be-budgeted 0)
          (setq message (format "%s" "All Money Assigned"))
        (setq message
              (format "%s"
                      (propertize "You assigned more than you have"
                                  'face
                                  'diff-removed)))))
    message))

(defun ynab--totalise-columns-for-list (list)
  (list
   "Total"
   (vector
    (format "%s" (propertize "Total" 'face 'bold))
    (format "%s"
            (propertize (ynab--format-value-as-money
                         (ynab--calculate-sum-for-value
                          'budgeted list))
                        'face 'bold))
    (format "%s"
            (propertize (ynab--format-value-as-money
                         (ynab--calculate-sum-for-value
                          'activity list))
                        'face 'bold))
    (format "%s"
            (propertize (ynab--format-value-as-money
                         (ynab--calculate-sum-for-value
                          'balance list))
                        'face 'bold)))))

;; TODO handle exclusion of category before printing data to ynab-data.lisp
(defun ynab--init-budget-entries (categories to-be-budgeted)
  "Creates a list of entries for a tabulated list display from `'categories`'.
   It includes special entries for `'To be budgeted`' and `'Total`' with formatted
   values and adds each category, excluding `'Inflow: Ready to Assign`', with formatted
   budget, activity, and balance values. Returns the list of entries."
  (let ((entries '()))

    (push (list
           "To be budgeted"
           (vector
            (ynab--display-to-be-budgeted-message to-be-budgeted)
            (format "%s"
                    (ynab--format-budget to-be-budgeted))
            (format "%s" "") (format "%s" "")))
          entries)

    (push (ynab--totalise-columns-for-list categories) entries)

    (cl-loop
     for element across categories do
     (if (not
          (string=
           (ynab--get-assoc-element 'name element)
           "Inflow: Ready to Assign"))
         (push (list
                (format "%s"
                        (propertize (ynab--get-assoc-element
                                     'id element)
                                    'face 'link))
                (vector
                 (format "%s"
                         (propertize
                          (ynab--get-assoc-element 'name element)
                          'face 'link))
                 (ynab--format-value-as-money
                  (ynab--get-assoc-element 'budgeted element))
                 (ynab--format-value-as-money
                  (ynab--get-assoc-element 'activity element))
                 (ynab--format-balance
                  (ynab--get-assoc-element 'balance element)
                  (ynab--get-assoc-element
                   'goal_under_funded element))))
               entries)))
    entries))

(defun ynab--format-balance (balance goal-under-funded)
  "Formats `'balance`' based on `'goal-under-funded`': if `'goal-under-funded`' is
   positive and `'balance`' is non-negative, `'balance`' is displayed in a modified
   face, if `'balance`' is negative, it's shown in a removed face; otherwise, `'balance`' is
   formatted as standard money. Returns the formatted `'balance`'"
  (if goal-under-funded
      (if (> goal-under-funded 0)
          (if (>= balance 0)
              (format "%s"
                      (propertize (ynab--format-value-as-money
                                   balance)
                                  'face 'diff-changed))
            (changed
             "%s"
             (propertize (ynab--format-value-as-money balance)
                         'face
                         'diff-removed)))
        (ynab--format-budget balance))
    (ynab--format-budget balance)))

(defun ynab--format-budget (balance)
  "Formats `'balance`': if positive, it's displayed in an added face,
   if exactly zero, as plain money, and if negative, in a removed face.
   Returns the formatted `'balance`'"
  (if (>= balance 0)
      (if (= balance 0)
          (format "%s" (ynab--format-value-as-money balance))
        (format "%s"
                (propertize (ynab--format-value-as-money balance)
                            'face
                            'diff-added)))
    (format "%s"
            (propertize (ynab--format-value-as-money balance)
                        'face
                        'diff-removed))))

(defun ynab--format-value-as-money (value)
  "Formats `'value`' as a string representing a monetary amount in pounds,
   converting from a thousandth of a pound to pounds with two decimal places.
   Conversion is because YNAB returns values in thousandths"
  (format "£%.2f" (/ (float value) 1000)))

(defun ynab--get-assoc-element (key alist)
  "Retrieves the value associated with `'key`'
   in an association list `'list`'"
  (cdr (assoc key alist)))

(defvar ynab--mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'ynab-update)
    (define-key map (kbd "b") 'ynab-budget)
    (define-key map (kbd "c") 'ynab-categories)
    (define-key map (kbd "u") 'ynab-underfunded)
    (define-key map (kbd "s") 'ynab-spent)
    (define-key map (kbd "a") 'ynab-available)
    map)
  "Defines the YNAB mode map for ergonomic calling of functions from the budget view")

(defun ynab--init-and-switch-to-budget-buffer
    (budget-data to-be-budgeted)
  "Initializes a tabulated list with `'ynab--budget-list-format`' and budget entries
   from `'budget-data`' and `'to-be-budgeted`', then switches to the newly created buffer."
  (let ((buffer
         (ynab--init-tabulated-list
          ynab--budget-list-format
          (ynab--init-budget-entries budget-data to-be-budgeted))))
    (switch-to-buffer buffer)
    (ynab-mode)))

(define-minor-mode ynab-mode
  "Defines the YNAB minor mode"
  :lighter " YNAB"
  :keymap ynab--mode-map)

(defun ynab--filter-by-non-zero-element (element list)
  (let ((non-zero-elements '()))
    (cl-loop
     for e across list do
     (if (ynab--get-assoc-element element e)
         (if (> (ynab--get-assoc-element element e) 0)
             (push e non-zero-elements))))
    non-zero-elements))


(defun ynab-update ()
  "Synchronously pull data from YNAB. Blocking action"
  (interactive)

  (if (and (> (length ynab-api-key) 0) (> (length ynab-budget-id) 0))
      (progn
        (setq ynab--month (ynab--fetch-current-month))
        (setq ynab--categories
              (ynab--get-assoc-element 'categories ynab--month))
        (setq ynab--to-be-budgeted
              (ynab--get-assoc-element 'to_be_budgeted ynab--month))
        (ignore-errors
          kill-buffer
          "YNAB")
        (ynab-budget))
    (progn
      (with-output-to-temp-buffer "*YNAB ERROR*"
        (princ
         "You must set your budget ID and API key. Please see: \n\nC-h v ynab-set-budget-id \n\nC-h v ynab-set-api-key")))))

(defun ynab-available ()
  "Display all categories where money is available"
  (interactive)

  (let ((available
         (ynab--filter-by-non-zero-element
          'balance ynab--categories)))
    (ynab--init-and-switch-to-budget-buffer
     (vconcat available) ynab--to-be-budgeted)))

(defun ynab-underfunded ()
  "Display categories by underfunded"
  (interactive)

  (let ((underfunded
         (ynab--filter-by-non-zero-element
          'goal_under_funded ynab--categories)))
    (ynab--init-and-switch-to-budget-buffer
     (vconcat underfunded) ynab--to-be-budgeted)))

(defun ynab-spent ()
  "Display categories where you have spent money"
  (interactive)

  (let ((spent '()))
    (cl-loop
     for element across ynab--categories do
     (if (ynab--get-assoc-element 'activity element)
         (if (> 0 (ynab--get-assoc-element 'activity element))
             (push element spent))))
    (ynab--init-and-switch-to-budget-buffer
     (vconcat spent) ynab--to-be-budgeted)))

(defun ynab-categories ()
  "Display categories by their respective category group"
  (interactive)

  (let ((entries-in-category-group '())
        (chosen-category-group
         (completing-read
          "Category Groups"
          (ynab--get-category-groups-from-categories
           ynab--categories))))

    (cl-loop
     for element across ynab--categories do
     (if (string=
          (ynab--get-assoc-element
           'category_group_name element)
          chosen-category-group)
         (push element entries-in-category-group)))

    (ynab--init-and-switch-to-budget-buffer
     (vconcat entries-in-category-group) ynab--to-be-budgeted)))

(defun ynab--category-names ()
  (mapcar
   (lambda (arg) (ynab--get-assoc-element 'name arg))
   ynab--categories))

(defun ynab-assign ()
  "Assign money"
  (interactive)
  (let ((choice
         (completing-read
          "Choose category to assign to" (ynab--category-names)))
	(amount (completing-read "Set amount" nil)))))

(defun ynab-budget ()
  "Open your YNAB budget for the current month"
  (interactive)

  (if (> (length ynab--month) 0)
      (progn
        (ynab--init-and-switch-to-budget-buffer
         ynab--categories ynab--to-be-budgeted))
    (ynab-update)))

(provide 'ynab)
