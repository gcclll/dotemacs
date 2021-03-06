;;; irfc.el --- Interface for IETF RFC document.

;; Filename: irfc.el
;; Description: Interface for IETF RFC document.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;;         Juanma Barranquero <lekktu@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Copyright (C) 2009, Juanma Barranquero, all rights reserved.
;; Created: 2009-01-14 08:13:15
;; Version: 0.5.4
;; Last-Updated: 2009-02-13 10:33:22
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/irfc.el
;; Keywords: RFC, IETF
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `cl' `url-vars'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Introduction:
;;
;; For historical reasons, IETF Internet RFCs are required to be in a plain
;; ASCII text format that's best-suited for sending directly to a 6-lpi
;; US-letter-size printer.  This makes them suboptimal for viewing on-screen,
;; as you will be doing for countless hours if you're ever doing network
;; programming to one of them.  Fortunately, the ASCII format is usually
;; close to what you, the Emacs zealot, *truly* want -- which is a format
;; suited to more pleasurably viewing the RFC in Emacs.
;;
;; The `irfc' package uses Emacs overlays to add some fortification and
;; hide the page headers and footers (which it replaces with one-line page
;; number references that look like "(p.1)", right-justified).  The file is
;; never modified, and you can see the raw ASCII text by pressing `T'.
;;

;;; Commentary:
;;
;; Interface for IETF RFC document.
;;
;; This package use some code from `rfcview.el'.
;; Thanks "Neil W.  Van Dyke"!
;;
;; The features this package provide:
;;
;; * Format RFC document for easy reading.
;; * Single keystroke for fast view.
;; * Render status switch.
;; * Smart table and content switch.
;; * Visit RFC link around point.
;; * Download RFC document *asynchronous*.
;;
;; Below are commands you can use:
;;
;; `irfc-render-toggle'         Toggle render status with RFC buffer.
;; `irfc-quit'                  Quit RFC buffer.
;; `irfc-visit'                 Ask for RFC number and visit document.
;; `irfc-follow'                Visit RFC document around point.
;; `irfc-table-jump'            Switch between table and content.
;; `irfc-page-goto'             Goto page.
;; `irfc-page-next'             Jump next page.
;; `irfc-page-prev'             Jump previous page.
;; `irfc-page-first'            Jump first page.
;; `irfc-page-last'             Jump last page.
;; `irfc-page-table'            Jump table page.
;; `irfc-head-next'             Jump next heading.
;; `irfc-head-prev'             Jump previous heading.
;; `irfc-rfc-link-next'         Jump next RFC link.
;; `irfc-rfc-link-prev'         Jump previous RFC link.
;; `irfc-scroll-up-one-line'    Scroll up one line.
;; `irfc-scroll-down-one-line'  Scroll down one line.
;;
;; Tips:
;;
;; You can use command `irfc-render-toggle' to toggle render status.
;;
;; Command `irfc-table-jump' can switch between table and content,
;; example you stay cursor at *table*, and type "G" will jump corresponding
;; content in buffer, alike, you can stay at any content and type "G"
;; will jump corresponding table item.
;;
;; Command `irfc-follow' will visit RFC document around point,
;; example you stay cursor at "[RFC3986]", and type "o" will
;; open rfc3986.txt in storage directory.  If have not found
;; this file in directory, will download from `http://www.ietf.org/rfc/'
;; and open it when download complete.
;;
;; And command ???irfc-follow??? can also use at title of RFC document.
;; Example rfc3986.txt contain ???Obsoletes: 2732, 2396, 1808??? at title,
;; you can move cursor to ???2732??? and type ???o??? will visit RFC 2732 document.
;; ???irfc-follow??? support below keywords in title:
;;
;;        ???Request for Comments:???
;;        ???Updates:???
;;        ???Obsoletes:???
;;
;; You can use command `irfc-rfc-link-next' or `irfc-rfc-link-prev'
;; to jump next or previous RFC link in document.
;;
;; Command `irfc-visit' will ask the user for a RFC number and will
;; visit that document, either from `irfc-directory', if exists, or by
;; downloading it.  This command can serve as entry point for Irfc,
;; to go to a RFC without having to visit the file or remember
;; whether it is already in `irfc-directory'.
;; And if you visit same document with your previous type, so just
;; hit RET, and don't need type RFC document number.
;;


;;; Installation:
;;
;; Put irfc.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'irfc)
;;
;; Setup your storage directory for RFC documents.
;;
;; (setq irfc-directory "YourStorageDirectory")
;;
;; If you want make RFC document load `irfc-mode' automatically,
;; setup like below:
;;
;; (setq irfc-assoc-mode t)
;;

;;; Customize:
;;
;; `irfc-assoc-mode' whether assoc RFC document with `irfc-mode'.
;; `irfc-directory' the storage directory for RFC document.
;; `irfc-download-base-url' the base url for download RFC document.
;;
;; All of the above can customize by:
;;      M-x customize-group RET irfc RET
;;

;;; Change log:
;;
;; 2009/02/13
;;   * Andy Stewart:
;;      * New variable `irfc-table-regex'.
;;      * Fix bug of `irfc-table-jump'.
;;      * Fix doc.
;;
;; 2009/01/29
;;   * Andy Stewart:
;;      * Fix overlay RFC link (as format [RFC-number]) in RFC1034.txt.
;;      * Fix RFC link jump bug.
;;
;; 2009/01/22
;;   * Juanma Barranquero
;;      * Add require information to avoid compile warning.
;;      * Add new function `irfc-unload-function' to cleanup
;;        when execute command `unload-feature'.
;;
;; 2009/01/21
;;   * Juanma Barranquero
;;      * Add new command `irfc-visit' for fast open or download RFC
;;        document.
;;      * Fix doc.
;;   * Andy Stewart:
;;      * Applied Juanma's patch (with slightly modified). Thanks!
;;      * Add variable `irfc-last-visit-number' to record last input
;;        RFC document number, save type if have to visit same document
;;        with previous times.
;;      * Fix bug of function `irfc-download-callback'.
;;        Display error information when download RFC document failed.
;;
;; 2009/01/18
;;   * Andy Stewart:
;;      * Make `irfc-follow' can open RFC link at title.
;;        Now support below keyword in title:
;;              "Request for Comments:"
;;              "Updates:"
;;              "Obsoletes:"
;;      * Add new commands: `irfc-rfc-link-next' and `irfc-rfc-link-prev'.
;;      * Fix doc.
;;
;;   * Juanma Barranquero:
;;      * Fix defface error, and improve document.
;;
;; 2009/01/17
;;   * Andy Stewart:
;;      * Fix doc.
;;      * Remove function `irfc-render-buffer-hide-cr'.
;;      * Thanks "Juanma Barranquero" improve document and advices. :)
;;
;; 2009/01/16
;;   * Andy Stewart:
;;      * Modified code for 22 compatibility.
;;      * Fix doc.
;;
;; 2009/01/14
;;   * Andy Stewart:
;;      * First released.
;;

;;; Acknowledgements:
;;
;;      Neil W. Van Dyke        <neil@neilvandyke.org>
;;              For create rfcview.el
;;      Juanma Barranquero      <lekktu@gmail.com>
;;              Thanks Juanma Barranquero send many patches.
;;              Juanma, thank you very much! :)
;;

;;; TODO
;;
;;
;;

;;; Require
(eval-when-compile (require 'cl))
(require 'url-vars)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup irfc nil
  "Interface for IETF RFC documents."
  :group 'edit)

(defcustom irfc-assoc-mode nil
  "If t, RFC documents are associated with `irfc-mode'.
Default is nil."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (if value
             (add-to-list 'auto-mode-alist
                          '("/rfc[0-9]+\\.txt\\'" . irfc-mode))
           (remove-hook 'auto-mode-alist
                        '("/rfc[0-9]+\\.txt\\'" . irfc-mode))))
  :group 'irfc)

(defcustom irfc-directory "~/.emacs.d/RFC/"
  "The storage directory for RFC document download and search."
  :type 'string
  :group 'irfc)

(defcustom irfc-download-base-url "http://www.ietf.org/rfc/"
  "The base URL for downloading RFC documents."
  :type 'string
  :group 'irfc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface irfc-title-face
  '((t (:foreground "Gold" :bold t)))
  "Face used for titles."
  :group 'irfc)
(defvar irfc-title-overlay nil
  "Overlay for `irfc-title-face'.")

(defface irfc-head-name-face
  '((t (:foreground "DarkRed" :bold t :underline t)))
  "Face used for heading names."
  :group 'irfc)
(defvar irfc-head-name-overlay nil
  "Overlay for `irfc-head-name-face'.")

(defface irfc-head-number-face
  '((t (:foreground "DarkRed" :bold t)))
  "Face used for heading numbers."
  :group 'irfc)
(defvar irfc-head-number-overlay nil
  "Overlay for `irfc-head-number-face'.")

(defface irfc-rfc-number-face
  '((t (:foreground "Green3" :bold t)))
  "Face used for RFC number in the header."
  :group 'irfc)
(defvar irfc-rfc-number-overlay nil
  "Overlay for `irfc-rfc-number-face'.")

(defface irfc-std-number-face
  '((t (:foreground "Grey" :bold t)))
  "Face used for STD number in the header."
  :group 'irfc)
(defvar irfc-std-number-overlay nil
  "Overlay for `irfc-std-number-face'.")

(defface irfc-rfc-link-face
  '((t (:foreground "Grey30" :bold t)))
  "Face used for RFC link in the header."
  :group 'irfc)
(defvar irfc-rfc-link-overlay nil
  "Overlay for `irfc-rfc-link-face'.")

(defface irfc-table-item-face
  '((t (:foreground "LawnGreen")))
  "Face used for Table item."
  :group 'irfc)
(defvar irfc-table-item-overlay nil
  "Overlay for `irfc-table-item-face'.")

(defvar irfc-hide-overlay nil
  "Overlay for hiding whitespace or blank lines.")

(defvar irfc-page-number-overlay nil
  "Overlay for page number.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar irfc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "h") 'backward-char)
    (define-key map (kbd "l") 'forward-char)
    (define-key map (kbd "e") 'scroll-down)
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "J") 'irfc-scroll-up-one-line)
    (define-key map (kbd "K") 'irfc-scroll-down-one-line)
    (define-key map (kbd ",") 'end-of-buffer)
    (define-key map (kbd ".") 'beginning-of-buffer)
    (define-key map (kbd "T") 'irfc-render-toggle)
    (define-key map (kbd "q") 'irfc-quit)
    (define-key map (kbd "o") 'irfc-follow)
    (define-key map (kbd "v") 'irfc-visit)
    (define-key map (kbd "g") 'irfc-page-goto)
    (define-key map (kbd "N") 'irfc-page-next)
    (define-key map (kbd "P") 'irfc-page-prev)
    (define-key map (kbd "<") 'irfc-page-last)
    (define-key map (kbd ">") 'irfc-page-first)
    (define-key map (kbd "b") 'irfc-page-table)
    (define-key map (kbd "H") 'irfc-head-next)
    (define-key map (kbd "L") 'irfc-head-prev)
    (define-key map (kbd "G") 'irfc-table-jump)
    (define-key map (kbd "<tab>") 'irfc-rfc-link-next)
    (define-key map (kbd "<backtab>") 'irfc-rfc-link-prev)
    map)
  "Keymap used by `irfc-mode'.")

(defvar irfc-stock-section-names
  '("abstract"
    "acknowledgement"
    "acknowledgements"
    "acknowledgment"
    "acknowledgments"
    "appendices"
    "author's address"
    "authors' addresses"
    "bibliography"
    "chair's address"
    "copyright notice"
    "copyright statement"
    "editor's address"
    "editors' addresses"
    "full copyright notice"
    "full copyright statement"
    "iesg note"
    "index"
    "introduction"
    "references and bibliography"
    "references"
    "security considerations"
    "status of this memo"
    "table of contents")
  "The stock name for overlay heading.")

(defvar irfc-download-buffer nil
  "Download buffer used by `url-retrieve'.
This variable is always buffer-local.")
(make-variable-buffer-local 'irfc-download-buffer)

(defvar irfc-download-url nil
  "URL from which to download files.
This variable is always buffer-local.")
(make-variable-buffer-local 'irfc-download-url)

(defvar irfc-render-p t
  "Render status for RFC buffer.
This variable is always buffer-local.")
(make-variable-buffer-local 'irfc-render-p)

(defvar irfc-total-pages 0
  "Total number of pages in RFC buffer.
This variable is always buffer-local.")
(make-variable-buffer-local 'irfc-total-pages)

(defvar irfc-last-visit-number nil
  "Number of the last RFC document visited.")

(defvar irfc-table-regex "^[ ]+[A-Z]?[0-9\\.]*[ ]+\\([^\\.\n]+\\)[\\. ]+\\([0-9]+\\)$"
  "The regular-expression that match table item.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-derived-mode irfc-mode text-mode "Irfc"
  "Major mode for IETF RFC documents."
  ;; Setup.
  (use-local-map irfc-mode-map)
  (setq buffer-read-only t)
  (setq font-lock-defaults nil)
  (auto-save-mode 0)
  ;; Render.
  (irfc-render-buffer))

(defun irfc-render-buffer ()
  "Render RFC buffer."
  (interactive)
  (save-excursion
    (let ((case-fold-search nil)
          (top-point (point-min))
          (title-line-point nil)
          temp-point)
      ;; Clean up overlays.
      (irfc-overlay-remove-all)
      ;; Hide whitespace at start of file.
      (setq temp-point (irfc-render-buffer-hide-whitespace-at-start))
      (if temp-point (setq top-point temp-point))
      ;; Hide any extraneous blank lines.
      (setq title-line-point (irfc-render-buffer-hide-blank-line top-point))
      ;; Add overlays for page headers and footers.
      (irfc-render-buffer-overlay-page-number)
      ;; Add overlay for the RFC number.
      (irfc-render-buffer-overlay-rfc-number top-point title-line-point)
      ;; Add overlay for the STD number.
      (irfc-render-buffer-overlay-std-number top-point title-line-point)
      ;; Add overlay for the table item.
      (irfc-render-buffer-overlay-table-item top-point)
      ;; Add overlay for the RFC link.
      (irfc-render-buffer-overlay-rfc-link top-point)
      ;; Add overlay for the title.
      (irfc-render-buffer-overlay-title title-line-point)
      ;; Add overlay for the heading.
      (irfc-render-buffer-overlay-head title-line-point))))

(defun irfc-render-toggle ()
  "Toggle RFC buffer render status."
  (interactive)
  (if irfc-render-p
      (irfc-render-turn-off)
    (irfc-render-turn-on)))

(defun irfc-render-turn-on ()
  "Turn on RFC buffer render status."
  (irfc-render-buffer)
  (setq irfc-render-p t))

(defun irfc-render-turn-off ()
  "Turn off RFC buffer render status."
  (irfc-overlay-remove-all)
  (setq irfc-render-p nil))

(defun irfc-quit ()
  "Quit RFC buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun irfc-table-jump ()
  "Jump between table and content.
You can jump to the corresponding table item when you are at content.
You can jump to the corresponding content when you are at table."
  (interactive)
  (if (irfc-have-table-p)
      (let ((original-position (point))
            head-name
            page-number
            match-list)
        (cond ((irfc-in-table-p)
               ;; When in table.
               (beginning-of-line)
               (if (search-forward-regexp irfc-table-regex (line-end-position) t)
                   ;; Jump content when find valid table item.
                   (progn
                     ;; Get head name and page number.
                     (setq head-name (match-string 0))
                     (setq head-name (replace-regexp-in-string "[\\. ]+\\([0-9]+\\)$" "" head-name))
                     (setq head-name (replace-regexp-in-string "^[ ]+" "" head-name))
                     (setq page-number (string-to-number (match-string 2)))
                     ;; Jump page.
                     (irfc-page-goto page-number)
                     ;; Search head.
                     (re-search-forward head-name nil t)
                     ;; Indent.
                     (back-to-indentation))
                 ;; Restore original position and output message
                 ;; when at invalid table item.
                 (message "Invalid table item.")
                 (goto-char original-position)))
              ;; Do nothing when at front of table.
              ((irfc-front-table-p)
               (message "In front of table."))
              ;; Jump corresponding table item from content.
              (t
               ;; Get head name and page number.
               (end-of-line)
               (setq match-list (irfc-head-move t))
               (setq head-name (buffer-substring-no-properties (nth 2 match-list) (nth 3 match-list)))
               (setq page-number (irfc-current-page))
               ;; Jump table.
               (irfc-page-table)
               ;; Search head.
               (re-search-forward (concat (regexp-quote head-name) "[\\. ]+" (regexp-quote (number-to-string page-number))))
               ;; Indent.
               (back-to-indentation))))
    ;; Do nothing when haven't table in this RFC document.
    (message "This RFC document contains no Table of Contents.")))

(defun irfc-page-goto (number)
  "Goto page NUMBER."
  (interactive "nPage number: ")
  (cond ((<= number 1)
         ;; Move beginning of buffer when page number
         ;; is equal or below 1.
         (call-interactively 'beginning-of-buffer)
         (if (< number 1)
             (message "Reach top page.")))
        (t
         ;; Move special page.
         (let ((original-position (point))
               (original-render-status irfc-render-p)
               reach-bottom-p)
           ;; Set max page number when
           ;; query page is above max limit.
           (when (> number irfc-total-pages)
             (setq number irfc-total-pages)
             (setq reach-bottom-p t))
           ;; Turn off render.
           (irfc-render-turn-off)
           ;; Search page number.
           (goto-char (point-min))
           (if (re-search-forward (concat "\\[Page " (regexp-quote (number-to-string (1- number))) "\\]$")
                                  nil t)
               ;; Move special page when search successful.
               (progn
                 ;; Adjust cursor position.
                 (forward-line +3)
                 (re-search-forward "^.+$" nil t)
                 (back-to-indentation)
                 ;; Recenter when reach bottom page.
                 (when reach-bottom-p
                   (recenter 0)
                   (message "Reach bottom page.")))
             ;; Restore original position when search failed.
             (goto-char original-position))
           ;; Revert render status.
           (unless (equal original-render-status irfc-render-p)
             (irfc-render-toggle))))))

(defun irfc-page-next (arg)
  "Move to next ARGth page.
ARG defaults to 1."
  (interactive "P")
  (irfc-page-goto (+ (irfc-current-page) (or arg 1))))

(defun irfc-page-prev (arg)
  "Move to previous ARGth page.
ARG defaults to 1."
  (interactive "P")
  (irfc-page-goto (- (irfc-current-page) (or arg 1))))

(defun irfc-page-first ()
  "Move to first page."
  (interactive)
  (irfc-page-goto 1))

(defun irfc-page-last ()
  "Move to last page."
  (interactive)
  (irfc-page-goto irfc-total-pages))

(defun irfc-page-table ()
  "Move to Table of Contents."
  (interactive)
  (if (irfc-have-table-p)
      (progn
        (goto-char (point-min))
        (re-search-forward "^Table of Contents$" nil t)
        (back-to-indentation))
    (message "This RFC document has no Table of Contents.")))

(defun irfc-follow ()
  "Open RFC document around point.
Download and open RFC document if it
does not exist in `irfc-directory'."
  (interactive)
  (let ((rfc-file-name (irfc-get-rfc-filename)))
    (if rfc-file-name
        ;; Open RFC document.
        (irfc-open rfc-file-name)
      (message "Not found valid RFC link at cursor."))))

(defun irfc-visit (&optional rfc-number)
  "Open RFC document RFC-NUMBER.
Download and open RFC document if it
does not exist in `irfc-directory'."
  (interactive)
  (or rfc-number
      (setq rfc-number (read-number
                        "RFC document to visit: "
                        irfc-last-visit-number)))
  (setq irfc-last-visit-number rfc-number)
  (irfc-open (format "rfc%s.txt" rfc-number)))

(defun irfc-head-next ()
  "Move to next heading."
  (interactive)
  (let ((original-position (point)))
    (end-of-line)
    (if (irfc-head-move)
        ;; Move to next heading,
        ;; when search successful.
        (beginning-of-line)
      ;; Restore original position
      ;; when search failed.
      (goto-char original-position)
      (message "No next heading."))))

(defun irfc-head-prev ()
  "Move to previous heading."
  (interactive)
  (let ((original-position (point)))
    (beginning-of-line)
    (unless (irfc-head-move t)
      ;; Restore original position
      ;; when search failed.
      (goto-char original-position)
      (message "No previous heading."))))

(defun irfc-scroll-up-one-line ()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

(defun irfc-scroll-down-one-line ()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(defun irfc-rfc-link-next ()
  "Move the point to the next RFC link."
  (interactive)
  (let ((original-point (point)))
    (if (re-search-forward "\\(\\B\\[RFC-?[0-9]+\\]\\B\\|[ \t]+[0-9]+\\)" nil t)
        (catch 'match
          (while (and (not (string-match "\\[\\(RFC-?[0-9]+\\)\\]" (irfc-get-symbol-non-blank)))
                      (or (not (irfc-title-rfc-link-p)) ;not valid RFC link number
                          (eolp)                        ;number at end of line is invalid RFC number
                          ))
            (unless (re-search-forward "\\(\\B\\[RFC-?[0-9]+\\]\\B\\|[ \t]+[0-9]+\\)" nil t)
              (goto-char original-point)
              (message "No next RFC link.")
              (throw 'match "Match last one.")))))))

(defun irfc-rfc-link-prev ()
  "Move the point to the previous RFC link."
  (interactive)
  (let ((original-point (point)))
    (if (re-search-backward "\\(\\B\\[RFC-?[0-9]+\\]\\B\\|[ \t]+[0-9]+\\)" nil t)
        (catch 'match
          (while
              ;; Not match [RFCnnn] format.
              (not (string-match "\\[\\(RFC-?[0-9]+\\)\\]" (irfc-get-symbol-non-blank)))
            (skip-chars-forward " ")
            (if (and (irfc-title-rfc-link-p) ;is valid RFC link number
                     (save-excursion         ;skip number at end of line.
                       (search-forward-regexp "[0-9]+" nil t)
                       (not (eolp))))
                (progn
                  (if (string-match "^request for comments:[ \t]+$"
                                    (buffer-substring-no-properties (line-beginning-position) (point)))
                      (message "No previous RFC link."))
                  (throw 'match "Match title RFC link."))
              (re-search-backward "\\(\\B\\[RFC-?[0-9]+\\]\\B\\|[ \t]+[0-9]+\\)" nil t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun irfc-open (rfc-file-name)
  "Open RFC document with RFC-FILE-NAME."
  (let (filepath)
    (if (string-equal rfc-file-name (buffer-name))
        ;; Notify user if current buffer is search RFC document.
        (message "Current RFC document.")
      ;; Get full file path.
      (setq filepath (expand-file-name rfc-file-name irfc-directory))
      (cond ((file-exists-p filepath)
             ;; Open corresponding RFC document.
             (if (get-buffer rfc-file-name)
                 ;; Switch corresponding buffer when it open.
                 (switch-to-buffer rfc-file-name)
               ;; Or open that file.
               (find-file filepath)))
            (t
             ;; Otherwise download and open corresponding RFC document.
             (irfc-download (concat irfc-download-base-url rfc-file-name)))))))

(defun irfc-render-buffer-hide-whitespace-at-start ()
  "Hide whitespace at start of file.
Return adjusted point."
  (goto-char (point-min))
  (if (re-search-forward "\\`\\([ \t\f]*\r?\n\\)+" nil t)
      (progn
        (irfc-overlay-hide-region (match-beginning 0) (match-end 0))
        (point))
    nil))

(defun irfc-render-buffer-overlay-page-number ()
  "Add overlays for page headers and footers."
  (let ((headerfooter-re (concat "^[ \t]*"
                                 "\\(\r?\n\\)"        ; #1
                                 "\\([ \t]*\r?\n\\)*" ; #2
                                 "[^ \t\f].*\\[Page "
                                 "\\([0-9iIvVxX]+\\)" ; #3
                                 "\\][ ]*\r?\n?"
                                 "\\("  ; <#4
                                 "\f"
                                 "\\([ \t]*\r?\n\\)?" ; #5
                                 "\\("                ; <#6
                                 "\\("                ; <#7
                                 "RFC [0-9]+"
                                 "\\|"  ; |#7
                                 "Internet-Draft[ \t]"
                                 "\\)"  ; >#7
                                 ".*\r?\n"
                                 "\\([ \t]*\r?\n\\)*" ; #8
                                 "\\)?"               ; >#6
                                 "\\)?"               ; >#4
                                 )))
    (while (re-search-forward headerfooter-re nil t)
      ;; Hide old page information for clear reading.
      (irfc-overlay-hide-region (match-end 1) (match-end 0))
      (when (match-beginning 6)
        (let* ((overlay (irfc-overlay-add (match-beginning 1)
                                          (match-end 1)
                                          'irfc-page-number-overlay))
               ;; Get page number.
               (page-num (1+ (string-to-number (match-string 3))))
               ;; Generate page string.
               (page-str (format "(p.%s)" (number-to-string page-num)))
               ;; Generate new page string.
               (new-str (concat (make-string (max (- 79
                                                     (- (match-beginning 1)
                                                        (match-beginning 0))
                                                     (length page-str))
                                                  0)
                                             32)
                                page-str)))
          ;; Record total pages number.
          (setq irfc-total-pages page-num)
          ;; Overlay new page string instead old one.
          (overlay-put overlay
                       'before-string
                       new-str))))))

(defun irfc-render-buffer-hide-blank-line (top-point)
  "Hide any extraneous blank lines between top header and before title.
Argument TOP-POINT is the top point of RFC buffer after render."
  (goto-char top-point)
  (unless (re-search-forward (concat "^[ \t]*\r?\n"
                                     "\\(\\([ \t]*\r?\n\\)+\\)?")
                             nil t)
    (error "This doesn't seem to be an RFC - no blank line before title"))
  (when (match-beginning 1)
    ;; Hide blanks lines between top header and before title.
    (irfc-overlay-hide-region (match-beginning 1) (match-end 1)))
  (point))

(defun irfc-render-buffer-overlay-rfc-number (top-point title-line-point)
  "Overlay RFC number.
Argument TOP-POINT is the top point of RFC buffer after render.
Argument TITLE-LINE-POINT is the title line point of RFC buffer after render."
  (goto-char top-point)
  (while (let ((case-fold-search t))
           (re-search-forward "^\\(request for comments\\|updates\\|obsoletes\\):\\( RFCs\\)?[ \t]+\\(\\([0-9X]+\\)\\(,[ \t]+[0-9]+\\)*\\)"
                              title-line-point t))
    ;; Overlay RFC number.
    (irfc-overlay-add (match-beginning 3)
                      (match-end 3)
                      'irfc-rfc-number-overlay)))

(defun irfc-render-buffer-overlay-std-number (top-point title-line-point)
  "Overlay STD number.
Argument TOP-POINT is the top point of RFC buffer after render.
Argument TITLE-LINE-POINT is the title line point of RFC buffer after render."
  (goto-char top-point)
  (when (let ((case-fold-search nil))
          (re-search-forward "^STD:[ \t]+[0-9]+"
                             title-line-point t))
    ;; Overlay STD number.
    (irfc-overlay-add (match-beginning 0)
                      (match-end 0)
                      'irfc-std-number-overlay)))

(defun irfc-render-buffer-overlay-table-item (top-point)
  "Overlay valid item in table for jump.
Argument TOP-POINT is the top point of RFC buffer after render."
  (when (irfc-have-table-p)             ;whether have table in current buffer
    (goto-char top-point)
    (let* ((case-fold-search t)
           (start-position (re-search-forward "^Table of Contents$" nil t))
           (end-position (re-search-forward "^[0-9\\.]+" nil t)))
      (goto-char start-position)
      (while (re-search-forward irfc-table-regex end-position t)
        ;; Overlay valid table item.
        (irfc-overlay-add (match-beginning 1)
                          (match-end 1)
                          'irfc-table-item-overlay)))))

(defun irfc-render-buffer-overlay-rfc-link (top-point)
  "Overlay valid RFC link.
Argument TOP-POINT is the top point of RFC buffer after render."
  (goto-char top-point)
  (while (let ((case-fold-search nil))
           (re-search-forward "\\[RFC-?[0-9]+\\]"
                              nil t))
    ;; Overlay valid RFC link.
    (irfc-overlay-add (match-beginning 0)
                      (match-end 0)
                      'irfc-rfc-link-overlay)))

(defun irfc-render-buffer-overlay-title (title-line-point)
  "Add overlays to the title line(s).
Note that we currently assume no blank lines in the title; otherwise
we have to do a perfect job of identifying the first non-title line
\(usually a section heading, which some some RFCs make difficult to
always identify).
Argument TITLE-LINE-POINT is the title line point of RFC buffer after render."
  (goto-char title-line-point)
  (if (re-search-forward (concat
                          "\\([^ \t\f\r\n].*[^ \t\f\r\n]\\)"
                          "\\(\r?\n[ \t]*[^ \t\f\r\n].*[^ \t\f\r\n]\\)*"))
      ;; Overlay title.
      (irfc-overlay-add (match-beginning 0)
                        (match-end       0)
                        'irfc-title-overlay)))

(defun irfc-render-buffer-overlay-head (title-line-point)
  "Overlay heading.
Argument TITLE-LINE-POINT is the title line point of RFC buffer after render."
  (goto-char title-line-point)
  (let (match-list)
    (while (setq match-list (irfc-head-move))
      (if (and (nth 0 match-list) (nth 1 match-list))
          ;; Overlay heading number.
          (irfc-overlay-add (nth 0 match-list)
                            (nth 1 match-list)
                            'irfc-head-number-overlay))
      ;; Overlay heading name.
      (irfc-overlay-add (nth 2 match-list)
                        (nth 3 match-list)
                        'irfc-head-name-overlay))))

(defun irfc-head-move (&optional reverse)
  "Move to special heading.
Return heading list for overlay.
Default is to move to next heading;
move to previous heading if REVERSE is `non-nil'."
  (let ((case-fold-search t)
        ;; Note: We can't just look for lines that begin in column 0, since
        ;; some RFCs put source code, ASCII-art, description list headings,
        ;; body text, and other stuff in column 0.
        ;; So we look for stock headings and ones that appear to
        ;; begin with section numbers.
        (heading-re (concat
                     "^"
                     "\\("                         ; <#1
                     "\\("                         ; <#2 = numbered section
                     "\\("                         ; <#3 = number
                     "\\([0-9]+\\.?\\|[A-Z]\\.\\)" ; #4
                     "\\([0-9]+\\.?\\)*"           ; #5
                     "\\)"                         ; >#3 = number
                     "[ \t]+"
                     "\\([^\r\n]+\\)"   ; #6 = name
                     "\\)"              ; >#2 = numbered section
                     "\\|"              ; |#1
                     "\\("              ; <#7 = stock section
                     "\\("              ; <#8
                     (mapconcat 'identity irfc-stock-section-names "\\|")
                     "\\)"              ; >#8
                     ":?[ \t]*$"
                     "\\)"              ; >#7 = stock section
                     "\\|"              ; |#1
                     "\\("              ; <#9 = lit-appendix

                     "appendix[ \t]+"
                     "\\([A-Z]\\)"      ; #10 = number

                     "\\(\\.\\|:\\|[ \t]+-\\)" ; #11
                     "[ \t]+"
                     "\\([^\r\n]+\\)"   ; #12 = name

                     "\\)"              ; >#9 = lit-appendix
                     "\\)"              ; >#1
                     )))
    (if (if reverse
            ;; Search backward.
            (re-search-backward heading-re nil t)
          ;; Search forward.
          (re-search-forward heading-re nil t))
        (let ((num-match nil)
              (num-highlight-begin nil)
              (num-highlight-end nil)
              (name-match nil))
          ;; Get the match data numbers.
          (cond ((match-beginning 3) (setq num-match 3
                                           name-match 6))
                ((match-beginning 8) (setq num-match nil
                                           name-match 8))
                ((match-beginning 9) (setq num-match 10
                                           name-match 12)
                 (setq num-highlight-begin (match-beginning 9)
                       num-highlight-end (match-end 11)))
                (t (error " should never happen")))
          ;; Return heading list for overlay.
          (list
           (if num-match
               (or num-highlight-begin
                   (match-beginning num-match))
             nil)
           (if num-match
               (or num-highlight-end
                   (match-end num-match))
             nil)
           (match-beginning name-match)
           (match-end name-match)))
      nil)))

(defun irfc-overlay-put-alist (symbol alist)
  "Put special overlay prop with value.
SYMBOL is overlay variable.
ALIST contain special properties for overlay."
  (mapcar (function (lambda (cell)
                      (put symbol (nth 0 cell) (cdr cell))))
          alist))

(defun irfc-overlay-remove-all ()
  "Remove all overlays from current buffer."
  (mapcar (function (lambda (lst)
                      (while lst
                        (delete-overlay (car lst))
                        (setq lst (cdr lst)))))
          (let ((lists (overlay-lists)))
            (list (car lists) (cdr lists)))))

(defun irfc-overlay-add (begin end category)
  "Add overlay.
BEGIN is start position to overlay.
END is end position to overlay.
CATEGORY is special overlay variable."
  (or category (error "Irfc-overlay-add nil category"))
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'category category)
    overlay))

(defun irfc-overlay-hide-region (start end)
  "Use overlay to hide region.
START is start position to hide.
END is end position to hide."
  (irfc-overlay-add start end 'irfc-hide-overlay))

(defun irfc-have-table-p ()
  "Return non-nil if the RFC contain a Table of Contents."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (re-search-forward "^Table of Contents$" nil t))))

(defun irfc-front-table-p ()
  "Return t when point is before the Table of Contents."
  (let ((case-fold-search t)
        (original-position (point))
        table-start-position)
    (save-excursion
      (goto-char (point-min))
      (setq table-start-position (re-search-forward "^Table of Contents$" nil t))
      (< original-position table-start-position))))

(defun irfc-in-table-p ()
  "Return t when point is in the Table of Contents."
  (let ((case-fold-search t)
        (original-position (point))
        table-start-position
        table-end-position)
    (save-excursion
      ;; Get start and end position of table.
      (goto-char (point-min))
      (re-search-forward "^Table of Contents$" nil t)
      (beginning-of-line)
      (setq table-start-position (point))
      (re-search-forward "^[0-9\\.]+" nil t)
      (beginning-of-line)
      (forward-char -1)
      ;; Compare current cursor with table scope.
      (setq table-end-position (point))
      (and (>= original-position table-start-position)
           (<= original-position table-end-position)))))

(defun irfc-current-page ()
  "Return current page number at point."
  (let ((original-render-status irfc-render-p)
        current-page)
    (save-excursion
      ;; Turn off render.
      (irfc-render-turn-off)
      (if (re-search-forward "\\[Page \\([0-9]+\\)\\]$" nil t)
          ;; Set current page number when search successful.
          (setq current-page (string-to-number (match-string 1)))
        ;; Set max page number when search failed.
        (setq current-page irfc-total-pages))
      ;; Revert render status.
      (unless (equal original-render-status irfc-render-p)
        (irfc-render-toggle)))
    current-page))

(defun irfc-download (url)
  "Download RFC document URL.
URL is download URL that base on `irfc-download-base-url'."
  (let* ((url-request-method "GET")
         (url-request-extra-headers nil)
         (url-mime-accept-string "*/*")
         (parsed-url (url-generic-parse-url url))
         download-buffer
         download-buffer-name)
    ;; Get unique buffer for handle download information.
    (setq download-buffer (irfc-get-buffer))
    (setq download-buffer-name (buffer-name download-buffer))
    (with-current-buffer (get-buffer download-buffer-name)
      ;; Bind download url with local buffer.
      (setq irfc-download-url url)
      (setq irfc-download-buffer (url-retrieve parsed-url 'irfc-download-callback (list download-buffer-name))))))

(defun irfc-download-callback (&optional redirect download-buffer-name)
  "Callback for `irfc-download'.
With `irfc-download', this downloads RFC files asynchronously.
REDIRECT is default return argument for `url-retrieve'.
DOWNLOAD-BUFFER-NAME is the buffer name for handling download content."
  (if (eq (car redirect) ':error)
      ;; Output error information if download RFC document failed.
      (with-current-buffer (get-buffer download-buffer-name)
        (message "Not found %s." irfc-download-url)
        (kill-buffer download-buffer-name))
    ;; Decode retrieve information.
    (irfc-retrieve-decode download-buffer-name 'utf-8)
    (with-current-buffer (get-buffer download-buffer-name)
      ;; Write file.
      (write-file (expand-file-name (url-file-nondirectory irfc-download-url) irfc-directory))
      ;; Switch buffer.
      (switch-to-buffer (current-buffer)))))

(defun irfc-retrieve-decode (retrieve-buffer-name coding)
  "Decode the retrieve buffer RETRIEVE-BUFFER-NAME with coding CODING."
  (declare (special url-http-end-of-headers))
  (with-current-buffer (get-buffer retrieve-buffer-name)
    (insert
     (with-current-buffer irfc-download-buffer
       (set-buffer-multibyte t)
       (goto-char (1+ url-http-end-of-headers))
       (decode-coding-region
        (point) (point-max)
        (coding-system-change-eol-conversion coding 'dos))
       (buffer-substring (point) (point-max))))
    (goto-char (point-min))))

(defun irfc-get-buffer ()
  "Get a buffer for temporary storage of downloaded content.
Uses `current-time' to make buffer name unique."
  (let (time-now buffer)
    (setq time-now (current-time))
    (get-buffer-create (format "*%s<%s-%s-%s>*"
                               "irfc"
                               (nth 0 time-now) (nth 1 time-now) (nth 2 time-now)))))

(defun irfc-get-rfc-filename ()
  "Return filename for RFC file.
Look at point and extract an RFC number: either a string `[RFCnnn]',
or a RFC Number in a standard header field (`Updates:', etc.).
In that case, return `rfcnnn.txt'; otherwise return nil."
  (let (case-fold-search
        (symbol (irfc-get-symbol-non-blank)))
    (cond ((string-match "\\[\\(RFC-?[0-9]+\\)\\]" symbol)
           (format "%s.txt" (replace-regexp-in-string "-" "" (downcase (match-string 1 symbol)))))
          ((and (string-match "^\\([0-9]+\\),*$" symbol)
                (irfc-title-rfc-link-p))
           (string-match "^\\([0-9]+\\),*$" symbol)
           (format "rfc%s.txt" (match-string 1 symbol)))
          (t
           nil))))

(defun irfc-title-rfc-link-p ()
  "Return t if current point is at title RFC link.
Otherwise return nil."
  (save-excursion
    (let ((case-fold-search t))
      (search-forward-regexp " \\|$" nil t)
      (skip-chars-backward " ")
      (if (string-match "^\\(request for comments\\|updates\\|obsoletes\\):\\( RFCs\\)?[ \t]+\\(\\([0-9X]+\\)\\(,[ \t]+[0-9]+\\)*\\)\\b"
                        (buffer-substring-no-properties (line-beginning-position) (point)))
          t
        nil))))

(defun irfc-get-symbol-non-blank ()
  "Return symbol between `blank'."
  (save-excursion
    (let (start end)
      (search-backward-regexp " \\|^" nil t)
      (skip-chars-forward " ")
      (setq start (point))
      (search-forward-regexp " \\|$" nil t)
      (skip-chars-backward " ")
      (setq end (point))
      (if (and start
               end
               (>= end start))
          (buffer-substring-no-properties start end)
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overlay setup.
(irfc-overlay-put-alist 'irfc-title-overlay '((face . irfc-title-face)))
(irfc-overlay-put-alist 'irfc-head-name-overlay '((face . irfc-head-name-face)))
(irfc-overlay-put-alist 'irfc-head-number-overlay '((face . irfc-head-number-face)))
(irfc-overlay-put-alist 'irfc-rfc-number-overlay '((face . irfc-rfc-number-face)))
(irfc-overlay-put-alist 'irfc-std-number-overlay '((face . irfc-std-number-face)))
(irfc-overlay-put-alist 'irfc-rfc-link-overlay '((face . irfc-rfc-link-face)))
(irfc-overlay-put-alist 'irfc-table-item-overlay '((face . irfc-table-item-face)))
(irfc-overlay-put-alist 'irfc-hide-overlay
                        '((face . default)
                          (intangible . t)
                          (invisible . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cleanup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun irfc-unload-function ()
  "Unload the Irfc library."
  (dolist (buffer (buffer-list))
    (set-buffer buffer)
    (when (eq major-mode 'irfc-mode)
      (irfc-render-turn-off)
      (text-mode)))
  ;; `nil' mean continue standard unloading.
  nil)

(provide 'irfc)

;;; irfc.el ends here

;;; LocalWords:  irfc IETF lpi rfcview Dyke txt YourStorageDirectory DarkRed cr
;;; LocalWords:  LawnGreen iesg nPage filepath headerfooter iIvVxX num str SPC
;;; LocalWords:  lst eol Juanma Barranquero ARGth RFCnnn backtab rfcnnn regex
;;; LocalWords:  Juanma's
