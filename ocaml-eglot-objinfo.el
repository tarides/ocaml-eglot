;;; ocaml-eglot-objinfo.el --- Display build artifact   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2025  The OCaml-eglot Project Contributors
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>
;; Created: 10 November 2025
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Display build-artifact using `ocamlobjinfo'

;;; Code:

(defgroup ocaml-eglot-objinfo nil
  "Display OCaml object information."
  :group 'ocaml-eglot)

;;;###autoload
(add-to-list
 'auto-mode-alist
 '("\\.\\(cmi\\|cmti\\|cmt\\|cmo\\|cmx\\|cma\\|cmxa\\|cmxs\\)\\'"
   . ocaml-eglot-ocamlobjinfo-mode) t)

(defcustom ocaml-eglot-objinfo-flags
  (list "-shape" "-index" "-decls" "-uid-deps")
  "Flags passed to `ocamlobjinfo'."
  :type '(set
          (const "-shape")
          (const "-index")
          (const "-decls")
          (const "-uid-deps"))
  :group 'ocaml-eglot-objinfo)

(defun ocaml-eglot-ocamlobjinfo--handler ()
  "Display the result of `ocamlobjinfo' instead of file content."
  (let ((file buffer-file-name)
        (inhibit-read-only t))
    (if (executable-find "ocamlobjinfo")
        (progn
          (setq buffer-read-only nil)
          (erase-buffer)
          (apply #'call-process "ocamlobjinfo" nil t nil
                   (append ocaml-eglot-objinfo-flags
                           (list file)))
          (goto-char (point-min)))
      (user-error "`ocamlobjinfo' not found in PATH"))))

;;;###autoload
(define-derived-mode ocaml-eglot-ocamlobjinfo-mode special-mode "OCamlobjinfo"
  "Major Mode for displaying `ocamlobjinfo' output."
  (buffer-disable-undo)
  (ocaml-eglot-ocamlobjinfo--handler)
  (setq buffer-read-only t
        buffer-file-name nil
        buffer-offer-save nil
        auto-save-default nil
        make-backup-files nil
        create-lockfiles nil)
  (define-key ocaml-eglot-ocamlobjinfo-mode-map (kbd "q") #'quit-window))

(provide 'ocaml-eglot-objinfo)
;;; ocaml-eglot-objinfo.el ends here
