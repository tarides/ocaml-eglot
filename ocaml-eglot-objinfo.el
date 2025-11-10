;;; ocaml-eglot-xreobjinfo.el --- Display build artifact   -*- coding: utf-8; lexical-binding: t -*-

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

(defcustom ocaml-eglot-objinfo-flags
  (list "-shape" "-index" "-decls" "-uid-deps")
  "Flags passed to `ocamlobjinfo'."
  :type '(set
          (const "-shape")
          (const "-index")
          (const "-decls")
          (const "-uid-deps"))
  :group 'ocaml-eglot-objinfo)


;;;###autoload
(defun ocaml-eglot--artifact-file-p (file)
  "Return non-nil if FILE is an OCaml compiled artifact file."
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (and file (member ext '("cmi" "cmti" "cmt"
                            "cmo" "cmx" "cma"
                            "cmxa" "cmxs")))))

;;;###autoload
(define-derived-mode ocaml-eglot-objinfo-mode special-mode "OCaml-objinfo"
  "Mode for displaying ocamlobjinfo output."
  (setq buffer-read-only t
        buffer-offer-save nil
        buffer-file-name nil
        auto-save-default nil
        make-backup-files nil
        create-lockfiles nil)
  (define-key ocaml-eglot-objinfo-mode-map (kbd "q") #'quit-window))

;;;###autoload
(defun ocaml-eglot-objinfo-handler ()
  "Display the result of `ocamlobjinfo` instead of file contents."
  (when (and buffer-file-name
             (ocaml-eglot--artifact-file-p buffer-file-name))
    (let ((file buffer-file-name)
          (inhibit-read-only t))
      (setq buffer-read-only nil)
      (erase-buffer)
      (if (executable-find "ocamlobjinfo")
          (let ((args (append ocaml-eglot-objinfo-flags (list file))))
            (apply #'call-process "ocamlobjinfo" nil t nil args))
        (insert "`ocamlobjinfo' not found in PATH"))
      (goto-char (point-min))
      (ocaml-eglot-objinfo-mode))))

;;;###autoload
(defun ocaml-eglot-objinfo--ocaml-mode-p ()
  "Return non-nil if current buffer's major mode is an OCaml mode."
  (derived-mode-p 'tuareg-mode 'caml-mode 'ocaml-ts-mode
                  'neocaml-mode 'neocamli-mode))

;;;###autoload
(defun ocaml-eglot-objinfo--setup ()
  "Enable OCaml objinfo handler for artifact files in OCaml buffers."
  (when (ocaml-eglot-objinfo--ocaml-mode-p)
    (add-hook 'find-file-hook #'ocaml-eglot-objinfo-handler nil t)))

;;;###autoload
(add-hook 'after-change-major-mode-hook #'ocaml-eglot-objinfo--setup)

(provide 'ocaml-eglot-objinfo)
;;; ocaml-eglot-objinfo.el ends here
