;;; ocaml-eglot-request.el --- LSP custom request   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2024  Xavier Van de Woestyne
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>
;; Created: 20 September 2024
;; Version: 1.0
;; Keywords: ocaml languages
;; Package-Requires: ((emacs "29.0"))
;; URL: https://github.com/xvw/ocaml-eglot

;;; Commentary

;; Set of functions for interacting with the ocaml-lsp-server via
;; JSONRPC requests. This module is internal and part of the
;; ocaml-eglot project. An add-on to the Emacs Eglot LSP client for
;; editing OCaml code.

;;; Code

(require 'cl-lib)
(require 'eglot)
(require 'jsonrpc)

(defgroup ocaml-eglot-req nil
  "All custom requests supported by ocaml-lsp-server."
  :link '(url-link "https://ocaml.org")
  :group 'languages
  :prefix "ocaml-eglot-req-")


;;; Low-level plumbing to execute a request

(defun ocaml-eglot-req--current-server ()
  "Return current logical Eglot server connection or error."
  (eglot--current-server-or-lose))

(cl-defun ocaml-eglot-req--send (method params &key
                                        immediate
                                        timeout
                                        cancel-on-input
                                        cancel-on-input-retval)
  "Executes a custom request on the current LSP server"
  (let ((server (ocaml-eglot-req--current-server)))
    (unless immediate (eglot--signal-textDocument/didChange))
    (jsonrpc-request server method params
                     :timeout timeout
                     :cancel-on-input cancel-on-input
                     :cancel-on-input-retval cancel-on-input-retval)))

;;; Parameters structures

(defun ocaml-eglot-req--TextDocumentIdentifier ()
  "Compute `TextDocumentIdentifier' object for current buffer."
  (eglot--TextDocumentIdentifier))

(defun ocaml-eglot-req--TextDocumentPositionParams ()
  "Compute `TextDocumentPositionParams' object for the current buffer."
  (append
   (eglot--TextDocumentPositionParams)
   (ocaml-eglot-req--TextDocumentIdentifier)))

(defun ocaml-eglot-req--ConstructParams (depth with-local-values)
  "Compute `ConstructParams' object for current buffer."
  (append (ocaml-eglot-req--TextDocumentPositionParams)
          `(:depth, depth)
          `(:withValues,
            (if (or with-local-values ocaml-eglot-construct-with-local-values)
                "local"
              "none"))))

(defun ocaml-eglot-req--SearchParams (query limit)
  "Compute the `SearchParams' object for the current buffer."
  (append (ocaml-eglot-req--TextDocumentPositionParams)
          `(:query, query)
          `(:limit, (or
                     (if (> limit 1) limit nil)
                     ocaml-eglot-type-search-limit 25))
          `(:with_doc, (if ocaml-eglot-type-search-include-doc
                           :json-true :json-false))
          `(:doc_dormat, ocaml-eglot-preferred-markupkind)))

;;; Concrete requests

(defun ocaml-eglot-req--jump (target)
  "Execute the `ocamllsp/jump' request with a given target."
  (let ((params (append (ocaml-eglot-req--TextDocumentPositionParams)
                        `(:target, target))))
    (ocaml-eglot-req--send :ocamllsp/jump params)))

(defun ocaml-eglot-req--construct (depth with-local-value)
  "Execute the `ocamllsp/construct'."
  (let ((params (ocaml-eglot-req--ConstructParams depth with-local-value)))
    (ocaml-eglot-req--send :ocamllsp/construct params)))

(defun ocaml-eglot-req--search (query limit)
  "Execute the `ocamllsp/typeSearch'."
  (let ((params (ocaml-eglot-req--SearchParams query limit)))
    (ocaml-eglot-req--send :ocamllsp/typeSearch params)))

(defun ocaml-eglot-req--holes ()
  "Returns a list of all the typed holes in the document as an range list."
  (let ((params (ocaml-eglot-req--TextDocumentIdentifier)))
    (append (ocaml-eglot-req--send :ocamllsp/typedHoles params) nil)))

(provide 'ocaml-eglot-request)
;;; ocaml-eglot-request ends here
