;;; ocaml-eglot-req.el --- LSP custom request   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2024-2025  Xavier Van de Woestyne
;; Licensed under the MIT license.

;; Author: Xavier Van de Woestyne <xaviervdw@gmail.com>
;; Created: 20 September 2024
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Set of functions for interacting with the ocaml-lsp-server via
;; JSONRPC requests.  This module is internal and part of the
;; ocaml-eglot project.  An add-on to the Emacs Eglot LSP client for
;; editing OCaml code.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'ocaml-eglot-util)
(require 'jsonrpc)

;;; Low-level plumbing to execute a request

(defun ocaml-eglot-req--current-server ()
  "Return current logical Eglot server connection or error."
  (eglot--current-server-or-lose))

(cl-defun ocaml-eglot-req--send (method params &key
                                        immediate
                                        timeout
                                        cancel-on-input
                                        cancel-on-input-retval
                                        fallback
                                        server)
  "Execute a custom request on the current LSP server.
METHOD is the dedicated lsp server request, PARAMS is the parameters of the
query, IMMEDIATE is a flag to trigger the request only if the document has
changed, TIMEOUT is a timeout time response.  CANCEL-ON-INPUT,
CANCEL-ON-INPUT-RETVAL are hooks for cancellation and FALLBACK is a hook when
request fails.  SERVER can also be conditionnaly given."
  (let ((server (or server (ocaml-eglot-req--current-server))))
    (unless immediate (eglot--signal-textDocument/didChange))
    (condition-case err
        (jsonrpc-request server method params
                         :timeout timeout
                         :cancel-on-input cancel-on-input
                         :cancel-on-input-retval cancel-on-input-retval)
      (jsonrpc-error (if fallback
                         (funcall fallback err)
                       (signal (car err) (cdr err)))))))

(defun ocaml-eglot-req--server-capable (&rest feats)
  "Determine if current server is capable of FEATS."
  (if (fboundp 'eglot-server-capable)
      (apply #'eglot-server-capable feats)
    ;; Before Emacs 30
    (with-no-warnings (apply #'eglot--server-capable feats))))

(defun ocaml-eglot-req--server-capable-or-lose (&rest feats)
  "Determine if current server is capable of FEATS (or fail)."
  (if (fboundp 'eglot-server-capable-or-lose)
      (apply #'eglot-server-capable-or-lose feats)
    ;; Before Emacs 30
    (with-no-warnings (apply #'eglot--server-capable-or-lose feats))))

;;; Parameters structures

(defun ocaml-eglot-req--TextDocumentIdentifier ()
  "Compute `TextDocumentIdentifier' object for current buffer."
  (eglot--TextDocumentIdentifier))

(defun ocaml-eglot-req--PlainUri ()
  "A hack for requests that do not respect the URI parameter scheme."
  (make-vector 1 (ocaml-eglot-util--current-uri)))

(defun ocaml-eglot-req--TextDocumentPositionParams ()
  "Compute `TextDocumentPositionParams' object for the current buffer."
  (append
   (eglot--TextDocumentPositionParams)
   (ocaml-eglot-req--TextDocumentIdentifier)))

(defun ocaml-eglot-req--TextDocumentPositionParamsWithPos (position)
  "Compute `TextDocumentPositionParams' object for the current buffer.
With a given POSITION"
  (append (list :textDocument (ocaml-eglot-req--TextDocumentIdentifier)
                :position position)
          (ocaml-eglot-req--TextDocumentIdentifier)))

(defun ocaml-eglot-req--ConstructParams (position depth with-local-values)
  "Compute `ConstructParams' object for current buffer.
POSITION the position of the hole.
DEPTH is the depth of the search (default is 1).
WITH-LOCAL-VALUES is a flag for including local values in construction."
  (append (ocaml-eglot-req--TextDocumentPositionParamsWithPos position)
          `(:depth, depth)
          `(:withValues, with-local-values)))

(defun ocaml-eglot-req--SearchParams (query limit with-doc markup-kind)
  "Compute the `SearchParams' object for the current buffer.
QUERY is the requested type-search query and LIMIT is the number of
results to return.  If WITH-DOC is non-nil, the documentation will be
included and the documentation output can be set using MARKUP-KIND."
  (append (ocaml-eglot-req--TextDocumentPositionParams)
          `(:query, query)
          `(:limit, limit)
          `(:with_doc, with-doc)
          `(:doc_dormat, markup-kind)))

(defun ocaml-eglot-req--GetDocumentationParam (identifier markup-kind)
  "Compute the `GetDocumentationParam'.
A potential IDENTIFIER can be given and MARKUP-KIND can be parametrized."
  (let ((params (append (ocaml-eglot-req--TextDocumentPositionParams)
                        `(:contentFormat, markup-kind))))
    (if identifier (append params `(:identifier, identifier))
      params)))

(defun ocaml-eglot-req--TypeEnclosingParams (at index verbosity)
  "Compute the `TypeEnclosingParams'.
AT is the range or the position.
INDEX is the index of the enclosing.
VERBOSITY is a potential verbosity index."
  (append (list :textDocument (ocaml-eglot-req--TextDocumentIdentifier))
          (ocaml-eglot-req--TextDocumentIdentifier)
          `(:at, at)
          `(:index, index)
          `(:verbosity, verbosity)))

;;; Concrete requests

(defun ocaml-eglot-req--jump ()
  "Execute the `ocamllsp/jump' request."
  (let ((params (ocaml-eglot-req--TextDocumentPositionParams)))
    (ocaml-eglot-req--send :ocamllsp/jump params)))

(defun ocaml-eglot-req--construct (position depth with-local-value)
  "Execute the `ocamllsp/construct' request for a given POSITION.
DEPTH and WITH-LOCAL-VALUE can be parametrized."
  (let ((params (ocaml-eglot-req--ConstructParams
                 position depth with-local-value)))
    (ocaml-eglot-req--send :ocamllsp/construct params)))

(defun ocaml-eglot-req--search (query limit with-doc markup-kind)
  "Execute the `ocamllsp/typeSearch' request with a QUERY and a LIMIT.
If WITH-DOC is not nil, it include the documentation in the result.
The markup used to format documentation can be set using MARKUP-KIND."
  (let ((params
         (ocaml-eglot-req--SearchParams query limit with-doc markup-kind)))
    (append (ocaml-eglot-req--send :ocamllsp/typeSearch params) nil)))

(defun ocaml-eglot-req--holes ()
  "Execute the `ocamllsp/typedHoles' request."
  (let ((params (ocaml-eglot-req--TextDocumentIdentifier)))
    (append (ocaml-eglot-req--send :ocamllsp/typedHoles params) nil)))

(defun ocaml-eglot-req--hole (position &optional direction range)
  "Get the following (DIRECTION) hole since POSITION.
In an optional RANGE.  Relaying on `ocamllsp/jumpToTypedHole'"
  (let* ((direction (pcase direction
                      ('prev "prev")
                      (_ "next")))
         (textpos (ocaml-eglot-req--TextDocumentPositionParamsWithPos position))
         (params (append textpos
                         `(:direction, direction)
                         `(:range, range))))
    (ocaml-eglot-req--send :ocamllsp/jumpToTypedHole params)))

(defun ocaml-eglot-req--switch-file (uri)
  "Execute the `ocamllsp/switchImplIntf' request with a given URI."
  (let ((params (make-vector 1 uri)))
    (ocaml-eglot-req--send :ocamllsp/switchImplIntf params)))

(defun ocaml-eglot-req--infer-intf (uri)
  "Execute the `ocamllsp/inferIntf' request with a given URI."
  (let ((params (make-vector 1 uri)))
    (ocaml-eglot-req--send :ocamllsp/inferIntf params)))

(defun ocaml-eglot-req--get-documentation (identifier markup-kind)
  "Execute the `ocamllsp/getDocumentation'.
If IDENTIFIER is non-nil, it documents it, otherwise, it use the identifier
under the cursor.  The MARKUP-KIND can also be configured."
  (let ((params (ocaml-eglot-req--GetDocumentationParam
                 identifier
                 markup-kind)))
    (ocaml-eglot-req--send :ocamllsp/getDocumentation params)))

(defun ocaml-eglot-req--locate-fallback (err)
  "A fallback for printing ERR from locate queries."
  (let ((error-data (alist-get 'jsonrpc-error-data err)))
      (eglot--error "%s" error-data)))

(defun ocaml-eglot-req--definition ()
  "Execute the `textDocument/definition' request for the current point."
  (let ((params (ocaml-eglot-req--TextDocumentPositionParams)))
    (ocaml-eglot-req--send :textDocument/definition params
                           :fallback 'ocaml-eglot-req--locate-fallback)))

(defun ocaml-eglot-req--type-definition ()
  "Execute the `textDocument/typeDefinition' request for the current point."
  (let ((params (ocaml-eglot-req--TextDocumentPositionParams)))
    (ocaml-eglot-req--send :textDocument/typeDefinition params
                           :fallback 'ocaml-eglot-req--locate-fallback)))

(defun ocaml-eglot-req--declaration ()
  "Execute the `textDocument/declaration' request for the current point."
  (let ((params (ocaml-eglot-req--TextDocumentPositionParams)))
    (ocaml-eglot-req--send :textDocument/declaration params
                           :fallback 'ocaml-eglot-req--locate-fallback)))

(defun ocaml-eglot-req--type-enclosings (at index verbosity)
  "Execute the `ocamllsp/typeEnclosing' request for the current point.
AT is the range or the position.
INDEX is the index of the enclosing.
VERBOSITY is a potential verbosity index."
  (let ((params (ocaml-eglot-req--TypeEnclosingParams at index verbosity)))
    (ocaml-eglot-req--send :ocamllsp/typeEnclosing params)))

(defun ocaml-eglot-req--call-code-action (beg end action-kind)
  "Call ACTION-KIND promptly (at BEG . END)."
  (eglot-code-actions beg end action-kind t))

(defun ocaml-eglot-req--destruct (beg end)
  "Call code-action `destruct' for a given position BEG/END."
  (let ((action-kind "destruct (enumerate cases)"))
    (ocaml-eglot-req--call-code-action beg end action-kind)))

(defun ocaml-eglot-req--merlin-call (command argv)
  "Use tunneling `ocamllsp/merlinCallCompatible'.
COMMAND is the command of the Merlin Protocol.
ARGV is the list of arguments."
  (let ((params (append (ocaml-eglot-req--TextDocumentIdentifier)
                        `(:command, command)
                        `(:resultAsSexp, :json-false)
                        `(:args, argv))))
    (ocaml-eglot-req--send :ocamllsp/merlinCallCompatible params)))

(defun ocaml-eglot-req--phrase (target)
  "Compute the beginning of the phrase referenced by TARGET."
  ;; TODO: use a dedicated custom request instead of tunneling
  (let ((argv (vector "-position" (ocaml-eglot-util-point-as-arg (point))
                      "-target" target)))
    (ocaml-eglot-req--merlin-call "phrase" argv)))

(defun ocaml-eglot-req--type-expression (expression)
  "Get the type of EXPRESSION inside the local context."
  ;; TODO: use a dedicated custom request instead of tunneling
  (let ((argv (vector "-position" (ocaml-eglot-util-point-as-arg (point))
                      "-expression" expression)))
    (ocaml-eglot-req--merlin-call "type-expression" argv)))

(defun ocaml-eglot-req--locate-ident (ident look-for)
  "Locate an identifier (IDENT) using `merlin-locate'.
LOOK-FOR is used to define if it should find the interface
or the implementation."
  ;; TODO: use a dedicated custom request instead of tunneling
  (let ((argv (vector "-position" (ocaml-eglot-util-point-as-arg (point-max))
                      "-prefix" ident
                      "-look-for" (pcase look-for
                                    ('interface "interface")
                                    (_ "implementation")))))
    (ocaml-eglot-req--merlin-call "locate" argv)))

(provide 'ocaml-eglot-req)
;;; ocaml-eglot-req.el ends here
