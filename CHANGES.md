unreleased
======================

- Allows known compilation artefacts to be displayed via the `ocamlobjinfo` binary ([#56](https://github.com/tarides/ocaml-eglot/pull/56))

ocaml-eglot 1.2.0
======================
Tue Apr 15 11:14:03 PM CEST 2025

- Fix Type-enclosing's buffer update when using `caml-mode` ([#48](https://github.com/tarides/ocaml-eglot/pull/48))
- Add `ocaml-eglot-search-definition`, `ocaml-eglot-search-declaration` and alternative functions ([#45](https://github.com/tarides/ocaml-eglot/pull/45))
- Fix some warnings on byte-compilation ([#40](https://github.com/tarides/ocaml-eglot/pull/40))
- Fix error on on `ocaml-eglot-construct` ([#42](https://github.com/tarides/ocaml-eglot/pull/40))
- `ocaml-eglot-alternate-file` now visits file in other window when prefix argument is set ([#51](https://github.com/tarides/ocaml-eglot/pull/51))
- Add error-handling for jsonrpc-request ([#52](https://github.com/tarides/ocaml-eglot/pull/52))
- Maintain more diagnostics for location failure ([#52](https://github.com/tarides/ocaml-eglot/pull/52))
- Fix hole cycle navigation ([#53](https://github.com/tarides/ocaml-eglot/pull/53))
- Relay on custom request (if it is available) for managing holes ([#53](https://github.com/tarides/ocaml-eglot/pull/53))
- Implementation of support for experimental client commands (and implementation of `ocaml.next-hole` in the presence of the `ocaml-eglot-destruct` action) ([#54](https://github.com/tarides/ocaml-eglot/pull/54))

ocaml-eglot 1.1.0
======================
Fri Mar 28 01:27:46 PM CET 2025

- A first support for `flycheck` ([#29](https://github.com/tarides/ocaml-eglot/pull/29), [#33](https://github.com/tarides/ocaml-eglot/pull/33) and [#37](https://github.com/tarides/ocaml-eglot/pull/37))
- Use a more efficient way to ensure that a vector is empty ([#27](https://github.com/tarides/ocaml-eglot/pull/27))
- Made the mode-line "lighter" more conventional ([#26](https://github.com/tarides/ocaml-eglot/pull/26))

ocaml-eglot 1.0.0
=================
Fri Jan 17 04:50:35 PM CET 2025

- First release of `ocaml-eglot`
  ([#1](https://github.com/tarides/ocaml-eglot/pull/1),
  [#2](https://github.com/tarides/ocaml-eglot/pull/2),
  [#3](https://github.com/tarides/ocaml-eglot/pull/3),
  [#4](https://github.com/tarides/ocaml-eglot/pull/4),
  [#5](https://github.com/tarides/ocaml-eglot/pull/5),
  [#6](https://github.com/tarides/ocaml-eglot/pull/6),
  [#7](https://github.com/tarides/ocaml-eglot/pull/7),
  [#8](https://github.com/tarides/ocaml-eglot/pull/8),
  [#9](https://github.com/tarides/ocaml-eglot/pull/9),
  [#10](https://github.com/tarides/ocaml-eglot/pull/10),
  [#11](https://github.com/tarides/ocaml-eglot/pull/11),
  [#12](https://github.com/tarides/ocaml-eglot/pull/12),
  [#13](https://github.com/tarides/ocaml-eglot/pull/13),
  [#14](https://github.com/tarides/ocaml-eglot/pull/14),
  [#16](https://github.com/tarides/ocaml-eglot/pull/16),
  [#18](https://github.com/tarides/ocaml-eglot/pull/18),
  [#19](https://github.com/tarides/ocaml-eglot/pull/19),
  [#20](https://github.com/tarides/ocaml-eglot/pull/20),
  [#21](https://github.com/tarides/ocaml-eglot/pull/21),
  [#22](https://github.com/tarides/ocaml-eglot/pull/22))
