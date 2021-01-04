# cubical
A toy programming language with a cubical type system, implemented in Rust.

This doesn't aim to do anything beyond what other cubically typed languages support,
I've mainly been using it as an exercise to help with understanding the [Cubical Type Theory paper](https://arxiv.org/abs/1611.02108).

Feature checklist:
- [x] Pi types
- [x] Path types
- [x] Face systems
- [ ] Composition
- [ ] Kan filling
- [ ] Other operations derived from comp
- [ ] Glueing
- [ ] Universes

Side goals:
- [ ] Sigma types
- [ ] Nat
- [ ] A circle type
    - What else do we need to write a proof term for FundamentGrp(S1) == Z?
- [ ] Support for more general interval/face formulae
