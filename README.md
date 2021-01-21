# cubical
A toy programming language with a cubical type system, implemented in Rust.

This doesn't aim to do anything beyond what other cubically typed languages support,
I've mainly been using it as an exercise to help with understanding Cohen, Coquand, Huber and Mörtberg's [Cubical Type Theory paper](https://arxiv.org/abs/1611.02108).

### Example Syntax

The following term defines path-composition for a type A. (See section 4, example 4 of the CTT paper for details.)
```
Lambda (a: A) =>
Lambda (b: A) =>
Lambda (c: A) =>
Lambda (p: Path A a b) =>
Lambda (q: Path A b c) =>
PathBind i =>
Comp j A [(i=0)->a, (i=1)->q j] (p i)
```

### Progress

Feature checklist:
- [x] Pi types
- [x] Path types
- [x] Face systems
- [x] Composition
- [x] Kan filling
- [x] Sigma types
- [ ] Other operations derived from comp
    - [x] transport
    - [x] contraction
    - [x] pres
    - [ ] equiv
- [ ] Glueing
- [ ] Universes

Side goals:
- [ ] Nat
- [ ] A circle type
    - What else do we need to write a proof term for FundamentGrp(S1) == Z?
- [ ] Support for more general interval/face formulae
