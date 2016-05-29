# Whippet Language

An experimental pure functional language. Goals:

- statically typed
- strictly evaluated
- uses System F with row polymorphism as the core calculus
- compiles to Emacs Lisp (or possible lapcode).

Current status:

- [ ] Parser _[in progress]_
- [ ] Desugaring
- [ ] System F
- [ ] Code gen

We'll see how far I get. ;)

## Hacking

Make sure you have [Stack][] installed, then run:

```sh
git clone https://github.com/chrisbarrett/whippet.git
cd whippet
stack build
```

[Stack]: http://docs.haskellstack.org/en/stable/install_and_upgrade/
