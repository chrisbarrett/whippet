# Whippet Language [![Build Status](https://travis-ci.org/chrisbarrett/whippet.svg?branch=master)](https://travis-ci.org/chrisbarrett/whippet)

An experimental pure functional language. Goals:

- statically typed
- strictly evaluated
- uses System F with row polymorphism as the core calculus
- compiles to Emacs Lisp (or possibly lapcode).

Current status:

- [ ] Reference
- [x] Parser
- [ ] Typechecking
- [ ] Desugaring
- [ ] System F
- [ ] Code gen

We'll see how far I get. ;)

## Syntax Example

The current Whippet syntax is OCaml-inspired, though it might become more
Haskelly as I iterate on the design.

```rust
type Option a = Some a | None

signature Option {
  let get : a -> Option a -> a
  let some? : Option a -> Bool
  let none? : Option a -> Bool
  let map : (a -> b) -> Option a -> Option b
  let flatMap : (a -> Option b) -> Option a -> Option b
}

module Option {
  let get d = fn {
    | Some x -> x
    | None   -> d
  }

  let some? = fn {
    | Some _ -> True
    | None   -> False
  }

  let none? = fn {
    | Some _ -> False
    | None   -> True
  }

  let map f = fn {
    | Some x -> Some (f x)
    | None   -> None
  }

  let flatMap f = fn {
    | Some x -> f x
    | None   -> None
  }
}
```

## Hacking

Make sure you have [Stack][] installed, then run:

```sh
git clone https://github.com/chrisbarrett/whippet.git
cd whippet
stack build
```

[Stack]: http://docs.haskellstack.org/en/stable/install_and_upgrade/
