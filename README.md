# Fell

Freer monads for Clojure and [funcool/cats](http://funcool.github.io/cats/latest/) based on
[Freer Monads, More Extensible Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf) and
[Effect Handlers in Scope](https://www.cs.ox.ac.uk/people/nicolas.wu/papers/Scope.pdf).

## Usage

Don't.

```clojure
(ns fell.example
  (:require [cats.core :refer [mlet return]]
            [fell.core :as fell :refer [request-eff]]
            [fell.reader :as r]
            [fell.state :as state]))

;; Workaround for lack of parametric modules.
;; Could use singletons instead, but fn usages would be more verbose.
(let [{ask :ask, run-reader :run} (r/make ::reader)]
  (def ask ask)
  (def run-reader run-reader))

(let [{get-counter :get, set-counter :set, run-counter :run} (state/make ::counter)]
  (def get-counter get-counter)
  (def set-counter set-counter)
  (def run-counter run-counter))

(let [{get-status :get, set-status :set, run-status :run} (state/make ::status)]
  (def get-status get-status)
  (def set-status set-status)
  (def run-status run-status))

(def stateful-computation
  (mlet [initial-status get-status
         counter get-counter
         increment ask
         _ (set-counter (+ counter increment))
         counter* get-counter
         _ (set-status (str "Energy: " counter))]
    (return initial-status)))

(-> stateful-computation
    (run-counter 8)
    (run-reader 17)
    (run-status "Asleep")
    fell/run) ;=> #<Pair ["Energy: 8" #<Pair [25 "Asleep"]>]>
```

## TODO

- [ ] More tests
- [ ] API (and internal naming) design and refactoring
- [ ] More effects (e.g. bracket, threading)
- [ ] Documentation
- [ ] Performance optimization
- [ ] Tail call "optimization"

## License

Copyright © 2019 Pauli Jaakkola

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
