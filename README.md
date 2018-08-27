# Fell

Freer monads for Clojure and [funcool/cats](http://funcool.github.io/cats/latest/) based on
[Freer Monads, More Extensible Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf).

## Usage

Don't.

```clojure
(ns fell.example
  (:require [fell.core :refer [send state-runner run-reader run]]
            [cats.core :refer [mlet return]]))

(def run-counter (state-runner :counter-state))

(def run-status (state-runner :status-state))

(def stateful-computation
  (mlet [initial-status (send [:status-state :get])
         counter (send [:counter-state :get])
         increment (send [:reader/get])
         _ (send [:counter-state :set (+ counter increment)])
         counter* (send [:counter-state :get])
         _ (send [:status-state :set (str "Energy: " counter)])]
    (return initial-status)))

(-> stateful-computation
    (run-counter 8)
    (run-reader 17)
    (run-status "Asleep")
    run) ;=> [["Asleep" 25] "Energy: 8"]
```

## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
