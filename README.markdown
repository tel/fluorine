fluorine
========

Yet another attempt to formulate functional reactive programming in Haskell. `fluorine` draws
inspiration from `Elerea` and earlier formulations. Unlike `Elerea`, `fluorine` is based around events.
The foundation of `fluorine` is a monad for time-dependent effects. The time-dependence of values is
left implicit and is automatically managed by the monad. Values are coalgebraic in nature and are
interacted with by message-passing. That's all there is to say.