# Revision history for monad-rail

## 0.1.0.0 -- 2026-03-16

* First release.
* `RailT` monad transformer for Railway-Oriented Programming.
* `Rail` type alias for `RailT RailError IO`.
* `throwError` for single-error failures.
* `<!>` operator for parallel validation with error accumulation.
* `HasErrorInfo` typeclass for custom error types.
* `ErrorInfo` with public/internal message separation and JSON serialization.
* `ErrorSeverity` with `Error` and `Critical` levels.
* `CaughtException` newtype for wrapping runtime exceptions as Railway errors.
* `tryRail` for lifting IO actions that may throw into the Railway, converting any exception to a `CaughtException` error with `Critical` severity.
