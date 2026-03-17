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
