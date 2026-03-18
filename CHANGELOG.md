# Revision history for monad-rail

## 0.1.0.0 -- 2026-03-17

* First release.
* `RailT` monad transformer for Railway-Oriented Programming.
* `Rail` type alias for `RailT Failure IO`.
* `throwError` for single-error failures.
* `<!>` operator for parallel validation with error accumulation.
* `HasErrorInfo` typeclass for custom error types, with two methods:
  * `publicErrorInfo :: e -> PublicErrorInfo` — user-facing data (serialized to JSON).
  * `internalErrorInfo :: e -> InternalErrorInfo` — sensitive diagnostic data (never serialized). Has a default implementation with `Error` severity and all optional fields set to `Nothing`.
* `PublicErrorInfo` with fields `message`, `code`, and `details` for user-facing error data. Null fields are omitted from JSON output.
* `InternalErrorInfo` with fields `internalMessage`, `severity`, `exception`, `requestInfo`, `component`, and `callStack` for internal diagnostics. Implements `ToJSON` for structured log output; null fields are omitted. Never included in public API responses.
* `ErrorSeverity` with `Error` and `Critical` levels.
* `CaughtException` data type for wrapping runtime exceptions as Railway errors, with fields `caughtCode`, `caughtEx`, and `caughtCallStack`. The code defaults to `"UNCAUGHT_EXCEPTION"` and can be customized when constructing manually.
* `tryRail` (with `HasCallStack`) for lifting IO actions that may throw into the Railway, converting any exception to a `CaughtException` error with `Critical` severity and an automatic call stack.
* `tryRailWithCode` (with `HasCallStack`) — like `tryRail` but accepts a custom error code as the first argument, enabling partial application for domain-specific helpers.
