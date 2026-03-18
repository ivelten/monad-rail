# Revision history for monad-rail

## 0.1.0.0 -- 2026-03-18

* First release.
* `RailT` monad transformer for Railway-Oriented Programming.
* `Rail` type alias for `RailT Failure IO`.
* `throwError` for single-error failures.
* `<!>` operator for parallel validation with error accumulation.
* `HasErrorInfo` typeclass for custom error types. Only `errorMessage :: e -> Text` is required. All other methods have defaults:
  * `errorCode :: e -> Text` — defaults to the constructor name via `Data.toConstr` (requires `Data`).
  * `errorDetails :: e -> Maybe Value` — defaults to `Nothing`.
  * `errorSeverity :: e -> ErrorSeverity` — defaults to `Error`.
  * `errorInternalMessage`, `errorException`, `errorRequestInfo`, `errorComponent`, `errorUserId`, `errorEntrypoint`, `errorComponentVersion`, `errorCallStack` — all default to `Nothing`.
* `publicErrorInfo :: HasErrorInfo e => e -> PublicErrorInfo` — assembles the user-facing error record from an instance.
* `internalErrorInfo :: HasErrorInfo e => e -> InternalErrorInfo` — assembles the internal diagnostic record from an instance.
* `PublicErrorInfo` with fields `publicMessage`, `code`, and `details`. Serializes to JSON as `message`, `code`, and `details`. Null fields are omitted from JSON output.
* `InternalErrorInfo` with fields `internalMessage`, `severity`, `exception`, `requestInfo`, `component`, `userId`, `entrypoint`, `componentVersion`, and `callStack`. Implements `ToJSON` for structured log output; null fields are omitted. Never included in public API responses.
* `ErrorSeverity` with `Error` and `Critical` levels.
* `CaughtException` data type for wrapping runtime exceptions as Railway errors, with fields `caughtCode`, `caughtEx`, `caughtCallStack`, and `caughtMessage`. The code defaults to `"UncaughtException"` and can be customized when constructing manually.
* `tryRail` (with `HasCallStack`) for lifting IO actions that may throw into the Railway, converting any exception to a `CaughtException` error with `Critical` severity and an automatic call stack.
* `tryRailWithCode` (with `HasCallStack`) — like `tryRail` but accepts a function `SomeException -> Text` as the first argument, allowing the error code to be derived from the caught exception. Pass `const "MyCode"` for a fixed code, or inspect the exception to return different codes dynamically.
* `tryRailWithError` (with `HasCallStack`, `HasErrorInfo e`) — like `tryRailWithCode`, but takes an error-building function `SomeException -> e` and uses `errorCode` and `errorMessage` from the resulting `HasErrorInfo` instance as the error code and public message.
* `throwCaughtEx` (with `HasCallStack`) — convenience function for throwing a `CaughtException` directly, without manually constructing `SomeError` and `CaughtException`. Captures the call stack automatically at the call site.
* `runRailT` — the general form of `runRail` for custom base monads. Use when `RailT` is stacked on top of `StateT`, `ReaderT`, or any other transformer.
