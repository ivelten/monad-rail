# monad-rail

[![Hackage](https://img.shields.io/hackage/v/monad-rail.svg)](https://hackage.haskell.org/package/monad-rail)
[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](LICENSE)

Railway-Oriented error handling for Haskell.

`monad-rail` implements [Railway-Oriented Programming (ROP)](https://fsharpforfunandprofit.com/rop/) — a functional pattern that makes error handling explicit and composable. Your computation runs on two tracks: success and failure. Once on the failure track, execution stops — unless you use `<!>` to run multiple validations in parallel and collect all their errors at once.

## Installation

Add to your `.cabal` file:

```cabal
build-depends:
  monad-rail ^>=0.1.0.0
```

## Quick Start

### 1. Define your error type

```haskell
import Monad.Rail

data UserError
  = NameEmpty
  | EmailInvalid
  | AgeTooLow
  deriving (Show)

instance HasErrorInfo UserError where
  publicErrorInfo NameEmpty =
    PublicErrorInfo
      { publicMessage = "Name cannot be empty"
      , code          = "USER_NAME_EMPTY"
      , details       = Nothing
      }
  publicErrorInfo EmailInvalid =
    PublicErrorInfo
      { publicMessage = "Invalid email format"
      , code          = "USER_EMAIL_INVALID"
      , details       = Nothing
      }
  publicErrorInfo AgeTooLow =
    PublicErrorInfo
      { publicMessage = "Must be at least 18 years old"
      , code          = "USER_AGE_TOO_LOW"
      , details       = Nothing
      }
```

### 2. Write your validations

```haskell
validateName :: String -> Rail ()
validateName name
  | null name = throwError (SomeError NameEmpty)
  | otherwise = pure ()

validateEmail :: String -> Rail ()
validateEmail email
  | '@' `notElem` email = throwError (SomeError EmailInvalid)
  | otherwise = pure ()

validateAge :: Int -> Rail ()
validateAge age
  | age < 18  = throwError (SomeError AgeTooLow)
  | otherwise = pure ()
```

### 3. Accumulate errors with `<!>`

```haskell
validateUser :: String -> String -> Int -> Rail ()
validateUser name email age = do
  validateName name <!> validateEmail email <!> validateAge age
  -- All three run regardless of failure.
  -- If any fail, ALL errors are collected before stopping.
  saveUser name email age
```

### 4. Run and handle results

```haskell
main :: IO ()
main = do
  result <- runRail (validateUser "" "not-an-email" 16)
  case result of
    Right () ->
      putStrLn "User saved!"
    Left errors ->
      -- Prints all 3 errors as a JSON array
      print errors
```

Output:

```json
[
  {"message":"Name cannot be empty","code":"USER_NAME_EMPTY"},
  {"message":"Invalid email format","code":"USER_EMAIL_INVALID"},
  {"message":"Must be at least 18 years old","code":"USER_AGE_TOO_LOW"}
]
```

## Core Concepts

### `Rail a`

The main type alias for railway computations:

```haskell
type Rail a = RailT Failure IO a
```

Use `RailT` directly if you need a different base monad.

### `throwError`

Moves execution to the failure track with a single error:

```haskell
throwError :: SomeError -> RailT Failure m a
```

All subsequent steps in the `do`-block are skipped.

### `<!>` (error accumulation)

The key operator for Railway-Oriented Programming. Runs **both** sides regardless of failure and combines the errors:

| Left | Right | Result |
| --- | --- | --- |
| `Right` | `Right` | `Right` — continue |
| `Left e1` | `Right` | `Left e1` — stop |
| `Right` | `Left e2` | `Left e2` — stop |
| `Left e1` | `Left e2` | `Left (e1 <> e2)` — stop, both errors |

Ideal for form validation, configuration checks, and any scenario where you want to report all problems at once.

### `tryRail`

Wraps any IO action that may throw exceptions and lifts it into the Railway:

```haskell
tryRail :: HasCallStack => IO a -> Rail a
```

If the action throws, the exception is caught and converted to an `SomeError` wrapping a `CaughtException`. This lets you bring ordinary IO operations into a Railway pipeline without manual exception handling.

```haskell
-- File operations
readConfig :: FilePath -> Rail String
readConfig path = tryRail (readFile path)

-- Combined with validations
pipeline :: FilePath -> Rail ()
pipeline filePath = do
  content <- tryRail (readFile filePath)
  validateName content <!> validateEmail content
  saveToDb content
```

### `tryRailWithCode`

Like `tryRail`, but lets you specify a domain-specific error code:

```haskell
tryRailWithCode :: HasCallStack => Text -> IO a -> Rail a
```

Because the code is the first argument, you can partially apply it to create reusable, domain-specific helpers:

```haskell
tryDb :: HasCallStack => IO a -> Rail a
tryDb = tryRailWithCode "DB_ERROR"

tryHttp :: HasCallStack => IO a -> Rail a
tryHttp = tryRailWithCode "HTTP_ERROR"

pipeline :: Rail ()
pipeline = do
  user <- tryDb   (queryUser userId)
  resp <- tryHttp (fetchProfile user)
  pure ()
```

> **Note:** add `HasCallStack` to your wrapper's own signature so the call stack is captured at each call site rather than frozen at the wrapper's definition.

The resulting error for a caught exception will have:

| Info | Field | Value |
| --- | --- | --- |
| `PublicErrorInfo` | `publicMessage` | `"An unexpected error occurred"` |
| `PublicErrorInfo` | `code` | `"UNCAUGHT_EXCEPTION"` (customizable via `CaughtException`) |
| `InternalErrorInfo` | `internalMessage` | The exception message (logs only) |
| `InternalErrorInfo` | `severity` | `Critical` |
| `InternalErrorInfo` | `exception` | The original `SomeException` |
| `InternalErrorInfo` | `callStack` | Haskell call chain at the `tryRail` call site |

### `CaughtException`

The error type produced by `tryRail`. It wraps `SomeException` and implements `HasErrorInfo`, so it works anywhere a Railway error is expected:

```haskell
data CaughtException = CaughtException
  { caughtCode      :: Text
  , caughtEx        :: SomeException
  , caughtCallStack :: Maybe CallStack
  }
```

When produced by `tryRail`, `caughtCode` defaults to `"UNCAUGHT_EXCEPTION"` and `caughtCallStack` is captured automatically at the call site.

Use it directly when you catch exceptions yourself and want a domain-specific code:

```haskell
import qualified Control.Exception as E

safeQuery :: Rail Row
safeQuery = do
  result <- liftIO $ E.try runQuery
  case result of
    Right row -> pure row
    Left ex   -> throwError (SomeError (CaughtException "DB_QUERY_FAILED" ex Nothing))
```

Or use `throwCaughtEx` for a more concise form — it also captures the call stack automatically:

```haskell
safeQuery :: Rail Row
safeQuery = do
  result <- liftIO $ E.try runQuery
  case result of
    Right row -> pure row
    Left ex   -> throwCaughtEx "DB_QUERY_FAILED" ex
```

### `runRail`

Executes the computation and returns `Either Failure a`:

```haskell
runRail :: Rail a -> IO (Either Failure a)
```

### `runRailT`

The general form of `runRail`, for when your base monad is not `IO`:

```haskell
runRailT :: Monad m => RailT e m a -> m (Either e a)
```

Use it when `RailT` is stacked on top of another transformer, such as `StateT` or `ReaderT`:

```haskell
import Control.Monad.State (StateT, runStateT)

data AppState = AppState { counter :: Int }

type AppRail a = RailT Failure (StateT AppState IO) a

runAppRail :: AppState -> AppRail a -> IO (Either Failure a, AppState)
runAppRail initialState = runStateT . runRailT
```

### `HasErrorInfo`

Typeclass connecting your domain error types to the standard error format:

```haskell
class HasErrorInfo e where
  publicErrorInfo   :: e -> PublicErrorInfo
  internalErrorInfo :: e -> InternalErrorInfo  -- has a default implementation
```

`internalErrorInfo` defaults to `Error` severity with all optional fields set to `Nothing`. Override it when your error carries sensitive diagnostic context for logging.

### `PublicErrorInfo` and `InternalErrorInfo`

Error data is split into two records by visibility:

**`PublicErrorInfo`** — serialized to JSON, safe to return to callers:

| Field | JSON | Purpose |
| --- | --- | --- |
| `publicMessage` | ✅ | Human-readable message safe to show end users |
| `code` | ✅ | Machine-readable identifier |
| `details` | ✅ | Extra JSON context (resource ID, etc.) |

**`InternalErrorInfo`** — for logging and monitoring only. It implements `ToJSON` so you can log it server-side, but `SomeError`'s `ToJSON` instance only serializes `PublicErrorInfo`, so internal fields are never included in API responses:

| Field | Purpose |
| --- | --- |
| `severity` | `Error` or `Critical`, for monitoring |
| `internalMessage` | Sensitive details for logs (stack traces, DB info) |
| `exception` | Underlying exception, for debugging only |
| `requestInfo` | Structured `RequestInfo` with request ID, headers, and body |
| `component` | Subsystem label (`"auth"`, `"payment"`) for log filtering |
| `userId` | Identifier of the user making the request |
| `entrypoint` | API endpoint or handler that was called (e.g. `"POST /api/v1/users"`) |
| `componentVersion` | Version of the component running when the error occurred |
| `callStack` | Haskell call chain at the throw site (requires `HasCallStack`) |

**`RequestInfo`** — structured context about the HTTP request that triggered the error, attached via `requestInfo`:

| Field | Purpose |
| --- | --- |
| `requestId` | Unique request identifier for cross-service correlation |
| `requestHeaders` | HTTP headers as `[(Text, Text)]` name-value pairs; empty list is omitted |
| `requestBody` | Request body as `RequestContent` |

**`RequestContent`** — the request body, as either a structured JSON value or raw text:

| Constructor | Purpose |
| --- | --- |
| `JsonBody Value` | JSON payload — log aggregators can index the fields directly |
| `TextBody Text` | Non-JSON payload (plain text, form-encoded, etc.) stored as raw text |

### Error Severity

```haskell
data ErrorSeverity = Error | Critical
```

Use `Critical` for errors that need immediate attention (e.g., data corruption, infrastructure failures). Use `Error` for recoverable application-level failures.

## Combining Errors from Different Sources

`SomeError` is an existential wrapper, so you can mix error types freely:

```haskell
data DbError = ConnectionFailed deriving (Show)

instance HasErrorInfo DbError where
  publicErrorInfo ConnectionFailed =
    PublicErrorInfo
      { publicMessage = "Service temporarily unavailable"
      , code          = "DB_CONNECTION_FAILED"
      , details = Nothing
      }
  internalErrorInfo ConnectionFailed =
    (internalErrorInfo ConnectionFailed)
      { internalMessage = Just "Postgres replica at 10.0.0.5:5432 unreachable"
      , severity        = Critical
      }

pipeline :: Rail ()
pipeline = do
  validateName name <!> validateEmail email  -- UserError
  fetchFromDb                                -- DbError
```

## JSON Serialization

`Failure` implements `ToJSON` via `aeson`. A failed computation serializes as a JSON array of error objects. Each error is a `SomeError`, whose `ToJSON` instance delegates only to `PublicErrorInfo` — internal diagnostic fields are never included in the output:

```haskell
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BS

result <- runRail myRail
case result of
  Left errors -> BS.putStrLn (encode errors)
  Right _     -> pure ()
```

## License

[BSD-3-Clause](LICENSE) © 2026 Ismael Carlos Velten
