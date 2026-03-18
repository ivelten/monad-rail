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

Implement `HasErrorInfo` with `errorPublicMessage` — the only required method. Derive `Data` to get an automatic error code from the constructor name:

```haskell
{-# LANGUAGE DeriveDataTypeable #-}

import Monad.Rail

data UserError
  = NameEmpty
  | EmailInvalid
  | AgeTooLow
  deriving (Show, Data)

instance HasErrorInfo UserError where
  errorPublicMessage NameEmpty    = "Name cannot be empty"
  errorPublicMessage EmailInvalid = "Invalid email format"
  errorPublicMessage AgeTooLow    = "Must be at least 18 years old"
-- NameEmpty    → { message: "Name cannot be empty",           code: "NameEmpty" }
-- EmailInvalid → { message: "Invalid email format",           code: "EmailInvalid" }
-- AgeTooLow    → { message: "Must be at least 18 years old",  code: "AgeTooLow" }
```

When you need custom codes or extra per-constructor behaviour, override individual methods — see [`HasErrorInfo`](#haserrorinfo) for the full pattern.

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
  {"message":"Name cannot be empty","code":"NameEmpty"},
  {"message":"Invalid email format","code":"EmailInvalid"},
  {"message":"Must be at least 18 years old","code":"AgeTooLow"}
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

If the action throws, the exception is caught and converted to a `SomeError` wrapping an `UnhandledException`. This lets you bring ordinary IO operations into a Railway pipeline without manual exception handling.

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

Like `tryRail`, but lets you derive a domain-specific error code from the caught exception:

```haskell
tryRailWithCode :: HasCallStack => (SomeException -> Text) -> IO a -> Rail a
```

Pass a constant function when the code is fixed, or inspect the exception to return different codes:

```haskell
tryDb :: HasCallStack => IO a -> Rail a
tryDb = tryRailWithCode (const "DbError")

tryHttp :: HasCallStack => IO a -> Rail a
tryHttp = tryRailWithCode $ \ex ->
  if "timeout" `T.isInfixOf` T.pack (displayException ex)
    then "HttpTimeout"
    else "HttpError"

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
| `PublicErrorInfo` | `code` | `"UnhandledException"` (customizable via `tryRailWithCode` or `tryRailWithError`) |
| `InternalErrorInfo` | `internalMessage` | The exception message (logs only) |
| `InternalErrorInfo` | `severity` | `Critical` |
| `InternalErrorInfo` | `exception` | The original `SomeException` |
| `InternalErrorInfo` | `callStack` | Haskell call chain at the `tryRail` call site |

### `tryRailWithError`

Like `tryRailWithCode`, but derives the error code and public message from a `HasErrorInfo` value built from the caught exception:

```haskell
tryRailWithError :: (HasCallStack, HasErrorInfo e) => (SomeException -> e) -> IO a -> Rail a
```

The error-building function receives the `SomeException` that was thrown, allowing the resulting error to carry information extracted from the exception itself. `errorCode` is used as the error code and `errorPublicMessage` as the public message.

```haskell
{-# LANGUAGE DeriveDataTypeable #-}

data DbError = QueryFailed Text | ConnectionLost
  deriving (Show, Data)

instance HasErrorInfo DbError where
  errorPublicMessage (QueryFailed _) = "A database query failed"
  errorPublicMessage ConnectionLost  = "Lost connection to the database"

-- Always map to ConnectionLost, ignoring the exception:
safeQuery :: Rail [Row]
safeQuery = tryRailWithError (\_ -> ConnectionLost) runQuery

-- Inspect the exception to choose the right constructor:
safeQuery' :: Rail [Row]
safeQuery' = tryRailWithError (QueryFailed . T.pack . displayException) runQuery
```

> **Note:** add `HasCallStack` to any wrapper's own signature so the call stack is captured at each call site rather than frozen at the wrapper's definition.

### `UnhandledException`

The error type produced by `tryRail`. It wraps `SomeException` and implements `HasErrorInfo`, so it works anywhere a Railway error is expected:

```haskell
data UnhandledException = UnhandledException
  { unhandledCode      :: Maybe Text
  , unhandledException :: SomeException
  , unhandledCallStack :: Maybe CallStack
  , unhandledMessage   :: Maybe Text
  }
```

When produced by `tryRail`, `unhandledCode` is `Nothing` (defaulting to `"UnhandledException"`), `unhandledCallStack` is captured automatically at the call site, and `unhandledMessage` defaults to `Nothing` (falling back to the generic public message `"An unexpected error occurred"`).

Use `throwUnhandledException` when you catch exceptions yourself and the default code is sufficient:

```haskell
import qualified Control.Exception as E

safeQuery :: Rail Row
safeQuery = do
  result <- liftIO $ E.try runQuery
  case result of
    Right row -> pure row
    Left ex   -> throwUnhandledException ex
```

Use `throwUnhandledExceptionWithCode` when you need a domain-specific code — it also captures the call stack automatically:

```haskell
safeQuery :: Rail Row
safeQuery = do
  result <- liftIO $ E.try runQuery
  case result of
    Right row -> pure row
    Left ex   -> throwUnhandledExceptionWithCode "DbQueryFailed" ex
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

Typeclass connecting your domain error types to the standard error format. Only `errorPublicMessage` is required — all other methods have sensible defaults:

```haskell
class HasErrorInfo e where
  errorPublicMessage   :: e -> Text                -- Required
  errorCode            :: e -> Text                -- Default: constructor name via Data
  errorDetails         :: e -> Maybe Value         -- Default: Nothing
  errorSeverity        :: e -> ErrorSeverity       -- Default: Error
  errorInternalMessage :: e -> Maybe Text          -- Default: Nothing
  errorException       :: e -> Maybe SomeException -- Default: Nothing
  errorCallStack       :: e -> Maybe CallStack     -- Default: Nothing
```

Use `publicErrorInfo` and `internalErrorInfo` to assemble the corresponding records from any instance:

```haskell
publicErrorInfo  :: HasErrorInfo e => e -> PublicErrorInfo
internalErrorInfo :: HasErrorInfo e => e -> InternalErrorInfo
```

#### Simple errors — implement `errorPublicMessage` only

Derive `Data` and implement `errorPublicMessage`. The `errorCode` default derives the error code from the constructor name via `Data.toConstr`:

```haskell
{-# LANGUAGE DeriveDataTypeable #-}

data OrderError = ItemOutOfStock | PaymentDeclined
  deriving (Show, Data)

instance HasErrorInfo OrderError where
  errorPublicMessage ItemOutOfStock  = "One or more items are out of stock"
  errorPublicMessage PaymentDeclined = "Payment was declined"
-- errorCode = "ItemOutOfStock" or "PaymentDeclined"
```

> **Note:** the error code is the constructor name verbatim. Renaming a constructor silently changes its code, so treat constructor names as part of your public API contract.

#### Full control — override individual methods

Override any combination of methods when you need custom codes, `errorDetails`, severity, or internal context. Methods you do not override keep their defaults:

```haskell
instance HasErrorInfo OrderError where
  errorPublicMessage ItemOutOfStock  = "One or more items are out of stock"
  errorPublicMessage PaymentDeclined = "Payment was declined"

  -- Custom codes
  errorCode ItemOutOfStock  = "OrderItemOutOfStock"
  errorCode PaymentDeclined = "OrderPaymentDeclined"

  -- Override severity for one constructor only
  errorSeverity PaymentDeclined = Critical

  -- Override internal message for one constructor
  errorInternalMessage PaymentDeclined = Just "Stripe returned decline code: insufficient_funds"
```

### `PublicErrorInfo` and `InternalErrorInfo`

Error data is split into two records by visibility. Use the `publicErrorInfo` and `internalErrorInfo` functions to obtain them from any `HasErrorInfo` instance.

**`PublicErrorInfo`** — serialized to JSON, safe to return to callers:

| JSON key | Field | `HasErrorInfo` method |
| --- | --- | --- |
| `message` | `publicMessage` | `errorPublicMessage` |
| `code` | `code` | `errorCode` |
| `details` | `details` | `errorDetails` |

**`InternalErrorInfo`** — for logging and monitoring only. It implements `ToJSON` so you can log it server-side, but `SomeError`'s `ToJSON` instance only serializes `PublicErrorInfo`, so internal fields are never included in API responses:

| JSON key | Field | `HasErrorInfo` method |
| --- | --- | --- |
| `severity` | `severity` | `errorSeverity` |
| `message` | `internalMessage` | `errorInternalMessage` |
| `exception` | `exception` | `errorException` |
| `callStack` | `callStack` | `errorCallStack` |

### Error Severity

```haskell
data ErrorSeverity = Error | Critical
```

Use `Critical` for errors that need immediate attention (e.g., data corruption, infrastructure failures). Use `Error` for recoverable application-level failures.

## Combining Errors from Different Sources

`SomeError` is an existential wrapper, so you can mix error types freely:

```haskell
data DbError = ConnectionFailed deriving (Show, Data)

instance HasErrorInfo DbError where
  errorPublicMessage   ConnectionFailed = "Service temporarily unavailable"
  errorCode            ConnectionFailed = "DbConnectionFailed"
  errorSeverity        ConnectionFailed = Critical
  errorInternalMessage ConnectionFailed = Just "Postgres replica at 10.0.0.5:5432 unreachable"

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
