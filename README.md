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
  errorInfo NameEmpty =
    ErrorInfo
      { publicMessage   = "Name cannot be empty"
      , internalMessage = Nothing
      , code            = "USER_NAME_EMPTY"
      , severity        = Error
      , exception       = Nothing
      , details         = Nothing

      }
  errorInfo EmailInvalid =
    ErrorInfo
      { publicMessage   = "Invalid email format"
      , internalMessage = Nothing
      , code            = "USER_EMAIL_INVALID"
      , severity        = Error
      , exception       = Nothing
      , details         = Nothing

      }
  errorInfo AgeTooLow =
    ErrorInfo
      { publicMessage   = "Must be at least 18 years old"
      , internalMessage = Nothing
      , code            = "USER_AGE_TOO_LOW"
      , severity        = Error
      , exception       = Nothing
      , details         = Nothing

      }
```

### 2. Write your validations

```haskell
validateName :: String -> Rail ()
validateName name
  | null name = throwError (ApplicationError NameEmpty)
  | otherwise = pure ()

validateEmail :: String -> Rail ()
validateEmail email
  | '@' `notElem` email = throwError (ApplicationError EmailInvalid)
  | otherwise = pure ()

validateAge :: Int -> Rail ()
validateAge age
  | age < 18  = throwError (ApplicationError AgeTooLow)
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
  {"message":"Name cannot be empty","code":"USER_NAME_EMPTY","details":null},
  {"message":"Invalid email format","code":"USER_EMAIL_INVALID","details":null},
  {"message":"Must be at least 18 years old","code":"USER_AGE_TOO_LOW","details":null}
]
```

## Core Concepts

### `Rail a`

The main type alias for railway computations:

```haskell
type Rail a = RailT RailError IO a
```

Use `RailT` directly if you need a different base monad.

### `throwError`

Moves execution to the failure track with a single error:

```haskell
throwError :: ApplicationError -> RailT RailError m a
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

### `runRail`

Executes the computation and returns `Either RailError a`:

```haskell
runRail :: Rail a -> IO (Either RailError a)
```

### `HasErrorInfo`

Typeclass connecting your domain error types to the standard error format:

```haskell
class HasErrorInfo e where
  errorInfo :: e -> ErrorInfo
```

### `ErrorInfo`

Holds all metadata for an error. Fields are split by visibility:

| Field | JSON | Purpose |
| --- | --- | --- |
| `publicMessage` | ✅ as `"message"` | Safe to show end users |
| `code` | ✅ | Machine-readable identifier |
| `details` | ✅ | Extra JSON context (user ID, resource ID, etc.) |
| `internalMessage` | ❌ | Sensitive details for logs only |
| `severity` | ❌ | `Error` or `Critical`, for monitoring |
| `exception` | ❌ | Underlying exception, for debugging only |

Fields marked ❌ are intentionally excluded from JSON serialization to prevent accidental exposure in API responses.

### Error Severity

```haskell
data ErrorSeverity = Error | Critical
```

Use `Critical` for errors that need immediate attention (e.g., data corruption, infrastructure failures). Use `Error` for recoverable application-level failures.

## Combining Errors from Different Sources

`ApplicationError` is an existential wrapper, so you can mix error types freely:

```haskell
data DbError = ConnectionFailed deriving (Show)

instance HasErrorInfo DbError where
  errorInfo ConnectionFailed = ErrorInfo
    { publicMessage   = "Service temporarily unavailable"
    , internalMessage = Just "Postgres replica at 10.0.0.5:5432 unreachable"
    , code            = "DB_CONNECTION_FAILED"
    , severity        = Critical
    , exception       = Nothing
    , details         = Nothing
    }

pipeline :: Rail ()
pipeline = do
  validateName name <!> validateEmail email  -- UserError
  fetchFromDb                                -- DbError
```

## JSON Serialization

`RailError` implements `ToJSON` via `aeson`. A failed computation serializes as a JSON array of error objects:

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
