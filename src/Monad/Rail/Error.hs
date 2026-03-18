{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides the core error handling types and type classes for the Railway-Oriented monad.
--
-- The module defines a hierarchy of error types, built around two complementary records:
-- 'PublicErrorInfo' for data safe to expose to end users, and 'InternalErrorInfo' for
-- diagnostic data intended only for logging and monitoring.
-- These types work together to provide a flexible, type-safe error handling system that supports
-- error accumulation and serialization to JSON.
--
-- == Quick Start
--
-- 1. Define your error type and implement 'HasErrorInfo':
--
-- >>> data UserError = NameEmpty | EmailInvalid
-- >>> instance HasErrorInfo UserError where
-- >>>   publicErrorInfo NameEmpty   = PublicErrorInfo "Name cannot be empty" "USER_NAME_EMPTY" Nothing
-- >>>   publicErrorInfo EmailInvalid = PublicErrorInfo "Email is invalid" "USER_EMAIL_INVALID" Nothing
--
-- 2. Wrap it in 'SomeError' to use in your Railway:
--
-- >>> throwError (SomeError NameEmpty)
--
-- 3. Run your Railway and handle the result:
--
-- >>> result <- runRail myComputation
-- >>> case result of
-- >>>   Right value -> putStrLn "Success!"
-- >>>   Left errors -> print errors  -- Automatically serializes to JSON
module Monad.Rail.Error
  ( ErrorSeverity (..),
    PublicErrorInfo (..),
    RequestContent (..),
    RequestInfo (..),
    InternalErrorInfo (..),
    HasErrorInfo (..),
    SomeError (..),
    CaughtException (..),
    Failure (..),
  )
where

import qualified Control.Exception as E
import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (CallStack, prettyCallStack)

-- | Represents the severity level of an application error.
--
-- Severity levels are used to categorize errors by their importance and urgency.
-- This information is useful for logging, monitoring, and error handling strategies.
data ErrorSeverity
  = -- | Indicates a standard error that occurred during the execution of a Railway.
    -- Standard errors are recoverable and do not require immediate attention.
    Error
  | -- | Indicates a critical error that occurred during the execution of a Railway.
    -- Critical errors may require immediate attention and indicate serious problems
    -- that could affect system stability.
    Critical
  deriving (Eq, Show, Ord, Enum)

instance ToJSON ErrorSeverity where
  toJSON Error = "Error"
  toJSON Critical = "Critical"

-- | Contains the public-facing information about an application error.
--
-- All fields in this record are safe to expose to end users and will be included
-- in JSON serialization. This record is the only part of an error that flows into
-- API responses.
--
-- See 'InternalErrorInfo' for the complementary record holding sensitive diagnostic data.
data PublicErrorInfo = PublicErrorInfo
  { -- | A human-readable message for end users.
    -- This message should be clear, helpful, and safe to display to clients.
    -- It must not contain sensitive information such as database connection details,
    -- internal IP addresses, or stack traces.
    --
    -- Example: @\"Invalid email format\"@
    publicMessage :: Text,
    -- | A machine-readable error code that categorizes the type of error.
    --
    -- Error codes are useful for:
    --
    -- * Logging and monitoring systems
    -- * Creating error statistics and metrics
    -- * Routing errors to appropriate handlers
    -- * Building error catalogs
    --
    -- Example codes: @\"USER_NAME_EMPTY\"@, @\"DB_CONNECTION_FAILED\"@
    code :: Text,
    -- | Optional context details associated with the error.
    --
    -- This field can hold any JSON-serializable data that provides additional
    -- context about the error that is safe to share with the caller. For example:
    --
    -- * Affected resource identifiers
    -- * Custom business logic data
    --
    -- Using 'Value' from @aeson@ allows flexibility: you can store objects,
    -- arrays, strings, or any JSON value.
    --
    -- Example: @Just (object [\"resourceId\" .= (\"usr_123\" :: Text)])@
    details :: Maybe Value
  }
  deriving (Show)

instance ToJSON PublicErrorInfo where
  toJSON pub =
    object $
      catMaybes
        [ Just ("message" .= publicMessage pub),
          Just ("code" .= code pub),
          ("details" .=) <$> details pub
        ]

-- | The body content of an HTTP request, used in 'RequestInfo'.
--
-- Most logging solutions index JSON natively, so 'JsonBody' preserves the
-- structure of JSON payloads for richer querying. Non-JSON payloads are
-- captured as plain text via 'TextBody'.
data RequestContent
  = -- | A structured JSON request body.
    JsonBody Value
  | -- | A non-JSON request body stored as plain text.
    -- Use this for plain text, form-encoded data, or any other content type.
    TextBody Text
  deriving (Show)

instance ToJSON RequestContent where
  toJSON (JsonBody v) = object ["type" .= ("json" :: Text), "body" .= v]
  toJSON (TextBody t) = object ["type" .= ("text" :: Text), "body" .= t]

-- | Structured context about the HTTP request that triggered an error.
--
-- Attach a 'RequestInfo' to 'InternalErrorInfo' to give log aggregators
-- first-class access to request identifiers, headers, and body without
-- having to parse opaque blobs. All fields are optional so you can populate
-- only what is available at the call site.
--
-- Empty header lists are omitted from JSON serialization.
--
-- Example:
--
-- >>> RequestInfo
-- >>>   { requestId      = Just "req_abc123"
-- >>>   , requestHeaders = [("Content-Type", "application/json"), ("X-Request-Id", "req_abc123")]
-- >>>   , requestBody    = Just (JsonBody (object ["name" .= ("Alice" :: Text)]))
-- >>>   }
data RequestInfo = RequestInfo
  { -- | An optional unique identifier for the request.
    --
    -- Useful for correlating errors with a specific request across services
    -- and log entries.
    --
    -- Example: @Just \"req_abc123\"@
    requestId :: Maybe Text,
    -- | HTTP headers as name-value pairs.
    --
    -- Preserves order and repeated headers (e.g. multiple @Set-Cookie@ entries).
    -- Serialized as a JSON array of @{\"name\": ..., \"value\": ...}@ objects.
    -- An empty list is omitted from the JSON output.
    --
    -- Example: @[(\"Content-Type\", \"application\/json\"), (\"Accept\", \"*\/*\")]@
    requestHeaders :: [(Text, Text)],
    -- | The body of the request, if any.
    --
    -- Use 'JsonBody' for JSON payloads so log aggregators can index the fields
    -- directly. Use 'TextBody' for everything else.
    requestBody :: Maybe RequestContent
  }
  deriving (Show)

instance ToJSON RequestInfo where
  toJSON ri =
    object $
      catMaybes
        [ ("requestId" .=) <$> requestId ri,
          case requestHeaders ri of
            [] -> Nothing
            hs -> Just ("headers" .= [object ["name" .= n, "value" .= v] | (n, v) <- hs]),
          ("body" .=) <$> requestBody ri
        ]

-- | Contains internal diagnostic information about an application error.
--
-- This record implements 'ToJSON' so it can be serialized for server-side logging
-- and monitoring. However, 'SomeError'\'s 'ToJSON' instance delegates only to
-- 'PublicErrorInfo', so none of these fields ever appear in public API responses.
--
-- See 'PublicErrorInfo' for the complementary record holding user-facing data.
--
-- == Capturing a call stack
--
-- To record where an error was thrown, add a 'GHC.Stack.HasCallStack' constraint
-- to the function that constructs the error and populate 'callStack':
--
-- >>> import GHC.Stack (HasCallStack, callStack)
-- >>>
-- >>> checkAge :: HasCallStack => Int -> Rail ()
-- >>> checkAge age
-- >>>   | age < 0 = throwError (SomeError AgeTooNegative)
-- >>>   | otherwise = pure ()
-- >>>
-- >>> instance HasErrorInfo AgeError where
-- >>>   publicErrorInfo AgeTooNegative = PublicErrorInfo "Age must be non-negative" "AGE_NEGATIVE" Nothing
-- >>>   internalErrorInfo AgeTooNegative = (internalErrorInfo AgeTooNegative)
-- >>>     { callStack = Just GHC.Stack.callStack }
--
-- Note: if both 'callStack' (from this module) and 'GHC.Stack.callStack' (the
-- implicit-parameter accessor) are in scope, qualify the latter to avoid ambiguity.
data InternalErrorInfo = InternalErrorInfo
  { -- | An optional technical message for administrators and logs.
    -- This message can contain sensitive infrastructure details, stack traces,
    -- database information, and other diagnostic data.
    --
    -- 'Nothing' means the public message is sufficient for diagnostic purposes.
    --
    -- Example: @Just \"Failed to connect to replica database at 192.168.1.5:5432\"@
    internalMessage :: Maybe Text,
    -- | The severity level of the error, indicating how critical it is.
    -- See 'ErrorSeverity' for available levels.
    severity :: ErrorSeverity,
    -- | An optional runtime exception associated with the error, if any.
    --
    -- This field is useful for capturing the underlying exception that caused
    -- the error, such as a database connection timeout or file I\/O error.
    -- It is intended for logging and debugging purposes only.
    exception :: Maybe E.SomeException,
    -- | Optional structured context about the request that triggered the error.
    --
    -- Captures the request identifier, headers, and body for tracing and
    -- diagnostics. It is intended for internal observability only and is never
    -- included in API responses.
    --
    -- See 'RequestInfo' and 'RequestContent' for the available fields.
    --
    -- Example: @Just (RequestInfo { requestId = Just \"req_abc\", requestHeaders = [], requestBody = Nothing })@
    requestInfo :: Maybe RequestInfo,
    -- | The name of the application component or subsystem that produced this error.
    --
    -- Useful for filtering errors by origin in log aggregators without having to
    -- parse the error 'code'. For example: @\"auth\"@, @\"payment\"@, @\"user-service\"@.
    component :: Maybe Text,
    -- | An optional identifier for the user making the request.
    --
    -- Useful for correlating errors with specific users in log aggregators.
    -- Should not contain sensitive PII beyond what is appropriate for server-side logs.
    --
    -- Example: @Just \"usr_abc123\"@
    userId :: Maybe Text,
    -- | The entry point of the request that triggered the error.
    --
    -- Typically the API endpoint or handler that was called. Useful for grouping
    -- errors by origin in observability tooling.
    --
    -- Example: @Just \"POST \/api\/v1\/users\"@
    entrypoint :: Maybe Text,
    -- | The version of the system component running when the error occurred.
    --
    -- Useful for correlating errors with specific releases in log aggregators
    -- and for diagnosing regressions introduced by a particular deployment.
    --
    -- Example: @Just \"1.4.2\"@
    componentVersion :: Maybe Text,
    -- | The Haskell call stack at the point the error was constructed.
    --
    -- Populate this field by adding a 'GHC.Stack.HasCallStack' constraint to the
    -- function that builds the error and passing 'GHC.Stack.callStack'. Serialized
    -- as a human-readable string via 'GHC.Stack.prettyCallStack'.
    --
    -- See the module-level example for the recommended pattern.
    callStack :: Maybe CallStack
  }
  deriving (Show)

instance ToJSON InternalErrorInfo where
  toJSON internal =
    object $
      catMaybes
        [ Just ("severity" .= severity internal),
          ("message" .=) <$> internalMessage internal,
          ("exception" .=) . T.pack . E.displayException <$> exception internal,
          ("requestInfo" .=) <$> requestInfo internal,
          ("component" .=) <$> component internal,
          ("userId" .=) <$> userId internal,
          ("entrypoint" .=) <$> entrypoint internal,
          ("componentVersion" .=) <$> componentVersion internal,
          ("callStack" .=) . T.pack . prettyCallStack <$> callStack internal
        ]

-- | A type class for converting custom error types into 'PublicErrorInfo' and 'InternalErrorInfo'.
--
-- Implement this type class for your custom error types to integrate them with the
-- Railway-Oriented monad. This allows your errors to be automatically converted to
-- a standard format that can be logged, serialized, and combined with other errors.
--
-- The 'internalErrorInfo' method has a default implementation that returns an
-- 'InternalErrorInfo' with all optional fields set to 'Nothing' and severity set to
-- 'Error'. Override it when your error needs to carry diagnostic context.
--
-- == Example
--
-- >>> data UserError = NameEmpty | EmailInvalid
-- >>>
-- >>> instance HasErrorInfo UserError where
-- >>>   publicErrorInfo NameEmpty =
-- >>>     PublicErrorInfo "Name cannot be empty" "USER_NAME_EMPTY" Nothing
-- >>>   publicErrorInfo EmailInvalid =
-- >>>     PublicErrorInfo "Email format is invalid" "USER_EMAIL_INVALID" Nothing
-- >>>
-- >>>   -- Override internalErrorInfo when you have extra diagnostic context:
-- >>>   internalErrorInfo NameEmpty =
-- >>>     (internalErrorInfo NameEmpty)
-- >>>       { internalMessage = Just "name field was empty string after trimming"
-- >>>       , severity = Critical
-- >>>       }
class HasErrorInfo e where
  -- | Converts the error into public-facing 'PublicErrorInfo'.
  --
  -- Implement this method to define the user-visible 'publicMessage', machine-readable
  -- code, and optional public context for your error type.
  publicErrorInfo :: e -> PublicErrorInfo

  -- | Converts the error into internal diagnostic 'InternalErrorInfo'.
  --
  -- The default implementation returns an 'InternalErrorInfo' with all optional
  -- fields set to 'Nothing' and severity set to 'Error'. Override this method
  -- when your error needs to carry sensitive diagnostic context for logging.
  internalErrorInfo :: e -> InternalErrorInfo
  internalErrorInfo _ =
    InternalErrorInfo
      { internalMessage = Nothing,
        severity = Error,
        exception = Nothing,
        requestInfo = Nothing,
        component = Nothing,
        userId = Nothing,
        entrypoint = Nothing,
        componentVersion = Nothing,
        callStack = Nothing
      }

-- | Wrapper for caught exceptions that can be used as an error type.
--
-- This type captures a 'E.SomeException' thrown in 'IO' and makes it
-- compatible with the Railway error system via its 'HasErrorInfo' instance.
-- It is the error type produced by 'Monad.Rail.Types.tryRail' when an IO action throws.
--
-- The 'publicMessage' of the 'PublicErrorInfo' is intentionally generic so that internal
-- details are never accidentally exposed to end users. The original exception is
-- stored in the 'exception' field of 'InternalErrorInfo' for logging and debugging.
--
-- 'caughtCode' lets you assign a domain-specific error code when you catch
-- exceptions manually, rather than relying on the default @\"UNCAUGHT_EXCEPTION\"@:
--
-- >>> import qualified Control.Exception as E
-- >>>
-- >>> safeQuery :: Rail Row
-- >>> safeQuery = do
-- >>>   result <- liftIO $ E.try runQuery
-- >>>   case result of
-- >>>     Right row -> pure row
-- >>>     Left ex   -> throwError (SomeError (CaughtException "DB_QUERY_FAILED" ex Nothing))
--
-- Or use 'Monad.Rail.Types.throwCaughtEx' for a more concise form that also captures the call stack automatically:
--
-- >>> safeQuery :: Rail Row
-- >>> safeQuery = do
-- >>>   result <- liftIO $ E.try runQuery
-- >>>   case result of
-- >>>     Right row -> pure row
-- >>>     Left ex   -> throwCaughtEx "DB_QUERY_FAILED" ex
--
-- When using 'Monad.Rail.Types.tryRail', the code defaults to @\"UNCAUGHT_EXCEPTION\"@ and the
-- 'callStack' is captured automatically at the call site.
data CaughtException = CaughtException
  { -- | Machine-readable error code exposed in 'PublicErrorInfo'.
    -- Defaults to @\"UNCAUGHT_EXCEPTION\"@ when produced by 'Monad.Rail.Types.tryRail'.
    caughtCode :: Text,
    -- | The original exception.
    caughtEx :: E.SomeException,
    -- | Optional Haskell call stack at the catch site.
    -- Populated automatically by 'Monad.Rail.Types.tryRail' via 'GHC.Stack.HasCallStack'.
    caughtCallStack :: Maybe CallStack
  }

instance Show CaughtException where
  show ce = "Caught exception: " <> E.displayException (caughtEx ce)

instance HasErrorInfo CaughtException where
  publicErrorInfo ce =
    PublicErrorInfo
      { publicMessage = "An unexpected error occurred",
        code = caughtCode ce,
        details = Nothing
      }
  internalErrorInfo ce =
    InternalErrorInfo
      { internalMessage = Just (T.pack (E.displayException (caughtEx ce))),
        severity = Critical,
        exception = Just (caughtEx ce),
        requestInfo = Nothing,
        component = Nothing,
        userId = Nothing,
        entrypoint = Nothing,
        componentVersion = Nothing,
        callStack = caughtCallStack ce
      }

-- | A wrapper type that can hold any application error implementing 'HasErrorInfo'.
--
-- This existential type allows you to combine errors of different types in the same
-- Railway computation. It uses existential quantification to hide the concrete error type
-- while preserving the ability to extract error information via the 'HasErrorInfo'
-- interface.
--
-- This is particularly useful when you have multiple error sources (e.g., validation errors,
-- database errors, network errors) and want to combine them in a single computation.
--
-- == JSON serialization
--
-- 'SomeError'\'s 'ToJSON' instance serializes __only__ the 'PublicErrorInfo' fields.
-- 'InternalErrorInfo' is intentionally excluded so that sensitive diagnostic data
-- (internal messages, call stacks, exceptions) is never accidentally exposed in API responses.
-- Use 'internalErrorInfo' directly if you need to serialize that data for server-side logging.
--
-- == Example
--
-- >>> data UserError = NameEmpty
-- >>> data DatabaseError = ConnectionFailed
-- >>>
-- >>> instance HasErrorInfo UserError where { ... }
-- >>> instance HasErrorInfo DatabaseError where { ... }
-- >>>
-- >>> validate :: Rail ()
-- >>> validate = do
-- >>>   throwError (SomeError NameEmpty)      -- User error
-- >>>   throwError (SomeError ConnectionFailed)  -- Database error
data SomeError
  = forall e.
    (HasErrorInfo e, Show e) =>
    SomeError e

instance ToJSON SomeError where
  toJSON (SomeError e) = toJSON (publicErrorInfo e)

instance Show SomeError where
  show (SomeError e) = show e

instance HasErrorInfo SomeError where
  publicErrorInfo (SomeError e) = publicErrorInfo e
  internalErrorInfo (SomeError e) = internalErrorInfo e

-- | Represents a collection of one or more application errors accumulated during a Railway computation.
--
-- This type is used as the error type in 'Monad.Rail.Types.RailT' computations. It guarantees that
-- at least one error is always present (using 'NonEmpty'), which is essential for
-- the Railway-Oriented programming model where a failure state must contain error information.
--
-- Multiple errors are accumulated when using the 'Monad.Rail.Types.<!>' operator for validations,
-- allowing you to collect all validation errors before reporting failure.
--
-- == Combining Errors
--
-- Use the 'Semigroup' instance to combine multiple 'Failure' values:
--
-- >>> let err1 = Failure (SomeError e1 :| [])
-- >>> let err2 = Failure (SomeError e2 :| [])
-- >>> let combined = err1 <> err2  -- Contains both errors
newtype Failure = Failure
  { -- | Extracts the non-empty list of errors.
    --
    -- Use this function to access the individual errors for logging, reporting,
    -- or further processing.
    getErrors :: NonEmpty SomeError
  }
  deriving (Show)

instance Semigroup Failure where
  (Failure e1) <> (Failure e2) = Failure (e1 <> e2)

instance ToJSON Failure where
  toJSON = toJSON . getErrors
