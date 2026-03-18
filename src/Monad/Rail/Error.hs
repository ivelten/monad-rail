{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
-- === Simple errors
--
-- Implement 'HasErrorInfo' with just 'errorPublicMessage'. Derive 'Data.Data.Data' to get an
-- automatic error 'errorCode' derived from the constructor name:
--
-- >>> {-# LANGUAGE DeriveDataTypeable #-}
-- >>>
-- >>> data UserError = NameEmpty | EmailInvalid
-- >>>   deriving (Show, Data)
-- >>>
-- >>> instance HasErrorInfo UserError where
-- >>>   errorPublicMessage NameEmpty    = "Name cannot be empty"
-- >>>   errorPublicMessage EmailInvalid = "Email format is invalid"
--
-- Note: the error code is derived directly from the constructor name, so renaming
-- a constructor silently changes its code. Treat constructor names as part of your
-- public API contract when using this approach.
--
-- === Full control
--
-- Override any individual field method when you need custom behaviour. You can override
-- as many or as few as you need — all non-required methods have sensible defaults:
--
-- >>> instance HasErrorInfo UserError where
-- >>>   errorPublicMessage NameEmpty    = "Name cannot be empty"
-- >>>   errorPublicMessage EmailInvalid = "Email format is invalid"
-- >>>
-- >>>   errorCode NameEmpty    = "UserNameEmpty"
-- >>>   errorCode EmailInvalid = "UserEmailInvalid"
-- >>>
-- >>>   -- Override internal fields only when you have extra diagnostic context:
-- >>>   errorSeverity EmailInvalid = Critical
-- >>>   errorInternalMessage NameEmpty = Just "name field was empty string after trimming"
--
-- === Running your Railway
--
-- 1. Wrap errors in 'SomeError':
--
-- >>> throwError (SomeError NameEmpty)
--
-- 2. Run and handle the result:
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
    publicErrorInfo,
    internalErrorInfo,
    SomeError (..),
    UncaughtException (..),
    CaughtException (..),
    Failure (..),
  )
where

import qualified Control.Exception as E
import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Data (Data, toConstr)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes, fromMaybe)
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
    -- Example codes: @\"UserNameEmpty\"@, @\"DbConnectionFailed\"@
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
    -- parse the error 'errorCode'. For example: @\"auth\"@, @\"payment\"@, @\"user-service\"@.
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
    -- Note: the field name @callStack@ shadows the 'GHC.Stack.callStack' implicit-parameter
    -- accessor when both are in scope. Qualify the latter as @GHC.Stack.callStack@ to avoid
    -- ambiguity.
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

-- | A type class for converting custom error types into serializable error information.
--
-- Implement 'errorPublicMessage' — the only required method — to integrate any error type
-- with the Railway error system. All other methods have defaults and can be overridden
-- individually as needed.
--
-- Use 'publicErrorInfo' and 'internalErrorInfo' to assemble the corresponding records
-- from an instance.
--
-- == Simple errors: implement 'errorPublicMessage' only
--
-- Derive 'Data.Data.Data' and implement 'errorPublicMessage'. The 'errorCode' default derives
-- the error code from the constructor name via 'Data.Data.toConstr':
--
-- >>> {-# LANGUAGE DeriveDataTypeable #-}
-- >>>
-- >>> data UserError = NameEmpty | EmailInvalid
-- >>>   deriving (Show, Data)
-- >>>
-- >>> instance HasErrorInfo UserError where
-- >>>   errorPublicMessage NameEmpty    = "Name cannot be empty"
-- >>>   errorPublicMessage EmailInvalid = "Email format is invalid"
-- >>> -- publicErrorInfo NameEmpty
-- >>> --   = PublicErrorInfo { publicMessage = "Name cannot be empty"
-- >>> --                     , code          = "NameEmpty"
-- >>> --                     , details       = Nothing }
--
-- == Full control: override any field method
--
-- Override individual methods when you need custom codes, details, or internal context.
-- Methods you do not override keep their defaults:
--
-- >>> instance HasErrorInfo UserError where
-- >>>   errorPublicMessage NameEmpty    = "Name cannot be empty"
-- >>>   errorPublicMessage EmailInvalid = "Email format is invalid"
-- >>>
-- >>>   errorCode NameEmpty    = "UserNameEmpty"
-- >>>   errorCode EmailInvalid = "UserEmailInvalid"
-- >>>
-- >>>   errorSeverity _               = Critical
-- >>>   errorInternalMessage NameEmpty = Just "name field was empty string after trimming"
--
-- == Note on 'errorCallStack' and 'GHC.Stack.callStack'
--
-- The assembled 'InternalErrorInfo' record has a field named @callStack@. If both
-- 'InternalErrorInfo' and 'GHC.Stack' are imported unqualified, the name @callStack@
-- may be ambiguous. Qualify 'GHC.Stack.callStack' to avoid ambiguity.
class HasErrorInfo e where
  -- | A human-readable message safe to display to end users. This is the only required method.
  errorPublicMessage :: e -> Text

  -- | A machine-readable error code. Defaults to the constructor name via 'Data.Data.toConstr'.
  --
  -- Override when you need a code that differs from the constructor name.
  --
  -- Example: @errorCode NameEmpty = \"UserNameEmpty\"@
  errorCode :: e -> Text
  default errorCode :: (Data e) => e -> Text
  errorCode e = T.pack (show (toConstr e))

  -- | Optional JSON details safe to share with callers. Defaults to 'Nothing'.
  errorDetails :: e -> Maybe Value
  errorDetails _ = Nothing

  -- | Severity level of the error. Defaults to 'Error'.
  errorSeverity :: e -> ErrorSeverity
  errorSeverity _ = Error

  -- | An optional technical message for logs, safe to contain sensitive details.
  -- Defaults to 'Nothing'.
  errorInternalMessage :: e -> Maybe Text
  errorInternalMessage _ = Nothing

  -- | An optional underlying runtime exception. Defaults to 'Nothing'.
  errorException :: e -> Maybe E.SomeException
  errorException _ = Nothing

  -- | Optional structured HTTP request context for tracing. Defaults to 'Nothing'.
  errorRequestInfo :: e -> Maybe RequestInfo
  errorRequestInfo _ = Nothing

  -- | The application component or subsystem that produced the error. Defaults to 'Nothing'.
  errorComponent :: e -> Maybe Text
  errorComponent _ = Nothing

  -- | An identifier for the user making the request. Defaults to 'Nothing'.
  errorUserId :: e -> Maybe Text
  errorUserId _ = Nothing

  -- | The API endpoint or handler that was called. Defaults to 'Nothing'.
  errorEntrypoint :: e -> Maybe Text
  errorEntrypoint _ = Nothing

  -- | The version of the component running when the error occurred. Defaults to 'Nothing'.
  errorComponentVersion :: e -> Maybe Text
  errorComponentVersion _ = Nothing

  -- | The Haskell call stack at the point the error was constructed. Defaults to 'Nothing'.
  --
  -- Populate by adding 'GHC.Stack.HasCallStack' to the function that builds the error
  -- and passing @Just GHC.Stack.callStack@.
  errorCallStack :: e -> Maybe CallStack
  errorCallStack _ = Nothing

-- | Assembles a 'PublicErrorInfo' from a 'HasErrorInfo' instance.
--
-- This is the canonical way to obtain the public-facing error record for logging
-- or serialization. The result contains only data safe to expose to end users.
publicErrorInfo :: (HasErrorInfo e) => e -> PublicErrorInfo
publicErrorInfo e =
  PublicErrorInfo
    { publicMessage = errorPublicMessage e,
      code = errorCode e,
      details = errorDetails e
    }

-- | Assembles an 'InternalErrorInfo' from a 'HasErrorInfo' instance.
--
-- This is the canonical way to obtain the internal diagnostic record for
-- server-side logging and monitoring. Never include this in API responses.
internalErrorInfo :: (HasErrorInfo e) => e -> InternalErrorInfo
internalErrorInfo e =
  InternalErrorInfo
    { internalMessage = errorInternalMessage e,
      severity = errorSeverity e,
      exception = errorException e,
      requestInfo = errorRequestInfo e,
      component = errorComponent e,
      userId = errorUserId e,
      entrypoint = errorEntrypoint e,
      componentVersion = errorComponentVersion e,
      callStack = errorCallStack e
    }

-- | Marker type for the default error code and message used by
-- 'Monad.Rail.Types.tryRail' when an IO action throws an uncaught exception.
--
-- Its 'HasErrorInfo' instance provides the generic public message shown to end users
-- and the default error code @\"UncaughtException\"@ derived from the constructor name.
-- Both are consumed by 'CaughtException'\'s 'HasErrorInfo' instance so the values
-- stay in one place.
data UncaughtException = UncaughtException
  deriving (Show, Data)

instance HasErrorInfo UncaughtException where
  errorPublicMessage _ = "An unexpected error occurred"

-- | Wrapper for caught exceptions that can be used as an error type.
--
-- This type captures a 'E.SomeException' thrown in 'IO' and makes it
-- compatible with the Railway error system via its 'HasErrorInfo' instance.
-- It is the error type produced by 'Monad.Rail.Types.tryRail' when an IO action throws.
--
-- The 'publicMessage' of the assembled 'PublicErrorInfo' is intentionally generic so
-- that internal details are never accidentally exposed to end users. The original
-- exception is stored in the 'exception' field of 'InternalErrorInfo' for logging
-- and debugging.
--
-- 'caughtCode' lets you assign a domain-specific error code when you catch
-- exceptions manually, rather than relying on the default @\"UncaughtException\"@:
--
-- >>> import qualified Control.Exception as E
-- >>>
-- >>> safeQuery :: Rail Row
-- >>> safeQuery = do
-- >>>   result <- liftIO $ E.try runQuery
-- >>>   case result of
-- >>>     Right row -> pure row
-- >>>     Left ex   -> throwError (SomeError (CaughtException "DbQueryFailed" ex Nothing Nothing))
--
-- Or use 'Monad.Rail.Types.throwCaughtEx' for a more concise form that also captures the call stack automatically:
--
-- >>> safeQuery :: Rail Row
-- >>> safeQuery = do
-- >>>   result <- liftIO $ E.try runQuery
-- >>>   case result of
-- >>>     Right row -> pure row
-- >>>     Left ex   -> throwCaughtEx "DbQueryFailed" ex
--
-- When using 'Monad.Rail.Types.tryRail', the code defaults to @\"UncaughtException\"@ and the
-- call stack is captured automatically at the call site.
data CaughtException = CaughtException
  { -- | Machine-readable error code exposed in 'PublicErrorInfo'.
    -- Defaults to @\"UncaughtException\"@ when produced by 'Monad.Rail.Types.tryRail'.
    caughtCode :: Text,
    -- | The original exception.
    caughtException :: E.SomeException,
    -- | Optional Haskell call stack at the catch site.
    -- Populated automatically by 'Monad.Rail.Types.tryRail' via 'GHC.Stack.HasCallStack'.
    caughtCallStack :: Maybe CallStack,
    -- | Optional public message override.
    --
    -- When 'Nothing', falls back to @'errorPublicMessage' 'UncaughtException'@
    -- (@\"An unexpected error occurred\"@). Set this via
    -- 'Monad.Rail.Types.tryRailWithError' to surface a domain-specific message.
    caughtMessage :: Maybe Text
  }

instance Show CaughtException where
  show ce = "Caught exception: " <> E.displayException (caughtException ce)

instance HasErrorInfo CaughtException where
  errorPublicMessage ce = fromMaybe (errorPublicMessage UncaughtException) (caughtMessage ce)
  errorCode ce = caughtCode ce
  errorSeverity _ = Critical
  errorInternalMessage ce = Just (T.pack (E.displayException (caughtException ce)))
  errorException ce = Just (caughtException ce)
  errorCallStack ce = caughtCallStack ce

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
  errorPublicMessage (SomeError e) = errorPublicMessage e
  errorCode (SomeError e) = errorCode e
  errorDetails (SomeError e) = errorDetails e
  errorSeverity (SomeError e) = errorSeverity e
  errorInternalMessage (SomeError e) = errorInternalMessage e
  errorException (SomeError e) = errorException e
  errorRequestInfo (SomeError e) = errorRequestInfo e
  errorComponent (SomeError e) = errorComponent e
  errorUserId (SomeError e) = errorUserId e
  errorEntrypoint (SomeError e) = errorEntrypoint e
  errorComponentVersion (SomeError e) = errorComponentVersion e
  errorCallStack (SomeError e) = errorCallStack e

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
