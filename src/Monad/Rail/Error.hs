{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides the core error handling types and type classes for the Railway-Oriented monad.
--
-- The module defines a hierarchy of error types, from specific 'ErrorSeverity' levels to the generic
-- 'ApplicationError' wrapper that can hold any error type implementing 'HasErrorInfo'.
-- These types work together to provide a flexible, type-safe error handling system that supports
-- error accumulation and serialization to JSON.
--
-- == Quick Start
--
-- 1. Define your error type and implement 'HasErrorInfo':
--
-- >>> data UserError = NameEmpty | EmailInvalid
-- >>> instance HasErrorInfo UserError where
-- >>>   errorInfo NameEmpty = ErrorInfo "Name cannot be empty" "USER_NAME_EMPTY" Error Nothing
-- >>>   errorInfo EmailInvalid = ErrorInfo "Email is invalid" "USER_EMAIL_INVALID" Error Nothing
--
-- 2. Wrap it in 'ApplicationError' to use in your Railway:
--
-- >>> throwError (ApplicationError NameEmpty)
--
-- 3. Run your Railway and handle the result:
--
-- >>> result <- runRail myComputation
-- >>> case result of
-- >>>   Right value -> putStrLn "Success!"
-- >>>   Left errors -> print errors  -- Automatically serializes to JSON
module Monad.Rail.Error
  ( ErrorSeverity (..),
    ErrorInfo (..),
    HasErrorInfo (..),
    ApplicationError (..),
    RailError (..),
  )
where

import qualified Control.Exception as E
import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

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

-- | Contains detailed information about an application error.
--
-- This record type holds all the metadata about an error, including both public
-- and internal messages, a machine-readable code, severity level, an optional
-- associated exception, optional context details, and optional request information.
--
-- The public message is safe to expose to end users, while the internal message,
-- severity, and request data are intended only for logging and administrative purposes.
data ErrorInfo = ErrorInfo
  { -- | A human-readable message for end users.
    -- This message should be clear, helpful, and safe to display to clients.
    -- It should not contain sensitive information like database details or
    -- internal IP addresses.
    --
    -- Example: @\"Invalid email format\"@
    publicMessage :: Text,
    -- | An optional technical message for administrators and logs.
    -- This message can contain sensitive infrastructure details, stack traces,
    -- database information, and other diagnostic data.
    --
    -- This field is intentionally excluded from JSON serialization to prevent
    -- accidental exposure of sensitive information in API responses.
    --
    -- Example: @Just \"Failed to connect to replica database at 192.168.1.5:5432\"@
    internalMessage :: Maybe Text,
    -- | A machine-readable error code that categorizes the type of error.
    --
    -- Error codes are useful for:
    -- * Logging and monitoring systems
    -- * Creating error statistics and metrics
    -- * Routing errors to appropriate handlers
    -- * Building error catalogs
    --
    -- Example codes: @\"USER_NAME_EMPTY\"@, @\"DB_CONNECTION_FAILED\"@
    code :: Text,
    -- | The severity level of the error, indicating how critical it is.
    -- See 'ErrorSeverity' for available levels.
    --
    -- This field is intentionally excluded from JSON serialization as it is
    -- primarily used for logging and monitoring purposes.
    severity :: ErrorSeverity,
    -- | An optional runtime exception associated with the error, if any.
    --
    -- This field is useful for capturing the underlying exception that caused
    -- the error, such as a database connection timeout or file I\/O error.
    -- When serialized to JSON, the exception is converted to its string representation.
    exception :: Maybe E.SomeException,
    -- | Optional context details associated with the error.
    --
    -- This field can hold any JSON-serializable data that provides additional
    -- context about the error. For example:
    --
    -- * User ID that triggered the error
    -- * Request ID for tracing
    -- * Affected resource identifiers
    -- * Custom business logic data
    --
    -- Using 'Value' from @aeson@ allows flexibility: you can store objects,
    -- arrays, strings, or any JSON value. This field is exposed in API responses.
    --
    -- Example: @Just (object ["userId" .= (123 :: Int), "attemptCount" .= (5 :: Int)])@
    details :: Maybe Value,
    -- | Optional information about the request that caused this error.
    --
    -- This field is intentionally excluded from JSON serialization to prevent
    -- accidental exposure of request details in API responses. It is intended
    -- for logging, debugging, and administrative purposes only.
    --
    -- Example: @Just (object ["method" .= (\"POST\" :: Text), "path" .= (\"/api/users\" :: Text)])@
    requestInfo :: Maybe Value
  }
  deriving (Show)

instance ToJSON ErrorInfo where
  toJSON err =
    object
      [ "message" .= publicMessage err,
        "code" .= code err,
        "exception" .= fmap show (exception err),
        "details" .= details err
      ]

-- | A type class for converting custom error types into 'ErrorInfo'.
--
-- Implement this type class for your custom error types to integrate them with the
-- Railway-Oriented monad. This allows your errors to be automatically converted to
-- a standard format that can be logged, serialized, and combined with other errors.
--
-- == Example
--
-- >>> data UserError = NameEmpty | EmailInvalid
-- >>>
-- >>> instance HasErrorInfo UserError where
-- >>>   errorInfo NameEmpty =
-- >>>     ErrorInfo "Name cannot be empty" "USER_NAME_EMPTY" Error Nothing
-- >>>   errorInfo EmailInvalid =
-- >>>     ErrorInfo "Email format is invalid" "USER_EMAIL_INVALID" Error Nothing
class HasErrorInfo e where
  -- | Converts the error into detailed 'ErrorInfo'.
  --
  -- Implement this method to define how your custom error type is converted
  -- to the standard error information format.
  errorInfo :: e -> ErrorInfo

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
-- >>>   throwError (ApplicationError NameEmpty)      -- User error
-- >>>   throwError (ApplicationError ConnectionFailed)  -- Database error
data ApplicationError
  = forall e.
    (HasErrorInfo e, Show e) =>
    ApplicationError e

instance ToJSON ApplicationError where
  toJSON (ApplicationError e) = toJSON (errorInfo e)

instance Show ApplicationError where
  show (ApplicationError e) = show e

instance HasErrorInfo ApplicationError where
  errorInfo (ApplicationError e) = errorInfo e

-- | Represents a collection of one or more application errors.
--
-- This type is used as the error type in 'RailT' computations. It guarantees that
-- at least one error is always present (using 'NonEmpty'), which is essential for
-- the Railway-Oriented programming model where a failure state must contain error information.
--
-- Multiple errors are accumulated when using the '<!>' operator for validations,
-- allowing you to collect all validation errors before reporting failure.
--
-- == Combining Errors
--
-- Use the 'Semigroup' instance to combine multiple 'RailError' values:
--
-- >>> let err1 = RailError (ApplicationError e1 :| [])
-- >>> let err2 = RailError (ApplicationError e2 :| [])
-- >>> let combined = err1 <> err2  -- Contains both errors
newtype RailError = RailError
  { -- | Extracts the non-empty list of application errors.
    --
    -- Use this function to access the individual errors for logging, reporting,
    -- or further processing.
    getAppErrors :: NonEmpty ApplicationError
  }
  deriving (Show)

instance Semigroup RailError where
  (RailError e1) <> (RailError e2) = RailError (e1 <> e2)

instance ToJSON RailError where
  toJSON = toJSON . getAppErrors
