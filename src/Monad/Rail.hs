{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Railway-Oriented error handling for Haskell applications.
--
-- Monad.Rail is a Haskell library implementing Railway-Oriented Programming (ROP),
-- a functional approach to error handling that makes both success and failure paths
-- explicit and composable.
--
-- == What is Railway-Oriented Programming?
--
-- Railway-Oriented Programming uses a railway analogy: your program has two tracks,
-- one for success and one for failure. Operations can move between tracks, and once
-- on the failure track, you stay there until the end.
--
-- This library implements ROP through:
--
-- * 'RailT' - A monad transformer for railway computations
-- * 'throwError' - Moving to the failure track
-- * '(<!>)' - Combining validations while collecting all errors
-- * Error accumulation - Multiple errors are gathered, not just the first one
--
-- == Quick Start
--
-- Implement 'HasErrorInfo' with 'errorPublicMessage' — the only required method.
-- Derive 'Data.Data.Data' to get an automatic error code from the constructor name:
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
-- Override individual methods when you need custom codes or 'errorDetails':
--
-- >>> instance HasErrorInfo UserError where
-- >>>   errorPublicMessage NameEmpty    = "Name cannot be empty"
-- >>>   errorPublicMessage EmailInvalid = "Email format is invalid"
-- >>>
-- >>>   errorCode NameEmpty    = "UserNameEmpty"
-- >>>   errorCode EmailInvalid = "UserEmailInvalid"
--
-- Use in your railway:
--
-- >>> validateUser :: Rail ()
-- >>> validateUser = do
-- >>>   checkName <!> checkEmail
-- >>>   saveToDatabase
-- >>>   liftIO $ putStrLn "User created!"
--
-- Run and handle the result:
--
-- >>> main :: IO ()
-- >>> main = do
-- >>>   result <- runRail validateUser
-- >>>   case result of
-- >>>     Right () -> putStrLn "Success!"
-- >>>     Left errors -> print errors
--
-- == Error Accumulation
--
-- The '<!>' operator is key to ROP. It runs both validations regardless of failure:
--
-- >>> validate :: Rail ()
-- >>> validate = do
-- >>>   checkName <!> checkEmail <!> checkAge
-- >>>   -- If any checks fail, ALL errors are reported together
--
-- == Core Concepts
--
-- * 'Rail' - The monad for your computations
-- * 'Failure' - Contains accumulated errors
-- * 'SomeError' - Wrapper for any error type
-- * 'HasErrorInfo' - Typeclass for custom errors
-- * '<!>' - Error accumulation operator
--
-- == Logging and Monitoring
--
-- Each error carries two separate records:
--
-- * 'PublicErrorInfo' - Safe for end users: message, code, and details.
--   Assembled via 'publicErrorInfo'; serialized to JSON in API responses.
--   Null fields are omitted.
-- * 'InternalErrorInfo' - Sensitive diagnostics: severity, internal message,
--   exception, and call stack. Assembled via 'internalErrorInfo'; implements
--   'ToJSON' for structured log output but is never included in public API
--   responses. Null fields are omitted.
--
-- The 'Failure' type implements 'ToJSON', so errors serialize automatically:
--
-- >>> import Data.Aeson (encode)
-- >>> result <- runRail myRail
-- >>> case result of
-- >>>   Left errors -> BS.putStrLn $ encode errors
-- >>>   Right _ -> pure ()
module Monad.Rail
  ( -- * Core Types
    RailT (..),
    Rail,
    Failure (..),

    -- * Running Railways
    runRailT,
    runRail,

    -- * Throwing Errors
    throwError,
    throwUnhandledException,
    throwUnhandledExceptionWithCode,

    -- * Exception handling
    tryRail,
    tryRailWithCode,
    tryRailWithError,

    -- * Operators
    (<!>),

    -- * Error types
    ErrorSeverity (..),
    ErrorDetails (..),
    PublicErrorInfo (..),
    InternalErrorInfo (..),
    HasErrorInfo (..),
    publicErrorInfo,
    internalErrorInfo,
    SomeError (..),
    UnhandledException (..),
  )
where

import Monad.Rail.Error
import Monad.Rail.Types
