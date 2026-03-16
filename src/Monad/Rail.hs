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
-- Define your error type:
--
-- >>> data UserError = NameEmpty | EmailInvalid
-- >>>
-- >>> instance IsApplicationError UserError where
-- >>>   errorInfo NameEmpty = ApplicationErrorInfo
-- >>>     { publicMessage = "Name cannot be empty"
-- >>>     , internalMessage = Nothing
-- >>>     , code = "USER_NAME_EMPTY"
-- >>>     , severity = Error
-- >>>     , exception = Nothing
-- >>>     , details = Nothing
-- >>>     , requestInfo = Nothing
-- >>>     }
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
-- * 'RailError' - Contains accumulated errors
-- * 'ApplicationError' - Wrapper for any error type
-- * 'IsApplicationError' - Typeclass for custom errors
-- * '<!>' - Error accumulation operator
--
-- == Logging and Monitoring
--
-- Each error contains:
--
-- * 'publicMessage' - Safe for end users
-- * 'internalMessage' - Sensitive details for logs (not exposed in JSON)
-- * 'code' - Machine-readable error code
-- * 'details' - Additional context (exposed in JSON)
-- * 'requestInfo' - Request data for debugging (not exposed in JSON)
--
-- The 'RailError' type implements 'ToJSON', so errors serialize automatically:
--
-- >>> import Data.Aeson (encode)
-- >>> result <- runRail myRail
-- >>> case result of
-- >>>   Left errors -> BS.putStrLn $ encode errors
-- >>>   Right _ -> pure ()
module Monad.Rail
  ( -- * Core Types
    RailT,
    Rail,
    RailError (..),

    -- * Running Railways
    runRail,

    -- * Throwing Errors
    throwError,

    -- * Operators
    (<!>),

    -- * Re-exports from Error module

    -- | Error handling types and typeclasses
    ErrorSeverity (..),
    ApplicationErrorInfo (..),
    IsApplicationError (..),
    ApplicationError (..),
  )
where

import Monad.Rail.Error
import Monad.Rail.Types
