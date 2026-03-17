{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module provides the core monad transformer for Railway-Oriented Programming.
--
-- 'RailT' is a monad transformer that wraps 'ExceptT' and provides a convenient interface
-- for building reliable applications with explicit error handling. It allows you to:
--
-- * Chain operations that can fail, with automatic short-circuit on the first error
-- * Accumulate multiple errors using the '<!>' operator
-- * Lift IO actions into the Railway context
-- * Serialize errors to JSON for logging and monitoring
--
-- == Core Types
--
-- * 'RailT' - The monad transformer
-- * 'Rail' - A convenience alias for 'IO' based computations
-- * 'RailError' - The error type containing one or more 'ApplicationError' values
--
-- == Basic Usage
--
-- The simplest way to use the Railway monad is through the 'Rail' type alias:
--
-- >>> import Monad.Rail
-- >>>
-- >>> myComputation :: Rail ()
-- >>> myComputation = do
-- >>>   liftIO $ putStrLn "Starting..."
-- >>>   pure ()
-- >>>
-- >>> main :: IO ()
-- >>> main = do
-- >>>   result <- runRail myComputation
-- >>>   case result of
-- >>>     Right () -> putStrLn "Success!"
-- >>>     Left errors -> print errors
--
-- == Error Handling with Validation
--
-- Use the '<!>' operator to accumulate validation errors:
--
-- >>> validate :: Rail ()
-- >>> validate = do
-- >>>   checkName <!> checkEmail <!> checkAge
-- >>>   saveToDatabase
-- >>>   liftIO $ putStrLn "All validations passed!"
--
-- If any validation fails, all errors are collected and the computation stops
-- before reaching 'saveToDatabase'.
--
-- == Throwing Errors
--
-- Use 'throwError' to throw an 'ApplicationError':
--
-- >>> checkName :: Rail ()
-- >>> checkName = do
-- >>>   when (T.null name) $ throwError (ApplicationError NameEmpty)
-- >>>   pure ()
--
-- == Combining Different Error Types
--
-- The 'ApplicationError' wrapper allows you to combine errors from different sources:
--
-- >>> data UserError = NameEmpty
-- >>> data DatabaseError = ConnectionFailed
-- >>>
-- >>> instance HasErrorInfo UserError where { ... }
-- >>> instance HasErrorInfo DatabaseError where { ... }
-- >>>
-- >>> myRailway :: Rail ()
-- >>> myRailway = do
-- >>>   throwError (ApplicationError NameEmpty)
-- >>>   throwError (ApplicationError ConnectionFailed)
module Monad.Rail.Types
  ( RailT (..),
    Rail,
    runRail,
    throwError,
    (<!>),
  )
where

import Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty (NonEmpty (..))
import Monad.Rail.Error (ApplicationError, RailError (..))

-- | The Railway-Oriented monad transformer.
--
-- 'RailT' wraps 'ExceptT' to provide composable error handling with support for
-- error accumulation. It is parameterized over:
--
-- * @e@ - The error type (typically 'RailError')
-- * @m@ - The underlying monad (often 'IO')
-- * @a@ - The type of successful values
--
-- The transformer implements 'Functor', 'Applicative', 'Monad', and 'MonadIO',
-- allowing it to be used seamlessly with do-notation and other monadic operations.
--
-- == Examples
--
-- Create a computation that can fail:
--
-- >>> computation :: RailT RailError IO String
-- >>> computation = do
-- >>>   liftIO $ putStrLn "Starting..."
-- >>>   pure "result"
--
-- Use with 'Rail' for a more concise type signature:
--
-- >>> computation :: Rail String
-- >>> computation = do
-- >>>   liftIO $ putStrLn "Starting..."
-- >>>   pure "result"
newtype RailT e m a = RailT
  { -- | Extracts the underlying 'ExceptT' computation.
    unRailT :: ExceptT e m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | A convenient type alias for Railway computations in the 'IO' monad.
--
-- This is the most common way to use 'RailT'. It fixes the error type to 'RailError'
-- and the base monad to 'IO', providing a simple interface for building IO-based
-- applications with Railway-Oriented error handling.
--
-- @Rail a@ is equivalent to @RailT RailError IO a@
--
-- == Example
--
-- >>> myApp :: Rail ()
-- >>> myApp = do
-- >>>   validateInput
-- >>>   processData
-- >>>   liftIO $ putStrLn "Complete!"
type Rail a = RailT RailError IO a

-- | Runs a 'Rail' computation and returns the result or accumulated errors.
--
-- This function executes the railway computation and returns the final result wrapped
-- in 'Either'. On success, you get @Right value@. On failure, you get @Left errors@
-- containing all accumulated 'ApplicationError' values.
--
-- The returned 'RailError' contains at least one error (guaranteed by 'NonEmpty'),
-- making it safe to handle errors without null checks.
--
-- == Example
--
-- >>> result <- runRail myComputation
-- >>> case result of
-- >>>   Right value -> putStrLn $ "Success: " ++ show value
-- >>>   Left errors -> do
-- >>>     putStrLn "Errors occurred:"
-- >>>     mapM_ print (getAppErrors errors)
--
-- == JSON Serialization
--
-- The 'RailError' type implements 'ToJSON', so you can easily serialize errors:
--
-- >>> import Data.Aeson (encode)
-- >>> result <- runRail myComputation
-- >>> case result of
-- >>>   Left errors -> putStrLn $ BS.unpack $ encode errors
-- >>>   Right _ -> pure ()
runRail :: Rail a -> IO (Either RailError a)
runRail = runExceptT . unRailT

-- | Throws an application error in the Railway.
--
-- This function wraps a single 'ApplicationError' in a 'RailError' container,
-- immediately failing the computation with that error. Subsequent operations in the
-- do-block will not be executed.
--
-- Use this function when you encounter an error condition that should stop execution.
-- For validations where you want to collect multiple errors before failing, use the
-- '<!>' operator instead.
--
-- == Example
--
-- >>> checkAge :: Int -> Rail ()
-- >>> checkAge age = do
-- >>>   when (age < 0) $ throwError (ApplicationError AgeNegative)
-- >>>   pure ()
--
-- == Error Accumulation
--
-- When multiple errors occur (e.g., with '<!>'), 'RailError' will contain all of them.
-- You can then handle them together or individually:
--
-- >>> result <- runRail (checkName <!> checkEmail)
-- >>> case result of
-- >>>   Left errors -> mapM_ print (getAppErrors errors)
-- >>>   Right () -> putStrLn "All valid!"
throwError :: (Monad m) => ApplicationError -> RailT RailError m a
throwError err = RailT $ E.throwError $ RailError (err :| [])

-- | Accumulates errors from two Railway validations.
--
-- This operator runs both validations regardless of whether the first one fails,
-- collecting all errors before failing. This is the key operator for implementing
-- Railway-Oriented Programming in Haskell.
--
-- The operator is left-associative ('infixl') with precedence 5, meaning:
--
-- > v1 <!> v2 <!> v3
--
-- is interpreted as:
--
-- > (v1 <!> v2) <!> v3
--
-- == Example: Multiple Validations
--
-- Collect all validation errors at once:
--
-- >>> validateUser :: Rail ()
-- >>> validateUser = do
-- >>>   validateName <!> validateEmail <!> validateAge
-- >>>   saveToDatabase
-- >>>   liftIO $ putStrLn "User created successfully!"
--
-- If all validations pass, execution continues to 'saveToDatabase'.
-- If any validation fails, all errors are accumulated and the computation stops.
--
-- == Behavior
--
-- The behavior depends on the results of both validations:
--
-- * Both succeed (Right, Right) → Continue execution
-- * First fails, second succeeds (Left, Right) → Stop with first error
-- * First succeeds, second fails (Right, Left) → Stop with second error
-- * Both fail (Left, Left) → Stop with combined errors (using '<>')
--
-- This allows Railway-Oriented Programming where you can express both paths
-- (success and failure) explicitly in your code.
--
-- == Use Cases
--
-- The '<!>' operator is ideal for:
--
-- * Form validation (check multiple fields, report all errors)
-- * Configuration validation (validate all settings, report all problems)
-- * Data pipeline validation (check all constraints, fail with all violations)
-- * Any scenario where you want "fail-fast" with "report-all-errors"
(<!>) :: (Monad m) => RailT RailError m () -> RailT RailError m () -> RailT RailError m ()
v1 <!> v2 = RailT $ ExceptT $ do
  r1 <- runExceptT (unRailT v1)
  r2 <- runExceptT (unRailT v2)
  case (r1, r2) of
    (Right (), Right ()) -> pure (Right ())
    (Left e1, Right ()) -> pure (Left e1)
    (Right (), Left e2) -> pure (Left e2)
    (Left e1, Left e2) -> pure (Left (e1 <> e2))

infixl 5 <!>
