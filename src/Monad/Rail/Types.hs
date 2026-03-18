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
-- * 'Failure' - The error type containing one or more 'SomeError' values
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
-- Use 'throwError' to throw a 'SomeError':
--
-- >>> checkName :: Rail ()
-- >>> checkName = do
-- >>>   when (T.null name) $ throwError (SomeError NameEmpty)
-- >>>   pure ()
--
-- == Combining Different Error Types
--
-- The 'SomeError' wrapper allows you to combine errors from different sources:
--
-- >>> data UserError = NameEmpty
-- >>> data DatabaseError = ConnectionFailed
-- >>>
-- >>> instance HasErrorInfo UserError where { ... }
-- >>> instance HasErrorInfo DatabaseError where { ... }
-- >>>
-- >>> myRailway :: Rail ()
-- >>> myRailway = do
-- >>>   throwError (SomeError NameEmpty)
-- >>>   throwError (SomeError ConnectionFailed)
module Monad.Rail.Types
  ( RailT (..),
    Rail,
    runRailT,
    runRail,
    throwError,
    throwCaughtEx,
    tryRail,
    tryRailWithCode,
    tryRailWithError,
    (<!>),
  )
where

import qualified Control.Exception as Ex
import Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import GHC.Stack (HasCallStack, callStack)
import Monad.Rail.Error (CaughtException (..), Failure (..), HasErrorInfo (..), SomeError (..), UncaughtException (..))

-- | The Railway-Oriented monad transformer.
--
-- 'RailT' wraps 'ExceptT' to provide composable error handling with support for
-- error accumulation. It is parameterized over:
--
-- * @e@ - The error type (typically 'Failure')
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
-- >>> computation :: RailT Failure IO String
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
-- This is the most common way to use 'RailT'. It fixes the error type to 'Failure'
-- and the base monad to 'IO', providing a simple interface for building IO-based
-- applications with Railway-Oriented error handling.
--
-- @Rail a@ is equivalent to @RailT Failure IO a@
--
-- == Example
--
-- >>> myApp :: Rail ()
-- >>> myApp = do
-- >>>   validateInput
-- >>>   processData
-- >>>   liftIO $ putStrLn "Complete!"
type Rail a = RailT Failure IO a

-- | Runs a 'RailT' computation in any base monad @m@, returning @m (Either e a)@.
--
-- This is the general form of 'runRail'. Use it when your base monad is not 'IO' —
-- for example, when 'RailT' is stacked on top of 'StateT', 'ReaderT', or any other
-- transformer.
--
-- == Example: custom monad stack
--
-- >>> import Control.Monad.State (StateT, runStateT)
-- >>>
-- >>> type AppRail a = RailT Failure (StateT AppState IO) a
-- >>>
-- >>> runAppRail :: AppState -> AppRail a -> IO (Either Failure a, AppState)
-- >>> runAppRail initialState = runStateT . runRailT
runRailT :: (Monad m) => RailT e m a -> m (Either e a)
runRailT = runExceptT . unRailT

-- | Runs a 'Rail' computation and returns the result or accumulated errors.
--
-- This function executes the railway computation and returns the final result wrapped
-- in 'Either'. On success, you get @Right value@. On failure, you get @Left errors@
-- containing all accumulated 'SomeError' values.
--
-- The returned 'Failure' contains at least one error (guaranteed by 'NonEmpty'),
-- making it safe to handle errors without null checks.
--
-- == Example
--
-- >>> result <- runRail myComputation
-- >>> case result of
-- >>>   Right value -> putStrLn $ "Success: " ++ show value
-- >>>   Left errors -> do
-- >>>     putStrLn "Errors occurred:"
-- >>>     mapM_ print (getErrors errors)
--
-- == JSON Serialization
--
-- The 'Failure' type implements 'ToJSON', so you can easily serialize errors:
--
-- >>> import Data.Aeson (encode)
-- >>> result <- runRail myComputation
-- >>> case result of
-- >>>   Left errors -> putStrLn $ BS.unpack $ encode errors
-- >>>   Right _ -> pure ()
runRail :: Rail a -> IO (Either Failure a)
runRail = runExceptT . unRailT

-- | Throws an application error in the Railway.
--
-- This function wraps a single 'SomeError' in a 'Failure' container,
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
-- >>>   when (age < 0) $ throwError (SomeError AgeNegative)
-- >>>   pure ()
--
-- == Error Accumulation
--
-- When multiple errors occur (e.g., with '<!>'), 'Failure' will contain all of them.
-- You can then handle them together or individually:
--
-- >>> result <- runRail (checkName <!> checkEmail)
-- >>> case result of
-- >>>   Left errors -> mapM_ print (getErrors errors)
-- >>>   Right () -> putStrLn "All valid!"
throwError :: (Monad m) => SomeError -> RailT Failure m a
throwError err = RailT $ E.throwError $ Failure (err :| [])

-- | Throw a caught IO exception as a Railway error with a domain-specific code.
--
-- This is a convenience wrapper around 'throwError' for the common pattern of
-- catching an IO exception and re-throwing it as a 'CaughtException'. It
-- captures the call stack automatically, so you do not need to pass it manually.
--
-- The call stack is captured at the __call site__ of 'throwCaughtEx', not at the
-- definition of any wrapper around it (provided the wrapper also carries
-- 'GHC.Stack.HasCallStack').
--
-- == Example
--
-- >>> import qualified Control.Exception as E
-- >>>
-- >>> safeQuery :: Rail Row
-- >>> safeQuery = do
-- >>>   result <- liftIO $ E.try runQuery
-- >>>   case result of
-- >>>     Right row -> pure row
-- >>>     Left ex   -> throwCaughtEx "DbQueryFailed" ex
throwCaughtEx :: (HasCallStack, Monad m) => T.Text -> Ex.SomeException -> RailT Failure m a
throwCaughtEx errCode ex = throwError (SomeError caught)
  where
    caught =
      CaughtException
        { caughtCode = errCode,
          caughtException = ex,
          caughtCallStack = Just callStack,
          caughtMessage = Nothing
        }

-- | Safely execute an IO action that may throw exceptions,
-- converting any exception into a Railway error.
--
-- This is a convenience wrapper around 'tryRailWithCode' that uses the default
-- error code @\"UncaughtException\"@ (derived from 'UncaughtException' via
-- 'HasErrorInfo'). Use 'tryRailWithCode' for a custom code with the same generic
-- message, or 'tryRailWithError' to also customise the public message.
--
-- == Example
--
-- >>> readConfig :: FilePath -> Rail String
-- >>> readConfig path = tryRail (readFile path)
-- >>>
-- >>> pipeline :: FilePath -> Rail ()
-- >>> pipeline filePath = do
-- >>>   content <- tryRail (readFile filePath)
-- >>>   validateName content <!> validateEmail content
-- >>>   saveToDb content
tryRail :: (HasCallStack) => IO a -> Rail a
tryRail = tryRailWithCode (const (errorCode UncaughtException))

-- | Like 'tryRail', but with a custom error code derived from the exception.
--
-- Wraps an IO action that may throw, converting any exception into a Railway
-- error whose code is produced by applying the given function to the caught
-- exception. The public message remains the generic default
-- (@\"An unexpected error occurred\"@). Use 'tryRailWithError' when you also
-- need a domain-specific public message.
--
-- Pass a constant function (@'const' \"MyCode\"@) when the code is fixed, or
-- inspect the exception to return different codes:
--
-- >>> tryDb :: HasCallStack => IO a -> Rail a
-- >>> tryDb = tryRailWithCode (const "DbError")
-- >>>
-- >>> tryHttp :: HasCallStack => IO a -> Rail a
-- >>> tryHttp = tryRailWithCode $ \ex ->
-- >>>   if "timeout" `T.isInfixOf` T.pack (Ex.displayException ex)
-- >>>     then "HttpTimeout"
-- >>>     else "HttpError"
--
-- Note: if you partially apply this function, add 'GHC.Stack.HasCallStack' to the
-- wrapper's own signature so the call stack is captured at each call site
-- rather than frozen at the definition of the wrapper.
tryRailWithCode :: (HasCallStack) => (Ex.SomeException -> T.Text) -> IO a -> Rail a
tryRailWithCode mkCode action = do
  result <- liftIO $ Ex.try action
  case result of
    Right value -> pure value
    Left ex -> throwError (SomeError caught)
      where
        caught =
          CaughtException
            { caughtCode = mkCode ex,
              caughtException = ex,
              caughtCallStack = Just callStack,
              caughtMessage = Nothing
            }

-- | Like 'tryRailWithCode', but derives the error code and public message from
-- a 'HasErrorInfo' value built from the caught exception.
--
-- The error-building function receives the 'Ex.SomeException' that was thrown,
-- allowing the resulting error value to carry information extracted from the
-- exception itself. The 'HasErrorInfo' instance then supplies 'errorCode' as the
-- error code and 'errorPublicMessage' as the public message.
--
-- == Example
--
-- >>> {-# LANGUAGE DeriveDataTypeable #-}
-- >>>
-- >>> data DbError = QueryFailed Text | ConnectionLost
-- >>>   deriving (Show, Data)
-- >>>
-- >>> instance HasErrorInfo DbError where
-- >>>   errorPublicMessage (QueryFailed _) = "A database query failed"
-- >>>   errorPublicMessage ConnectionLost  = "Lost connection to the database"
-- >>>
-- >>> safeQuery :: Rail [Row]
-- >>> safeQuery = tryRailWithError (\_ -> ConnectionLost) runQuery
-- >>>
-- >>> -- Inspect the exception to choose the right constructor:
-- >>> safeQuery' :: Rail [Row]
-- >>> safeQuery' = tryRailWithError (QueryFailed . T.pack . Ex.displayException) runQuery
--
-- Note: add 'GHC.Stack.HasCallStack' to any wrapper so the call stack is
-- captured at each call site rather than frozen at the wrapper's definition.
tryRailWithError :: (HasCallStack, HasErrorInfo e) => (Ex.SomeException -> e) -> IO a -> Rail a
tryRailWithError mkErr action = do
  result <- liftIO $ Ex.try action
  case result of
    Right value -> pure value
    Left ex -> throwError (SomeError caught)
      where
        err = mkErr ex
        caught =
          CaughtException
            { caughtCode = errorCode err,
              caughtException = ex,
              caughtCallStack = Just callStack,
              caughtMessage = Just (errorPublicMessage err)
            }

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
(<!>) :: (Monad m) => RailT Failure m () -> RailT Failure m () -> RailT Failure m ()
v1 <!> v2 = RailT $ ExceptT $ do
  r1 <- runExceptT (unRailT v1)
  r2 <- runExceptT (unRailT v2)
  case (r1, r2) of
    (Right (), Right ()) -> pure (Right ())
    (Left e1, Right ()) -> pure (Left e1)
    (Right (), Left e2) -> pure (Left e2)
    (Left e1, Left e2) -> pure (Left (e1 <> e2))

infixl 5 <!>
