{-# LANGUAGE OverloadedStrings #-}

module Monad.Rail.ErrorSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Monad.Rail.Error
import Test.Hspec
import Test.QuickCheck

-- ---------------------------------------------------------------------------
-- Test fixtures
-- ---------------------------------------------------------------------------

data TestError = TestErrorA | TestErrorB
  deriving (Show, Eq)

instance HasErrorInfo TestError where
  errorInfo TestErrorA =
    ErrorInfo
      { publicMessage = "Error A occurred",
        internalMessage = Just "Internal details for A",
        code = "TEST_ERROR_A",
        severity = Error,
        exception = Nothing,
        details = Nothing,
        requestInfo = Nothing
      }
  errorInfo TestErrorB =
    ErrorInfo
      { publicMessage = "Error B occurred",
        internalMessage = Nothing,
        code = "TEST_ERROR_B",
        severity = Critical,
        exception = Nothing,
        details = Just (object ["key" .= ("value" :: Text)]),
        requestInfo = Just (object ["path" .= ("/api/test" :: Text)])
      }

mkAppError :: TestError -> ApplicationError
mkAppError = ApplicationError

mkRailError :: TestError -> RailError
mkRailError e = RailError (mkAppError e NE.:| [])

-- | Build a RailError containing n copies of TestErrorA.
mkRailErrorN :: Int -> RailError
mkRailErrorN n =
  let errs = replicate (max 1 n) (mkAppError TestErrorA)
   in RailError (NE.fromList errs)

-- ---------------------------------------------------------------------------
-- Byte-string helpers (avoid pulling in extra packages)
-- ---------------------------------------------------------------------------

contains :: String -> BSLC.ByteString -> Bool
contains needle haystack = go (BSLC.unpack haystack)
  where
    go [] = null needle
    go s@(_ : rest)
      | needle `isPrefixOf` s = True
      | otherwise = go rest
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

notContains :: String -> BSLC.ByteString -> Bool
notContains needle = not . contains needle

startsWith :: String -> BSLC.ByteString -> Bool
startsWith prefix bs = prefix `isPrefixOf` BSLC.unpack bs
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "ErrorSeverity" $ do
    describe "Eq" $ do
      it "Error equals Error" $
        Error `shouldBe` Error
      it "Critical equals Critical" $
        Critical `shouldBe` Critical
      it "Error does not equal Critical" $
        Error `shouldNotBe` Critical

    describe "Ord" $ do
      it "Error < Critical" $
        Error `shouldSatisfy` (< Critical)
      it "max Error Critical == Critical" $
        max Error Critical `shouldBe` Critical

    describe "Enum" $ do
      it "toEnum 0 == Error" $
        (toEnum 0 :: ErrorSeverity) `shouldBe` Error
      it "toEnum 1 == Critical" $
        (toEnum 1 :: ErrorSeverity) `shouldBe` Critical
      it "fromEnum Error == 0" $
        fromEnum Error `shouldBe` 0
      it "fromEnum Critical == 1" $
        fromEnum Critical `shouldBe` 1

    describe "ToJSON" $ do
      it "serializes Error as JSON string \"Error\"" $
        toJSON Error `shouldBe` String "Error"
      it "serializes Critical as JSON string \"Critical\"" $
        toJSON Critical `shouldBe` String "Critical"

  describe "ErrorInfo" $ do
    let errInfo =
          ErrorInfo
            { publicMessage = "Something went wrong",
              internalMessage = Just "DB connection failed at 10.0.0.1",
              code = "GENERIC_ERROR",
              severity = Error,
              exception = Nothing,
              details = Nothing,
              requestInfo = Just (object ["method" .= ("POST" :: Text)])
            }
    let encoded = encode (toJSON errInfo)

    describe "ToJSON — included fields" $ do
      it "includes 'message' (publicMessage)" $
        encoded `shouldSatisfy` contains "\"message\""
      it "includes 'code'" $
        encoded `shouldSatisfy` contains "\"code\""
      it "includes 'exception'" $
        encoded `shouldSatisfy` contains "\"exception\""
      it "includes 'details'" $
        encoded `shouldSatisfy` contains "\"details\""
      it "uses publicMessage as the 'message' value" $
        encoded `shouldSatisfy` contains "Something went wrong"

    describe "ToJSON — excluded sensitive fields" $ do
      it "does NOT include 'internalMessage'" $
        encoded `shouldSatisfy` notContains "internalMessage"
      it "does NOT include 'severity'" $
        encoded `shouldSatisfy` notContains "severity"
      it "does NOT include 'requestInfo'" $
        encoded `shouldSatisfy` notContains "requestInfo"

  describe "ApplicationError" $ do
    it "Show delegates to the wrapped error's Show instance" $
      show (mkAppError TestErrorA) `shouldBe` "TestErrorA"

    it "errorInfo extracts correct code" $ do
      let info = errorInfo (mkAppError TestErrorA)
      code info `shouldBe` "TEST_ERROR_A"

    it "errorInfo extracts correct publicMessage" $ do
      let info = errorInfo (mkAppError TestErrorA)
      publicMessage info `shouldBe` "Error A occurred"

    it "errorInfo extracts correct severity" $ do
      let info = errorInfo (mkAppError TestErrorA)
      severity info `shouldBe` Error

    it "wraps different error types, each with their own info" $ do
      let infoA = errorInfo (mkAppError TestErrorA)
          infoB = errorInfo (mkAppError TestErrorB)
      code infoA `shouldBe` "TEST_ERROR_A"
      code infoB `shouldBe` "TEST_ERROR_B"

    describe "ToJSON" $ do
      it "serializes via errorInfo" $
        toJSON (mkAppError TestErrorA) `shouldBe` toJSON (errorInfo TestErrorA)

  describe "RailError" $ do
    describe "Semigroup" $ do
      it "combining two single-error RailErrors yields two errors" $ do
        let combined = mkRailError TestErrorA <> mkRailError TestErrorB
        length (getAppErrors combined) `shouldBe` 2

      it "preserves left errors before right errors" $ do
        let combined = mkRailError TestErrorA <> mkRailError TestErrorB
            errList = getAppErrors combined
        show (NE.head errList) `shouldBe` "TestErrorA"
        show (NE.last errList) `shouldBe` "TestErrorB"

      it "satisfies associativity (error count)" $
        property $ \(Positive n1) (Positive n2) (Positive n3) ->
          let ra = mkRailErrorN (n1 `mod` 5 + 1)
              rb = mkRailErrorN (n2 `mod` 5 + 1)
              rc = mkRailErrorN (n3 `mod` 5 + 1)
           in length (getAppErrors ((ra <> rb) <> rc))
                == length (getAppErrors (ra <> (rb <> rc)))

    describe "ToJSON" $ do
      it "serializes as a JSON array" $
        encode (mkRailError TestErrorA) `shouldSatisfy` startsWith "["
