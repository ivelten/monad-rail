{-# LANGUAGE OverloadedStrings #-}

module Monad.Rail.ErrorSpec (spec) where

import qualified Control.Exception as Ex
import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List (isInfixOf)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified GHC.Stack as GHC
import Monad.Rail.Error
import Test.Hspec
import Test.QuickCheck hiding (Failure)

-- ---------------------------------------------------------------------------
-- Test fixtures
-- ---------------------------------------------------------------------------

data TestError = TestErrorA | TestErrorB
  deriving (Show, Eq)

instance HasErrorInfo TestError where
  publicErrorInfo TestErrorA =
    PublicErrorInfo
      { publicMessage = "Error A occurred",
        code = "TEST_ERROR_A",
        details = Nothing
      }
  publicErrorInfo TestErrorB =
    PublicErrorInfo
      { publicMessage = "Error B occurred",
        code = "TEST_ERROR_B",
        details = Just (object ["key" .= ("value" :: Text)])
      }
  internalErrorInfo TestErrorA =
    InternalErrorInfo
      { internalMessage = Just "Internal details for A",
        severity = Error,
        exception = Nothing,
        requestInfo = Nothing,
        component = Nothing,
        userId = Nothing,
        entrypoint = Nothing,
        componentVersion = Nothing,
        callStack = Nothing
      }
  internalErrorInfo TestErrorB =
    InternalErrorInfo
      { internalMessage = Nothing,
        severity = Critical,
        exception = Nothing,
        requestInfo = Nothing,
        component = Nothing,
        userId = Nothing,
        entrypoint = Nothing,
        componentVersion = Nothing,
        callStack = Nothing
      }

mkSomeError :: TestError -> SomeError
mkSomeError = SomeError

mkFailure :: TestError -> Failure
mkFailure e = Failure (mkSomeError e NE.:| [])

-- | Build a Failure containing n copies of TestErrorA.
mkFailureN :: Int -> Failure
mkFailureN n =
  let errs = replicate (max 1 n) (mkSomeError TestErrorA)
   in Failure (NE.fromList errs)

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

  describe "PublicErrorInfo" $ do
    let pub =
          PublicErrorInfo
            { publicMessage = "Something went wrong",
              code = "GENERIC_ERROR",
              details = Nothing
            }
    let encoded = encode (toJSON pub)

    describe "ToJSON — included fields" $ do
      it "includes 'message'" $
        encoded `shouldSatisfy` contains "\"message\""
      it "includes 'code'" $
        encoded `shouldSatisfy` contains "\"code\""
      it "uses the message value" $
        encoded `shouldSatisfy` contains "Something went wrong"

    describe "ToJSON — null fields are omitted" $ do
      it "omits 'details' when Nothing" $
        encoded `shouldSatisfy` notContains "details"

    describe "ToJSON — non-null optional fields are included" $ do
      it "includes 'details' when Just" $ do
        let pubWithDetails = pub {details = Just (object ["resourceId" .= ("usr_1" :: Text)])}
        encode (toJSON pubWithDetails) `shouldSatisfy` contains "\"details\""

    describe "ToJSON — sensitive fields are absent" $ do
      it "does NOT include 'severity'" $
        encoded `shouldSatisfy` notContains "severity"
      it "does NOT include 'exception'" $
        encoded `shouldSatisfy` notContains "exception"
      it "does NOT include 'requestInfo'" $
        encoded `shouldSatisfy` notContains "requestInfo"
      it "does NOT include 'component'" $
        encoded `shouldSatisfy` notContains "component"
      it "does NOT include 'userId'" $
        encoded `shouldSatisfy` notContains "userId"
      it "does NOT include 'entrypoint'" $
        encoded `shouldSatisfy` notContains "entrypoint"
      it "does NOT include 'componentVersion'" $
        encoded `shouldSatisfy` notContains "componentVersion"
      it "does NOT include 'callStack'" $
        encoded `shouldSatisfy` notContains "callStack"

  describe "InternalErrorInfo" $ do
    let base =
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

    describe "ToJSON — severity is always present" $ do
      it "includes 'severity' even when all optional fields are Nothing" $
        encode (toJSON base) `shouldSatisfy` contains "\"severity\""

    describe "ToJSON — null fields are omitted" $ do
      it "omits 'message' when Nothing" $
        encode (toJSON base) `shouldSatisfy` notContains "message"
      it "omits 'exception' when Nothing" $
        encode (toJSON base) `shouldSatisfy` notContains "exception"
      it "omits 'requestInfo' when Nothing" $
        encode (toJSON base) `shouldSatisfy` notContains "requestInfo"
      it "omits 'component' when Nothing" $
        encode (toJSON base) `shouldSatisfy` notContains "component"
      it "omits 'userId' when Nothing" $
        encode (toJSON base) `shouldSatisfy` notContains "userId"
      it "omits 'entrypoint' when Nothing" $
        encode (toJSON base) `shouldSatisfy` notContains "entrypoint"
      it "omits 'componentVersion' when Nothing" $
        encode (toJSON base) `shouldSatisfy` notContains "componentVersion"
      it "omits 'callStack' when Nothing" $
        encode (toJSON base) `shouldSatisfy` notContains "callStack"

    describe "ToJSON — non-null optional fields are included" $ do
      it "includes 'message' when Just" $
        encode (toJSON base {internalMessage = Just "debug info"})
          `shouldSatisfy` contains "\"message\""
      it "includes 'component' when Just" $
        encode (toJSON base {component = Just "auth"})
          `shouldSatisfy` contains "\"component\""
      it "includes the component value" $
        encode (toJSON base {component = Just "auth"})
          `shouldSatisfy` contains "auth"
      it "includes 'userId' when Just" $
        encode (toJSON base {userId = Just "usr_abc123"})
          `shouldSatisfy` contains "\"userId\""
      it "includes the userId value" $
        encode (toJSON base {userId = Just "usr_abc123"})
          `shouldSatisfy` contains "usr_abc123"
      it "includes 'entrypoint' when Just" $
        encode (toJSON base {entrypoint = Just "POST /api/v1/users"})
          `shouldSatisfy` contains "\"entrypoint\""
      it "includes the entrypoint value" $
        encode (toJSON base {entrypoint = Just "POST /api/v1/users"})
          `shouldSatisfy` contains "POST /api/v1/users"
      it "includes 'componentVersion' when Just" $
        encode (toJSON base {componentVersion = Just "1.4.2"})
          `shouldSatisfy` contains "\"componentVersion\""
      it "includes the componentVersion value" $
        encode (toJSON base {componentVersion = Just "1.4.2"})
          `shouldSatisfy` contains "1.4.2"
      it "includes 'exception' as a string when Just" $ do
        ex <- Ex.try (Ex.evaluate (error "boom")) :: IO (Either Ex.SomeException ())
        case ex of
          Left e ->
            encode (toJSON base {exception = Just e})
              `shouldSatisfy` contains "\"exception\""
          Right _ -> expectationFailure "expected exception"
      it "includes 'requestInfo' when Just" $ do
        let ri = RequestInfo {requestId = Just "req_1", requestHeaders = [], requestBody = Nothing}
        encode (toJSON base {requestInfo = Just ri})
          `shouldSatisfy` contains "\"requestInfo\""
      it "includes 'callStack' as a string when Just" $ do
        let internal = internalErrorInfo (mkSomeError TestErrorA)
            withCs = internal {callStack = Just GHC.callStack}
        encode (toJSON withCs) `shouldSatisfy` contains "\"callStack\""

  describe "RequestContent" $ do
    describe "ToJSON — JsonBody" $ do
      it "serializes with type 'json'" $
        encode (toJSON (JsonBody (object ["x" .= (1 :: Int)])))
          `shouldSatisfy` contains "\"json\""
      it "serializes with a 'body' field" $
        encode (toJSON (JsonBody (object ["x" .= (1 :: Int)])))
          `shouldSatisfy` contains "\"body\""
      it "preserves the JSON structure in body" $
        encode (toJSON (JsonBody (object ["x" .= (1 :: Int)])))
          `shouldSatisfy` contains "\"x\""

    describe "ToJSON — TextBody" $ do
      it "serializes with type 'text'" $
        encode (toJSON (TextBody "hello"))
          `shouldSatisfy` contains "\"text\""
      it "serializes with a 'body' field" $
        encode (toJSON (TextBody "hello"))
          `shouldSatisfy` contains "\"body\""
      it "preserves the text value in body" $
        encode (toJSON (TextBody "hello"))
          `shouldSatisfy` contains "hello"

  describe "RequestInfo" $ do
    let emptyRi = RequestInfo {requestId = Nothing, requestHeaders = [], requestBody = Nothing}

    describe "ToJSON — null/empty fields are omitted" $ do
      it "omits 'requestId' when Nothing" $
        encode (toJSON emptyRi) `shouldSatisfy` notContains "requestId"
      it "omits 'headers' when list is empty" $
        encode (toJSON emptyRi) `shouldSatisfy` notContains "headers"
      it "omits 'body' when Nothing" $
        encode (toJSON emptyRi) `shouldSatisfy` notContains "body"

    describe "ToJSON — non-empty fields are included" $ do
      it "includes 'requestId' when Just" $
        encode (toJSON emptyRi {requestId = Just "req_abc"})
          `shouldSatisfy` contains "\"requestId\""
      it "includes the requestId value" $
        encode (toJSON emptyRi {requestId = Just "req_abc"})
          `shouldSatisfy` contains "req_abc"
      it "includes 'headers' when non-empty" $
        encode (toJSON emptyRi {requestHeaders = [("Content-Type", "application/json")]})
          `shouldSatisfy` contains "\"headers\""
      it "includes header name and value" $ do
        let encoded = encode (toJSON emptyRi {requestHeaders = [("Content-Type", "application/json")]})
        encoded `shouldSatisfy` contains "Content-Type"
        encoded `shouldSatisfy` contains "application/json"
      it "includes 'body' when Just" $
        encode (toJSON emptyRi {requestBody = Just (TextBody "data")})
          `shouldSatisfy` contains "\"body\""

  describe "CaughtException" $ do
    it "Show includes the exception message" $ do
      let ce = CaughtException "CODE" (Ex.SomeException (userError "test msg")) Nothing
      show ce `shouldSatisfy` ("test msg" `isInfixOf`)

    it "publicErrorInfo uses caughtCode as the error code" $ do
      let ce = CaughtException "MY_CUSTOM_CODE" (Ex.SomeException (userError "oops")) Nothing
      code (publicErrorInfo ce) `shouldBe` "MY_CUSTOM_CODE"

    it "publicErrorInfo message is always the generic safe message" $ do
      let ce = CaughtException "ANY_CODE" (Ex.SomeException (userError "internal detail")) Nothing
      publicMessage (publicErrorInfo ce) `shouldBe` "An unexpected error occurred"

    it "internalErrorInfo has Critical severity" $ do
      let ce = CaughtException "CODE" (Ex.SomeException (userError "oops")) Nothing
      severity (internalErrorInfo ce) `shouldBe` Critical

    it "internalErrorInfo.exception holds the original exception" $ do
      let originalEx = Ex.SomeException (userError "original")
          ce = CaughtException "CODE" originalEx Nothing
      exception (internalErrorInfo ce)
        `shouldSatisfy` maybe False (("original" `isInfixOf`) . show)

    it "internalErrorInfo.callStack is Nothing when caughtCallStack is Nothing" $ do
      let ce = CaughtException "CODE" (Ex.SomeException (userError "oops")) Nothing
      callStack (internalErrorInfo ce) `shouldSatisfy` isNothing

  describe "SomeError" $ do
    it "Show delegates to the wrapped error's Show instance" $
      show (mkSomeError TestErrorA) `shouldBe` "TestErrorA"

    it "publicErrorInfo extracts correct code" $ do
      let pub = publicErrorInfo (mkSomeError TestErrorA)
      code pub `shouldBe` "TEST_ERROR_A"

    it "publicErrorInfo extracts correct message" $ do
      let pub = publicErrorInfo (mkSomeError TestErrorA)
      publicMessage pub `shouldBe` "Error A occurred"

    it "internalErrorInfo extracts correct severity" $ do
      let internal = internalErrorInfo (mkSomeError TestErrorA)
      severity internal `shouldBe` Error

    it "wraps different error types, each with their own info" $ do
      let pubA = publicErrorInfo (mkSomeError TestErrorA)
          pubB = publicErrorInfo (mkSomeError TestErrorB)
      code pubA `shouldBe` "TEST_ERROR_A"
      code pubB `shouldBe` "TEST_ERROR_B"

    describe "ToJSON" $ do
      it "serializes via publicErrorInfo" $
        toJSON (mkSomeError TestErrorA) `shouldBe` toJSON (publicErrorInfo TestErrorA)

  describe "Failure" $ do
    describe "Semigroup" $ do
      it "combining two single-error Failures yields two errors" $ do
        let combined = mkFailure TestErrorA <> mkFailure TestErrorB
        length (getErrors combined) `shouldBe` 2

      it "preserves left errors before right errors" $ do
        let combined = mkFailure TestErrorA <> mkFailure TestErrorB
            errList = getErrors combined
        show (NE.head errList) `shouldBe` "TestErrorA"
        show (NE.last errList) `shouldBe` "TestErrorB"

      it "satisfies associativity (error count)" $
        property $ \(Positive n1) (Positive n2) (Positive n3) ->
          let ra = mkFailureN (n1 `mod` 5 + 1)
              rb = mkFailureN (n2 `mod` 5 + 1)
              rc = mkFailureN (n3 `mod` 5 + 1)
           in length (getErrors ((ra <> rb) <> rc))
                == length (getErrors (ra <> (rb <> rc)))

    describe "ToJSON" $ do
      it "serializes as a JSON array" $
        encode (mkFailure TestErrorA) `shouldSatisfy` startsWith "["
