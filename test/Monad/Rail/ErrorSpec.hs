{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Monad.Rail.ErrorSpec (spec) where

import qualified Control.Exception as Ex
import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Data (Data)
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
  errorPublicMessage TestErrorA = "Error A occurred"
  errorPublicMessage TestErrorB = "Error B occurred"
  errorCode TestErrorA = "TestErrorA"
  errorCode TestErrorB = "TestErrorB"
  errorDetails TestErrorB = Just (object ["key" .= ("value" :: Text)])
  errorDetails _ = Nothing
  errorInternalMessage TestErrorA = Just "Internal details for A"
  errorInternalMessage _ = Nothing
  errorSeverity TestErrorB = Critical
  errorSeverity _ = Error

-- Simple error type using the Data-derived code default
data SimpleError = NameEmpty | EmailInvalid
  deriving (Show, Data)

instance HasErrorInfo SimpleError where
  errorPublicMessage NameEmpty    = "Name cannot be empty"
  errorPublicMessage EmailInvalid = "Email format is invalid"

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
              code = "GenericError",
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
        let ri = RequestInfo {requestId = Just "req_1", requestMethod = Nothing, requestIp = Nothing, requestLength = Nothing, requestHeaders = [], requestBody = Nothing}
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
    let emptyRi = RequestInfo {requestId = Nothing, requestMethod = Nothing, requestIp = Nothing, requestLength = Nothing, requestHeaders = [], requestBody = Nothing}

    describe "ToJSON — null/empty fields are omitted" $ do
      it "omits 'requestId' when Nothing" $
        encode (toJSON emptyRi) `shouldSatisfy` notContains "requestId"
      it "omits 'method' when Nothing" $
        encode (toJSON emptyRi) `shouldSatisfy` notContains "method"
      it "omits 'ip' when Nothing" $
        encode (toJSON emptyRi) `shouldSatisfy` notContains "ip"
      it "omits 'length' when Nothing" $
        encode (toJSON emptyRi) `shouldSatisfy` notContains "length"
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
      it "includes 'method' when Just" $
        encode (toJSON emptyRi {requestMethod = Just "POST"})
          `shouldSatisfy` contains "\"method\""
      it "includes the method value" $
        encode (toJSON emptyRi {requestMethod = Just "POST"})
          `shouldSatisfy` contains "POST"
      it "includes 'ip' when Just" $
        encode (toJSON emptyRi {requestIp = Just "203.0.113.42"})
          `shouldSatisfy` contains "\"ip\""
      it "includes the ip value" $
        encode (toJSON emptyRi {requestIp = Just "203.0.113.42"})
          `shouldSatisfy` contains "203.0.113.42"
      it "includes 'length' when Just" $
        encode (toJSON emptyRi {requestLength = Just 1024})
          `shouldSatisfy` contains "\"length\""
      it "includes the length value" $
        encode (toJSON emptyRi {requestLength = Just 1024})
          `shouldSatisfy` contains "1024"
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

  describe "HasErrorInfo simple implementation" $ do
    it "errorPublicMessage returns the configured message for each constructor" $ do
      errorPublicMessage NameEmpty    `shouldBe` ("Name cannot be empty" :: Text)
      errorPublicMessage EmailInvalid `shouldBe` ("Email format is invalid" :: Text)
    it "errorCode defaults to the constructor name via Data" $ do
      errorCode NameEmpty    `shouldBe` ("NameEmpty" :: Text)
      errorCode EmailInvalid `shouldBe` ("EmailInvalid" :: Text)

  describe "publicErrorInfo" $ do
    it "assembles code from the constructor name" $
      code (publicErrorInfo NameEmpty) `shouldBe` "NameEmpty"
    it "assembles code from each constructor independently" $
      code (publicErrorInfo EmailInvalid) `shouldBe` "EmailInvalid"
    it "assembles publicMessage from errorPublicMessage" $
      publicMessage (publicErrorInfo NameEmpty) `shouldBe` "Name cannot be empty"
    it "sets details to Nothing by default" $
      details (publicErrorInfo NameEmpty) `shouldBe` Nothing
    it "assembles details from errorDetails when Just" $
      details (publicErrorInfo (mkSomeError TestErrorB)) `shouldBe` Just (object ["key" .= ("value" :: Text)])

  describe "internalErrorInfo" $ do
    it "default severity is Error" $
      severity (internalErrorInfo NameEmpty) `shouldBe` Error
    it "all optional fields default to Nothing for a simple error" $ do
      let internal = internalErrorInfo NameEmpty
      internalMessage internal `shouldSatisfy` isNothing
      exception internal       `shouldSatisfy` isNothing
      requestInfo internal     `shouldSatisfy` isNothing
      component internal       `shouldSatisfy` isNothing
      userId internal          `shouldSatisfy` isNothing
      entrypoint internal      `shouldSatisfy` isNothing
      componentVersion internal `shouldSatisfy` isNothing
      callStack internal       `shouldSatisfy` isNothing

  describe "UnhandledException" $ do
    it "Show includes the exception message" $ do
      let ue = UnhandledException (Just "CODE") (Ex.SomeException (userError "test msg")) Nothing Nothing
      show ue `shouldSatisfy` ("test msg" `isInfixOf`)

    it "publicErrorInfo uses unhandledCode as the error code when Just" $ do
      let ue = UnhandledException (Just "MyCustomCode") (Ex.SomeException (userError "oops")) Nothing Nothing
      code (publicErrorInfo ue) `shouldBe` "MyCustomCode"

    it "publicErrorInfo defaults to \"UnhandledException\" when unhandledCode is Nothing" $ do
      let ue = UnhandledException Nothing (Ex.SomeException (userError "oops")) Nothing Nothing
      code (publicErrorInfo ue) `shouldBe` "UnhandledException"

    it "publicErrorInfo message is always the generic safe message when unhandledMessage is Nothing" $ do
      let ue = UnhandledException (Just "ANY_CODE") (Ex.SomeException (userError "internal detail")) Nothing Nothing
      publicMessage (publicErrorInfo ue) `shouldBe` "An unexpected error occurred"

    it "publicErrorInfo uses unhandledMessage as public message when Just" $ do
      let ue = UnhandledException (Just "CODE") (Ex.SomeException (userError "detail")) Nothing (Just "Custom public message")
      publicMessage (publicErrorInfo ue) `shouldBe` "Custom public message"

    it "internalErrorInfo has Critical severity" $ do
      let ue = UnhandledException (Just "CODE") (Ex.SomeException (userError "oops")) Nothing Nothing
      severity (internalErrorInfo ue) `shouldBe` Critical

    it "internalErrorInfo.exception holds the original exception" $ do
      let originalEx = Ex.SomeException (userError "original")
          ue = UnhandledException (Just "CODE") originalEx Nothing Nothing
      exception (internalErrorInfo ue)
        `shouldSatisfy` maybe False (("original" `isInfixOf`) . show)

    it "internalErrorInfo.callStack is Nothing when unhandledCallStack is Nothing" $ do
      let ue = UnhandledException (Just "CODE") (Ex.SomeException (userError "oops")) Nothing Nothing
      callStack (internalErrorInfo ue) `shouldSatisfy` isNothing

  describe "SomeError" $ do
    it "Show delegates to the wrapped error's Show instance" $
      show (mkSomeError TestErrorA) `shouldBe` "TestErrorA"

    it "publicErrorInfo extracts correct code" $ do
      let pub = publicErrorInfo (mkSomeError TestErrorA)
      code pub `shouldBe` "TestErrorA"

    it "publicErrorInfo extracts correct message" $ do
      let pub = publicErrorInfo (mkSomeError TestErrorA)
      publicMessage pub `shouldBe` "Error A occurred"

    it "internalErrorInfo extracts correct severity" $ do
      let internal = internalErrorInfo (mkSomeError TestErrorA)
      severity internal `shouldBe` Error

    it "publicErrorInfo delegates errorDetails through the existential" $ do
      details (publicErrorInfo (mkSomeError TestErrorB)) `shouldBe` Just (object ["key" .= ("value" :: Text)])

    it "internalErrorInfo delegates errorInternalMessage through the existential" $ do
      internalMessage (internalErrorInfo (mkSomeError TestErrorA)) `shouldBe` Just "Internal details for A"

    it "internalErrorInfo optional fields are Nothing when not set" $ do
      let internal = internalErrorInfo (mkSomeError TestErrorA)
      exception internal `shouldSatisfy` isNothing

    it "wraps different error types, each with their own info" $ do
      let pubA = publicErrorInfo (mkSomeError TestErrorA)
          pubB = publicErrorInfo (mkSomeError TestErrorB)
      code pubA `shouldBe` "TestErrorA"
      code pubB `shouldBe` "TestErrorB"

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
