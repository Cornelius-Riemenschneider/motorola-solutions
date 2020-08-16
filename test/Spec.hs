{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Lib (app, truncate_db)
import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

postJson path = request methodPost path [(hContentType, "application/json")]

main :: IO ()
main =
  do
    -- TODO don't run this on your production db...
    truncate_db
    hspec spec

spec :: Spec
spec = with (return app) $
  do
    describe "Scenario 1" $ do
      it "add radio 100" $ do
        postJson "/radios/100" [json|{"alias": "Radio100", "allowed_locations": ["CPH-1", "CPH-2"]}|] `shouldRespondWith` 200
      it "add radio 101" $ do
        postJson "/radios/101" [json|{ "alias": "Radio101", "allowed_locations": ["CPH-1", "CPH-2", "CPH-3"]}|] `shouldRespondWith` 200
      it "Set location of radio 100 to \"CPH-1\"" $ do
        postJson "/radios/100/location" [json| { "location": "CPH-1" }|] `shouldRespondWith` 200
      it "Set location of radio 101 to \"CPH-3\" (accepted)" $ do
        postJson "/radios/101/location" [json| { "location": "CPH-3" }|] `shouldRespondWith` 200
      it "Set location of radio 100 to \"CPH-3\" (denied)" $ do
        postJson "/radios/100/location" [json| { "location": "CPH-3" }|] `shouldRespondWith` 403
      it "Retrieve location of radio 101 (returns \"CPH-3\")" $ do
        get "/radios/101/location" `shouldRespondWith` [json|{"location":"CPH-3"}|]
      it "Retrieve location of radio 100 (returns \"CPH-1\")" $ do
        get "/radios/100/location" `shouldRespondWith` [json|{"location":"CPH-1"}|]
    describe "Scenario 2" $
      do
        it "add radio 102" $ do
          postJson "/radios/102" [json|{"alias": "Radio102", "allowed_locations": ["CPH-1", "CPH-3"]}|] `shouldRespondWith` 200
        it "get location should return undefined" $ do
          get "/radios/102/location" `shouldRespondWith` 404
