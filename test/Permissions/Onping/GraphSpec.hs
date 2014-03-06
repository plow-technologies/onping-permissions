{-# LANGUAGE OverloadedStrings #-}
module Permissions.Onping.GraphSpec (main, spec) where
    
import Test.Hspec

import Permissions.Onping.Graph
import Database.Persist
import Persist.Mongo.Settings
import Data.Aeson
import Control.Applicative
import qualified Data.Yaml as Y
import Data.Traversable
import Data.Maybe
        
main :: IO ()
main = hspec spec
               
spec :: Spec
spec = do
  describe "getSuperUserList" $ do
    it "should pull a super user list from the supplied runDB with config.yml" $ do        
      emdbc <- Y.decodeFileEither "config.yml"
      case emdbc of 
        Left _ -> do 
                   print "error decoding config.yml"
                   False `shouldBe` True
        Right mdbc -> do 
                   meusr <- runDBConf mdbc $ selectFirst [] [Asc UserId]
                   let muid = entityKey <$> meusr
                   rslt <- traverse constructPermissionNodes muid
                   (isJust rslt) `shouldBe` True


