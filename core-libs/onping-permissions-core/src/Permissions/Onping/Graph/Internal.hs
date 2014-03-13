module Permissions.Onping.Graph.Internal
    (
     getSuperUserList,getAllEntitites
    ) where

import Control.Applicative
import Persist.Mongo.Settings
import Database.Persist
import Permissions.Onping.Types
import qualified Data.Text as T
import qualified Data.List as L
-- import qualified Data.Yaml as Y



-- | filterSuperUserList is extracted from Onping, modified to return the SuperUser Tree of any user
-- The first function here, getSuperUserList... Does nothing more than gather every possible entity
getSuperUserList :: MongoDBConf -> UserId -> IO  [OnPingPermissionEntity]
getSuperUserList mdbc uid = do  
  aidG <- runDBConf mdbc $ do 
    l <- selectList [] [] 
    return $ l
  aidU <- runDBConf mdbc $ do
    l <- selectList [] []
    return $ l
  filterSuperUserList' mdbc uid  $ (PEgroup <$> aidG) ++ (PEuser <$> aidU) -- Complete list of all UserTags and Groups

getAllEntitites :: MongoDBConf -> IO [OnPingPermissionEntity]
getAllEntitites mdbc = do 
  aidG <- runDBConf mdbc $ do 
    l <- selectList [] [] 
    return $ l
  aidU <- runDBConf mdbc $ do
    l <- selectList [] []
    return $ l
  return $ (PEgroup <$> aidG) ++ (PEuser <$> aidU) -- Complete list of all UserTags and Groups



filterSuperUserList' :: MongoDBConf -> UserId -> [PermissionEntity (Entity UserTag) (Entity Group)] -> IO [(PermissionEntity (Entity UserTag)  (Entity Group))]
filterSuperUserList' mdbc aid peList =  do
  peListSeed <- runDBConf mdbc $ do
                  (Just uTag) <- selectFirst [UserTagUser ==. (aid)] []  
                  (Just gTag) <- selectFirst [GroupId ==. (userTagGroup.entityVal $ uTag)] [] -- Default user grp
--                  egids  <- selectList [GroupUserJoinUId ==. aid] []-- other user grps
--                  grps   <- selectList [GroupId <-. (groupUserJoinGId.entityVal  <$> egids)] [] 
                  return $ [PEuser uTag, PEgroup gTag] -- ++ (PEgroup <$> grps)
  return $ (superListMaker peListSeed ) L.\\ peListSeed -- subtracting out 1 instance of each, if overlap occurs so be it
    where 
         superListMaker :: [PermissionEntity (Entity UserTag) (Entity Group)] -> [(PermissionEntity (Entity UserTag)  (Entity Group))]
         superListMaker peListSeed = (L.foldl (\a b ->  (runFilter a b) ++ a ) peListSeed peList) 


-- | runs the filter and collects the results 
runFilter :: [PermissionEntity (Entity UserTag) (Entity Group)] -> PermissionEntity (Entity UserTag) (Entity Group) -> [PermissionEntity (Entity UserTag) (Entity Group)]
runFilter (peTester:peList) candidate 
    | suFilterFunction peTester candidate = [candidate] 
    | otherwise = runFilter peList candidate 
runFilter [] _ = []



suFilterFunction :: PermissionEntity (Entity UserTag) (Entity Group) -> PermissionEntity (Entity UserTag) (Entity Group) -> Bool
suFilterFunction (PEuser superUser) (PEuser user)
    |either (\_ -> False ) (\u -> (userTagOwner.entityVal $ user) == u) (pullValueForCompare superUser) = True
    |otherwise = False
                 
suFilterFunction (PEuser superUser) (PEgroup grp)
  |((groupOwner.entityVal $ grp) == (userTagUser.entityVal $ superUser) ) = True
  |otherwise = False

suFilterFunction (PEgroup superGroup) (PEuser user)
  |(userTagSuperGroup.entityVal $ user) == entityKey superGroup = True
  |otherwise = False
               
suFilterFunction (PEgroup superGroup) (PEgroup grp)
  |(groupGroup.entityVal $ grp) == entityKey superGroup = True
  |otherwise = False
suFilterFunction _ _ = False

pullValueForCompare :: (PersistField a) => Entity entity -> Either T.Text a
pullValueForCompare = fromPersistValue.unKey.entityKey
