{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings,DeriveGeneric #-}

module Permissions.Onping.Graph where
import Prelude 
--import Data.List
import Database.Persist
import Permissions.Onping.Internal
import Permissions.Onping.Types 
import Persist.Mongo.Settings 
-- import Data.GraphViz hiding (Path)
import Data.Graph.Inductive.Graph
import Control.Applicative 
import Data.Aeson
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Graph as G
import qualified Data.Tree as Tree
-- import Data.GraphViz.Printing
--import Data.GraphViz.Attributes.Complete



permissionEntityToNode ::(Integral a) =>  OnPingPermissionEntity -> a -> OPNode
permissionEntityToNode ope = (\x -> (fromIntegral x , ope))


makeOnPingPermissionEdgeList :: PermissionMap -> OnPingPermissionEntity -> (S.Set (LEdge ()))
makeOnPingPermissionEdgeList pm (PEuser u) =  let uid = userTagUser.entityVal $ u
                                                  o = userTagOwner.entityVal $ u
                                                  sg = userTagSuperGroup.entityVal $ u
                                                  makeLedge = (\p c -> ((fst p),(fst c),()))
                                                  muNodeId = M.lookup (encodeAKey . PEuser $ uid) pm
                                                  msgNodeId = M.lookup (encodeAKey . PEgroup $ sg) pm
                                                  moNodeId = M.lookup (encodeAKey . PEuser $ o) pm
                                                  sgNodeIdSet = fromMaybe S.empty (msgNodeId >>= (\sgId -> muNodeId >>= (\uId -> return $ S.insert (makeLedge sgId uId) S.empty )))
                                                  oNodeIdSet = fromMaybe S.empty  (moNodeId  >>= (\oId ->  muNodeId >>= (\uId -> return $ S.insert (makeLedge oId uId) S.empty  )))
                                              in S.union sgNodeIdSet oNodeIdSet
makeOnPingPermissionEdgeList pm (PEgroup g) =  let gid = entityKey $ g
                                                   o = groupOwner.entityVal $ g
                                                   sg = groupGroup.entityVal $ g
                                                   makeLedge = (\p c -> ((fst p),(fst c),()))
                                                   mgNodeId = M.lookup (encodeAKey . PEgroup $ gid) pm
                                                   msgNodeId = M.lookup (encodeAKey . PEgroup $ sg) pm
                                                   moNodeId = M.lookup (encodeAKey . PEuser $ o) pm
                                                   sgNodeIdSet = fromMaybe S.empty (msgNodeId >>= (\sgId -> mgNodeId >>= (\gId -> return $ S.insert (makeLedge sgId gId) S.empty )))
                                                   oNodeIdSet = fromMaybe S.empty  (moNodeId  >>= (\oId ->  mgNodeId >>= (\gId -> return $ S.insert (makeLedge oId gId) S.empty  )))
                                               in S.union sgNodeIdSet oNodeIdSet
makeOnPingPermissionEdgeList _ (Empty) = S.empty
        





-- |Lookups are most likely done by by GID or UID, so here is a typesafe 
-- key generator to look stuff up by them 
makeAKey :: OnPingPermissionEntity -> AKey 
makeAKey (PEuser u) = AKey . encode.toJSON $ uKey
 where uKey :: OnpingPermissionKey
       uKey = PEuser . userTagUser . entityVal $ u

makeAKey (PEgroup g) = AKey . encode.toJSON $ gKey
 where gKey :: OnpingPermissionKey
       gKey = PEgroup . entityKey $ g
makeAKey (Empty) = AKey . encode.toJSON $ ()

encodeAKey :: OnpingPermissionKey -> AKey 
encodeAKey = AKey . encode .toJSON 

decodeAKey :: AKey -> Maybe OnpingPermissionKey
decodeAKey (AKey a) = decode a
                      
-- | Creates a map to the index of the node in the svg 
constructPermissionNodeMap ::  IO (PermissionMap, [OnPingPermissionEntity])
constructPermissionNodeMap = do 
  ul <- getAllEntitites
  let emptyNodeMap = M.empty :: M.Map AKey OPNode 
      zNodes = zip ([1 ..] :: [Integer] ) ul
      nodeMap = foldl  (\m (v,k)  -> M.insert (makeAKey k) (permissionEntityToNode k v) m ) emptyNodeMap zNodes
  return (nodeMap,ul )


constructPermissionGraph :: IO OnPingPermissionGraphViz
constructPermissionGraph = do 
  (pm,opnLst) <- constructPermissionNodeMap 
  let edgeList = S.toList $ L.foldl' (\s on  -> S.union (makeOnPingPermissionEdgeList pm on) s ) S.empty opnLst :: [LEdge () ]
      nodeList = M.elems pm :: [OPNode]
  return $ (mkGraph nodeList edgeList ) 

emptyPermissionGraph :: OnPingPermissionGraphViz 
emptyPermissionGraph = mkGraph [] []

constructPermissionGraphFromMap :: PermissionMap -> [OnPingPermissionEntity] -> IO OnPingPermissionGraphViz
constructPermissionGraphFromMap pm opnLst = do 
  let edgeList = S.toList $ L.foldl' (\s on  -> S.union (makeOnPingPermissionEdgeList pm on) s ) S.empty opnLst :: [LEdge () ]
      nodeList = M.elems pm :: [OPNode]
  return $ (mkGraph nodeList edgeList ) 


constructPartialGraph :: OnPingPermissionEntity -> IO OnPingPermissionGraphViz
constructPartialGraph  _ = do 
  (pm,opnLst) <- constructPermissionNodeMap 
  pg <- constructPermissionGraphFromMap pm opnLst
  return pg
      
convertOPGVtoOPG :: OnPingPermissionGraphViz -> OnpingPermissionGraph 
convertOPGVtoOPG opgv = G.buildG (nodeRange opgv) ((\(i,o,_) -> (i,o)) <$> labEdges opgv)
    
getCycles :: OnPingPermissionGraphViz -> Path
getCycles opgv = foldl constructPaths [] (G.scc $ convertOPGVtoOPG opgv)
    where 
      constructPaths pthList other = (Tree.flatten other) ++ pthList 

