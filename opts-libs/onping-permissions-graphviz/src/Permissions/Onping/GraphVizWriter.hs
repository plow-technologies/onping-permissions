{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings,DeriveGeneric #-}

module Permissions.Onping.GraphVizWriter where

import Permissions.Onping.GraphVizWriter.Internal



import Permissions.Onping.Graph
import Prelude 
--import Data.List
import Database.Persist
-- import Permissions.Onping.Internal
import Permissions.Onping.Types 
import Persist.Mongo.Settings 
import Data.GraphViz hiding (Path)
import Data.Graph.Inductive.Graph
import Control.Applicative 
import Data.Aeson

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Graph as G
import qualified Data.Tree as Tree
import Data.GraphViz.Printing
import Data.GraphViz.Attributes.Complete


writeGraphViz :: MongoDBConf -> IO ()
writeGraphViz mdbc = do
  pg <- constructPermissionGraph mdbc
  let    gtd =  graphToDot (goodDefaults) pg
         str =  renderDot.toDot $ gtd
  TIO.writeFile "autograph.gv" $  str  


permissionUserLabel :: forall a. Show a => a -> [Attribute]
permissionUserLabel u = [Label $ StrLabel (TL.pack.show $ u) , nodeFontSize
                         , FillColor clearColor, Shape Circle, clearStyle ]
    where clearColor = [WC (RGB 74 212 125 ) Nothing ]
          clearStyle = Style [SItem Filled []]



globalAttrs :: GlobalAttributes
globalAttrs = GraphAttrs [Size (GSize 10.0 (Just 10.0) True),
                          Scale $ PVal (createPoint 1 1),
                          RankDir FromLeft,
                          Ratio FillRatio,
                          Overlap ScaleXYOverlaps,
--                          Ratio $ AspectRatio 0.50,
                          Splines SplineEdges, 
                          FontName "courier" ]

goodDefaults :: forall el t1.
                      GraphvizParams
                        t1
                        (PermissionEntity (Entity UserTag) (Entity Group))
                        el
                        TL.Text
                        (PermissionEntity (Entity UserTag) (Entity Group))
goodDefaults = defaultParams {
                 globalAttributes =  [globalAttrs],
                 fmtNode = (\(_,l) -> labelMatcher l ),
                 fmtEdge = (\(n1,n2,_) -> labelEdge n1 n2 ),
                 clusterID = (\x -> Data.GraphViz.Str x)}

labelEdge :: forall t t1. t -> t1 -> [Attribute]
labelEdge _ _ = [Color penColorBlack, PenWidth epenWidth, ArrowSize arrowSize]
  where penColorBlack = [WC (RGB 0 0 0) Nothing]
--        penColorGreen  = [WC (RGB 74 212 125) Nothing]
--        penColorRed    = [WC (RGB 217 4 54) Nothing]
--        penColorLightG = [WC (RGB 48 8 158) Nothing]
--        penColorOrange = [WC (RGB 217 114 4) Nothing]

        epenWidth = 10
        arrowSize = 2

labelMatcher :: PermissionEntity (Entity UserTag) (Entity Group) -> [Attribute]
labelMatcher (PEgroup g)  = permissionGroupLabel (groupName.entityVal $ g)
labelMatcher (PEuser  u)  = permissionUserLabel  (toJSON . userTagUser . entityVal $ u) 
labelMatcher (Empty    )  = permissionGroupLabel ("Empty")
nodeFontSize :: Attribute
nodeFontSize = FontSize 10.0

permissionGroupLabel :: T.Text -> [Attribute]
permissionGroupLabel g = [Label $ StrLabel (TL.fromStrict $ g) , nodeFontSize
                         , FillColor clearColor, Shape Circle, clearStyle ]
    where clearColor = [WC (RGB 74 212 125 ) Nothing ]
          clearStyle = Style [SItem Filled []]


