{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,DeriveGeneric, MultiParamTypeClasses, FlexibleInstances  #-}
module Permissions.Onping.Types where

import Persist.Mongo.Settings
import Database.Persist
import GHC.Generics
import Data.Aeson
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Graph as G
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M









import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)


data PermissionEntity a b = PEuser {getPEuser::a} | PEgroup {getPEgroup::b} |Empty
    deriving (Show,Eq,Read,Generic,Ord)



instance (FromJSON a , FromJSON b ) => FromJSON (PermissionEntity a b)
instance (ToJSON a , ToJSON b ) => ToJSON (PermissionEntity a b)


type OnPingPermissionEntity = PermissionEntity (Entity UserTag) (Entity Group) 
type OnpingPermissionKey = PermissionEntity UserId GroupId

-- type Permission = Tree (PermissionEntity UserId GroupId)
permissionGroups :: [PermissionEntity a b] -> [PermissionEntity a b]
permissionGroups = filter chk 
                   where chk (PEgroup _) = True
                         chk (_) = False

permissionUsers :: [PermissionEntity a b] -> [PermissionEntity a b] 
permissionUsers = filter chk 
    where chk (PEuser _) = True
          chk (_) = False


type OPNode = LNode OnPingPermissionEntity

-- This type of Graph is for viewing 
type OnPingPermissionGraphViz = Gr OnPingPermissionEntity ()

type OnpingPermissionGraph = G.Graph 



newtype AKey = AKey {unAKey ::  BL.ByteString } 
    deriving (Show,Eq,Generic,Ord)

type PermissionMap = (M.Map  AKey OPNode)

-- This is the real Graph where you can do things like run traversals
