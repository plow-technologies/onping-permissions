{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings,DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction, NoImplicitPrelude #-}
module Permissions.Onping.DiagramWriter where
import Permissions.Onping.Graph
import Prelude (($))
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Maybe
import Text.Blaze.Svg.Renderer.Utf8

main = renderSvg $ renderDia SVG (SVGOptions (Width 250) Nothing) (circle 1 :: Diagram B R2)

