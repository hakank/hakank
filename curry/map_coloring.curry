{- 
  
  Map coloring in Curry

  This is a port of my Picat program http://hakank.org/picat/map_coloring_australia.pi
  """
  Example from Marriot & Stuckey "Programming with Constraints", 
  page 86.

  Western Australia  WA
  Northen Territory  NT
  South Australia    SA
  Queensland         Q
  New South Wales    NSW
  Victoria           V
  Tasmania           T

  Colors: red, green, blue (1..3)

  """

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
-- import CLP.FD

data color = Red | Green | Blue deriving (Eq,Enum,Show)

colors = anyOf [Red,Green,Blue]

data region = WA | NT | SA | Q | NSW | V | T deriving(Eq,Enum,Show)

regions = anyOf [WA, NT, SA, Q, NSW, V, T]


regionColor r = c =:= colors  &> c where c free

-- THIS IS NOT CORRECT. It should assign a color to all the regions.
-- Right now it's just one assignment... 
mapAustralia = r1 =:= regions &> r2 =:= regions &> r1 /= r2 &>
               c1 =:= colors &> c2 =:= colors &>
               ((neighbour r1 r2 =:= True && c1 /= c2)  || neighbour r1 r2 =:= False) 
               &> (r1,c1,r2,c2) 
               where r1,r2,c1,c2 free

neighbour WA NT = success
neighbour WA SA = success
neighbour NT WA = success
neighbour NT SA = success
neighbour NT Q = success
neighbour SA WA = success
neighbour SA NT = success
neighbour SA Q = success
neighbour SA NSW = success
neighbour SA V = success
neighbour Q NT = success
neighbour Q SA = success
neighbour Q NSW = success
neighbour NSW SA = success
neighbour NSW Q = success
neighbour NSW V = success
neighbour V SA = success
neighbour V NSW = success
neighbour V T = success
neighbour T V = success
