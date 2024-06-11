{- 
  
  Bus scheduling in Curry

  Problem from Taha "Introduction to Operations Research", page 58.
  Scheduling of buses during a day.

  This is a slightly more general model than Taha's.

  This is a port of my Picat model http://hakank.org/picat/bus_scheduling.pi

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD
import Debug.Trace


busScheduling demands =
                 let
                    timeSlots = length demands -- number of time slots
                    maxNum = Data.List.sum(demands)

                    -- how many buses start at time xs[t]?
                    xs = take timeSlots (domain 0 maxNum)
                    -- The total number of buses (be be minimized)
                    numBuses = head (domain 0 (timeSlots*maxNum))
                    numBusesOpt = anyOf [0..(timeSlots*maxNum)] -- for minimization 
                 in
                    solveFDOne [Minimize numBusesOpt] (numBuses:xs) $
                    numBuses =# Data.List.sum(xs) /\
                    fd numBusesOpt =# numBuses /\
                    --  meet the demands for this and the next time slot
                    foldl1 (/\) [ x1+x2 >=# fd d
                                  | (x1,x2,d) <- zip3 xs (tail xs) demands ] /\
                    -- "around the corner"
                    (last xs) + (head xs) >=# (fd $ last demands)

{-
  ([26],[0,8,2,5,7,4])
  Execution time: 32 msec. / elapsed: 33 msec.
-}
main = splitAt 1 $  busScheduling [8, 10, 7, 12, 4, 4]
                 
