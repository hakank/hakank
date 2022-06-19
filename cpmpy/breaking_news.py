"""
Breaking News puzzle in cpmpy.

From http://brownbuffalo.sourceforge.net/BreakingNewsClues.html
'''
Title: Breaking News
Author: Faith Johnson
Publication: Dell Logic Puzzles
Issue: April, 1998
Page: 9
Stars: 1

The Daily Galaxy sent its four best reporters 
  (Corey, Jimmy, Lois, and Perry) 
to different locations 
  (Bayonne, New Hope, Port Charles, and South Amboy) 
to cover four breaking news events 
  (30-pound baby, blimp launching, skyscraper dedication, and 
   beached whale). 
Their editor is trying to remember where each of the reporters is. 
Can you match the name of each reporter with the place he or she 
was sent, and the event that each covered?

1. The 30-pound baby wasn't born in South Amboy or New Hope.
2. Jimmy didn't go to Port Charles.
3. The blimp launching and the skyscraper dedication were covered, 
  in some order, by Lois and the reporter who was sent to Port Charles.
4. South Amboy was not the site of either the beached whale or the 
  skyscraper dedication.
5. Bayonne is either the place that Corey went or the place where 
  the whale was beached, or both.

Determine: Reporter -- Location -- Story
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def breaking_news():

  n = 4

  corey = 0
  jimmy = 1
  lois  = 2
  perry = 3
  reporters = [corey, jimmy, lois, perry]
  reporters_s = ["Corey", "Jimmy", "Lois", "Perry"]

  # Note:
  # Since we are using inverse, the domains must be 0..n-1, not 1..n!
  # 
  locations = intvar(0,n-1,shape=n,name="locations")
  bayonne, newhope, portcharles, southamboy = locations
  locations_s = ["Bayonne", "New Hope", "Port Charles", "South Amboy"]

  locations_inv = intvar(0,n-1,shape=n,name="locations_inv")
  
  news = intvar(0,n-1,shape=n,name="news")
  baby, blimp, skyscraper, whale = news
  news_s = ["Baby", "Blimp", "Skyscraper", "Whale"]
  
  news_inv = intvar(0,n-1,shape=n,name="news_inv")

  model = Model([
    AllDifferent(locations),
    AllDifferent(news),
    AllDifferent(locations_inv),
    AllDifferent(news_inv),

    # use assignment (inverse) for the presentation   
    inverse(locations, locations_inv),
    inverse(news, news_inv),

    # 1. The 30-pound baby wasn"t born in South Amboy or New Hope.
    baby != southamboy,
    baby != newhope,
   
    # 2. Jimmy didn"t go to Port Charles.
    jimmy != portcharles,
   
    # 3. The blimp launching and the skyscraper dedication were covered, 
    #    in some order, by Lois and the reporter who was sent to 
    #    Port Charles.
    lois != portcharles,
    ( 
      ((blimp == lois) & (skyscraper == portcharles)) |
      ((skyscraper == lois) & (blimp == portcharles))
    ),

    # 4. South Amboy was not the site of either the beached whale or the 
    #    skyscraper dedication.
    southamboy != whale,
    southamboy != skyscraper,

    # 5. Bayonne is either the place that Corey went or the place where 
    #    the whale was beached, or both.
    ( (bayonne == corey) | (bayonne == whale) )
    ])

  def print_sol():
    # print("reporters:",reporters)
    print("locations:",locations.value()+1, "locations_inv:", locations_inv.value())
    print("news     :",news.value()+1,      "news_inv     :", news_inv.value())
    for i in range(n):
      print(f"{reporters_s[i]:10s} {locations_s[locations_inv[i].value()]:14s} {news_s[news_inv[i].value()]:10s}")
    print()
    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)

breaking_news()
