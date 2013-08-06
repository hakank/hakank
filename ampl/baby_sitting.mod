/*

  Baby sitting puzzle (Dell Logic Puzzles) in AMPL+CP.

  Problem from http://brownbuffalo.sourceforge.net/BabysittingClues.html
  """
  Title: Babysitting
  Author: Scott Marley
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 7
  Stars: 1

  Each weekday, Bonnie takes care of five of the neighbors' children. 
  The children's names are Keith, Libby, Margo, Nora, and Otto; last names 
  are Fell, Gant, Hall, Ivey, and Jule. Each is a different number of years 
  old, from two to six. Can you find each child's full name and age?

  1. One child is named Libby Jule.
  2. Keith is one year older than the Ivey child, who is one year older 
     than Nora.
  3. The Fell child is three years older than Margo.
  4. Otto is twice as many years old as the Hall child.

  Determine: First name - Last name - Age 
  """
 
  Result
         Keith Libby Margo Nora Otto
         Fell  Jule  Hall  Gant Ivey
         5     6     2     3    4  
  last = [1, 4, 3, 5, 2]
  age = [5, 6, 2, 3, 4]


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

param Keith := 1;
param Libby := 2;
param Margo := 3;
param Nora := 4;
param Otto := 5;


set Firstnames;
set Lastnames;

# decision variables
var firstnames{Firstnames} >= 1 <= n integer;
var lastnames{Lastnames} >= 1 <= n integer;
var ages{1..n} >= 2 <= 6 integer;

var Fell  >= 1 <= n integer;
var Gant  >= 1 <= n integer;
var Hall  >= 1 <= n integer;
var Ivey  >= 1 <= n integer;
var Jule  >= 1 <= n integer;


#
# constraints
#
s.t. c0a:
   firstnames['Keith'] = 1 and
   firstnames['Libby'] = 2 and
   firstnames['Margo'] = 3 and
   firstnames['Nora']  = 4 and
   firstnames['Otto']  = 5 and

   lastnames['Fell'] = Fell and
   lastnames['Gant'] = Gant and
   lastnames['Hall'] = Hall and
   lastnames['Ivey'] = Ivey and
   lastnames['Jule'] = Jule 
;

s.t. c0b: alldiff{i in Lastnames} lastnames[i];
s.t. c0c: alldiff{i in 1..n} ages[i];

#  1. One child is named Libby Jule.
s.t. c1: Jule = Libby;

#  2. Keith is one year older than the Ivey child, who is one 
#     year older than Nora.
s.t. c2:
   exists{ivey in 1..n}
     Ivey = ivey and
     ages[Keith] = ages[ivey] + 1 and
     Keith != ivey and

     ages[ivey] = ages[Nora] + 1 and
     ivey != Nora
;

#  3. The Fell child is three years older than Margo.
s.t. c3: 
   exists{fell in 1..n}
     Fell = fell and
     ages[fell] = ages[Margo] + 3 and
     fell != Margo 
;

#  4. Otto is twice as many years old as the Hall child.
s.t. c4:
   exists{hall in 1..n}
     hall = Hall and 
     ages[Otto] = ages[hall]*2 and
     Otto != Hall
;


data;

param n := 5;
set Firstnames = Keith Libby Margo Nora Otto;
set Lastnames = Fell Gant Hall Ivey Jule;


# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display firstnames, lastnames, ages;

