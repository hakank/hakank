/*

  Babysitting puzzle (Dell Logic Puzzles) in SWI Prolog

  """
  Title: Babysitting
  Author: Scott Marley
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 7
  Stars: 1

  Each weekday, Bonnie takes care of five of the neighbors' children. 
  The children's names are Keith, Libby, Margo, Nora, and Otto; last 
  names are Fell, Gant, Hall, Ivey, and Jule. Each is a different
  number of years old, from two to six. Can you find each child's 
  full name and age?

  1. One child is named Libby Jule.
  2. Keith is one year older than the Ivey child, who is one year 
     older than Nora.
  3. The Fell child is three years older than Margo.
  4. Otto is twice as many years old as the Hall child.

  Determine: First name - Last name - Age 
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
   
   N = 5,

   Keith = 1, 
   Libby = 2, 
   Margo = 3, 
   Nora  = 4, 
   Otto  = 5,
   First = [Keith, Libby, Margo, Nora, Otto],

   Last  = [Fell, _Gant, Hall, Ivey, Jule],
   Last ins 1..N,

   length(Age,N),
   Age ins 2..6,

   all_different(Last),
   all_different(Age),

   % 1. One child is named Libby Jule.
   Libby #= Jule,
   % 2. Keith is one year older than the Ivey child, who is one year 
   %    older than Nora.
   element(Ivey, Age, AgeIvey),
   element(Keith,Age,AgeKeith),
   element(Nora,Age,AgeNora),
   AgeKeith #= AgeIvey + 1,
   AgeIvey #= AgeNora + 1,

   % 3. The Fell child is three years older than Margo.
   element(Fell, Age, AgeFell),
   element(Margo,Age,AgeMargo),
   AgeFell #= AgeMargo + 3,

   % 4. Otto is twice as many years old as the Hall child.
   element(Hall,Age,AgeHall),
   element(Otto, Age, AgeOtto),
   AgeOtto #= AgeHall*2,

   % search
   flatten([First,Last,Age], Vars),
   label(Vars),

   writeln(first=First),
   writeln(last=Last),
   writeln(age=Age),
   nl,
   
   % print solution
   FirstS = ["Keith", "Libby", "Margo", "Nora", "Otto"],
   LastS  = ["Fell", "Gant", "Hall", "Ivey", "Jule"],
   pretty_print(First,FirstS),
   pretty_print(Last,LastS),
   pretty_print([1,2,3,4,5], Age),
   nl.


%%
%% Pretty print solution
%%
pretty_print(X,S) :-
        length(X,Len),
        findall(E,
                (
                 between(1,Len,I),
                 nth1(J,X,I),
                 nth1(J,S,E)
                ),
                Sol),
        format("~t~w~12|~t~w~23|~t~w~37|~t~w~47|~t~w~64|~n",Sol).

