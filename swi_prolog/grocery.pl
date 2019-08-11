/*

  Grocery problem in SWI Prolog

  Ported from the Gecode example
  """
  A kid goes into a grocery store and buys four items. The cashier
  charges $7.11, the kid pays and is about to leave when the cashier
  calls the kid back, and says "Hold on, I multiplied the four items
  instead of adding them; I'll try again; Hah, with adding them the
  price still comes to $7.11". What were the prices of the four items?

  The model is taken from: Christian Schulte, Gert Smolka, Finite Domain
  Constraint Programming in Oz. A Tutorial. 2001.
  Available from: http://www.mozart-oz.org/documentation/fdt/
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        length(Item,4),
        Item ins 0..711,

        sum(Item, #=,711),
        S #= 711 * 100*100*100,
        prod(Item,S),

        increasing(Item),       % symmetry breaking  
        
        labeling([ffc], Item),
        
        writeln(item=Item),
        nl.

%% variant
go2 :- 
        Vs = [A,B,C,D], 
        Vs ins 0..711,
        A + B + C + D #= 711,
        A * B * C * D #= 711*100*100*100,
        A #=< B,
        B #=< C,
        C #=< D,
        
        labeling([ffc], Vs),
        writeln(Vs).

% mult(X,Y,Z) :- Z #= X*Y.
% prod(L,Product) :-
%         foldl(mult,L,1,Product).