/*

  Huey, Dewey and Louie problem in SWI Prolog

  From Marriott & Stuckey, Programming with Constraints, page 42
  """
  Huey, Dewey and Louie are being questioned by their uncle. These are the 
  statements the make:
   Huey: Dewey and Louie has equal share in it; if one is quitly, so
         is the other.
   Dewey: If Huey is guilty, then so am I.
   Louie: Dewey and I are not both quilty.
  
  Their uncle, knowing that they are cub scouts, realises that they
  cannot tell a lie. Has he got sufficient information to decide who 
  (if any) are quilty?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        huey_dewey_louie(L),
        writeln(L),nl.

huey_dewey_louie(L) :-

        L = [Huey,Dewey,Louie],
        L ins 0..1,

        %% Huey: Dewey and Louie has equal share in it; if one is 
        %% quitly, so is the other.
        ((Dewey #=1) #<==> (Louie #=1)),
  
        %%  Dewey: If Huey is guilty, then so am I.
        (Huey #=1 #==> Dewey #=1),

        %%  Louie: Dewey and I are not both quilty.
        #\(Dewey #=1  #/\ Louie #=1),

        label(L).
