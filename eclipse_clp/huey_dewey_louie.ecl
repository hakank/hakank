/*

  Huey, Dewey and Louie problem in ECLiPSe.

  From Marriott & Stucket, Programming with Constraints, page 42
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
 
  Compare with the following models:
  * http://www.hakank.org/minizinc/huey_dewey_louie.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/huey_dewey_louie.pl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(listut).
%:-lib(propia).

go :-
        findall(L,huey_dewey_louie(L),List),
        write(List),nl.

huey_dewey_louie(L) :-
        L = [Huey,Dewey,Louie],
        L :: 0..1,

        %  Huey: Dewey and Louie has equal share in it; if one is 
        % quitly, so is the other.
        ((Dewey #=1) #= (Louie #=1)),
  
        %  Dewey: If Huey is guilty, then so am I.
        (Huey #=1 => Dewey #=1),

        %  Louie: Dewey and I are not both quilty.
        (neg(Dewey #=1  and Louie #=1)),

        labeling(L).
