/*

   Schoolgirl logic puzzle in SWI Prolog

   From  ECLiPSs mailing list:
   http://groups.google.se/group/comp.lang.prolog/browse_thread/thread/119c52c1c22023b0/48b0ff5eb514e5d1?lnk=st&q=eclipse++%22lib(ic%22)&rnum=6&hl=sv#48b0ff5eb514e5d1

  """
  Five schoolgirls sat for an examination. Their parents -- so they
  thought -- showed an undue degree of interest in the result. They
  therefore agreed that, in writing home about the examination, each girl
  should make one true statement and one untrue one. The following are
  the relevant passages from their letters:
  
  * Betty: ``Kitty was second in the examination. I was only third.''
  * Ethel: ``You'll be glad to hear that I was on top. Joan was second.''
  * Joan: ``I was third, and poor old Ethel was bottom.''
  * Kitty: ``I came out second. Mary was only fourth.''
  * Mary: ``I was fourth. Top place was taken by Betty.''
  
  What in fact was the order in which the five girls were placed? 
  """

  solution/1 is a port of a ECLiPSe model, with tiny changes from the original.
  solution2/1 is - however - my own.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

:- op(710, xfy, xor).
:- op(720, xfy, and).

%%
%%- solution(Ss).
%% Ss = [1-kitty, 2-joan, 3-betty, 4-mary, 5-ethel]
%%
go :-
        solution(S),
        writeln(S).

%%
%% Using clpfd. Same result.
%%
go2 :-
        solution2(S),
        writeln(S).


true(A is A).
true(A and B) :-
         true(A),
         true(B).
true(A xor B) :-
         true(A),
         false(B).
true(A xor B) :-
         false(A),
         true(B).

false(A is B) :-
         A #\= B.

%%
%% Using xor/2 and and/2.
%% 
solution(Sol) :-
         Girls = [Betty,Ethel,Joan,Kitty,Mary],
         Girls ins 1..5,
         all_different(Girls),
         true(Kitty is 2 xor Betty is 3 and
              Ethel is 1 xor Joan is 2 and
              Joan is 3 xor Ethel is 5 and
              Kitty is 2 xor Mary is 4 and
              Mary is 4 xor Betty is 1),
         keysort([Betty-betty,Mary-mary,Joan-joan,
                  Kitty-kitty,Ethel-ethel], Sol).

%%
%% Using clpfd.
%%
solution2(Sol) :-
         Girls = [Betty,Ethel,Joan,Kitty,Mary],
         Girls ins 1..5,
         all_different(Girls),
         Kitty #= 2 #\ Betty #= 3,
         Ethel #= 1 #\ Joan #= 2,
         Joan #= 3 #\ Ethel #= 5,
         Kitty #= 2 #\ Mary #= 4,
         Mary #= 4 #\ Betty #= 1,
         label(Girls),
         keysort([Betty-betty,Mary-mary,Joan-joan,
                  Kitty-kitty,Ethel-ethel], Sol).
