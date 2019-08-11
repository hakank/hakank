/*

  Devil's word in SWI Prolog

  Translate each character in a word to ASCII value and then try
  to sum its values (either positive or negative) to a total.
  
  E.g. "hakankjellerstrand" and total 666 gives 359 solutions.
  Here is the first:
  +104 +97 +107 +97 +110 +107 +106 -101 +108 +108 -101 +114 +115 +116 -114 -97 -110 -100

  See http://www.hakank.org/data_snooping/666.cgi

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        Name = "hakankjellerstrand",
        atom_codes(Name,Res),
        format("~s", Name), 
        nl,
        Total #= 666,
        writeln(total=Total),
        devils_word(Res, SignedRes,Total),
        labeling([ff],SignedRes),
        
        writeln([Total, SignedRes]),
        nl.


%%
%% Let's see how many solutions there are: 359
%%
go2 :-

        Name = "hakankjellerstrand",
        atom_codes(Name,Res),
        format("~s", Name),
        nl,
        Total #= 666,
        findall(SignedRes, (devils_word(Res, SignedRes,Total),
                             labeling([ff],SignedRes)),L),
        length(L,Len),
        format("There are ~d solutions.\n", Len),
        nl.

%%
%% Let us go crazy and set Total free as well.
%% There are 130362 solutions.
%%
go3 :-
        Name = "hakankjellerstrand",
        %%Name = "hakan", 
        format("~s", Name), 
        atom_codes(Name,Res),

        nl,
        maplist(abs,Res,ResAbs),
        sum(ResAbs,#=,AbsSum),
        Total in 1..AbsSum,
        
        findall([Total,SignedRes],
                (devils_word(Res, SignedRes,Total),
                 labeling([ff],SignedRes)),L), 
        maplist(writeln,L),
        length(L,Len),
        format("There are ~d solutions.~n", Len),
        nl.

devils_word(List, SignedRes, Total) :-
        length(List,Len),
        length(Signs,Len),
        Signs ins -1\/1,
        scalar_product(List,Signs,#=,Total),
        maplist(signedres,List,Signs,SignedRes).


signedres(S,E,Y) :-
        Y #= S*E.
