/*

  Generating all spellings of Henning Mankell (and Kjellerstrand) in
  in SICStus Prolog.

  This is a recurring problem for me: Generating "all" possible
  spellings of Henning Mankell, and Kjellerstrand given a grammar (or
  regular expression).

  I have written about this before:
  - Regular expressions in Gecode
    http://www.hakank.org/constraint_programming_blog/2009/04/regular_expressions_in_gecode.html
    This blog post contains further links and references.

  - Icon program for the Henning Mankell problem:
    http://www.hakank.org/unicon/pattern_generation.icn

  Compare with the following model:
  * ECLiPSe: http://www.hakank.org/eclipse/mankell.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        mankell,
        kjellerstrand.


%
% The Henning Mankell problem.
%
% Given this regular expression, generate all the
% possible spellings:
%  [hm][ea](nk|n|nn)(ing|ell|all)
% 
smankell --> hm, ea, nknnm, ingellall.
hm --> [h].
hm --> [m].
ea --> [e].
ea --> [a].
nknnm --> [nk].
nknnm --> [n].
nknnm --> [nn].
ingellall --> [ing].
ingellall --> [ell].
ingellall --> [all].


mankell :-
        showall(smankell).


%
% This regular expression is for (most of) the misspellings
% of my last name, which actually is Kjellerstrand.
%
%    k(je|ä)ll(er|ar)?(st|b)r?an?d 
%
% skjellerstrand --> [k], je, [ll], erar, stb, r_star, [a], n_star, [d].
% je      --> [je] | ["Ã¤"].
% erar    --> [] | [er] | [ar].
% stb     --> [st] | [b].
% r_star  --> [] | [r].
% n_star  --> [] | [n].
% d       --> [d]. 

% This is more like the original regular expression. And that may be
% good or bad...
skjellerstrand --> 
        [k], ([je]|["ä"]), [ll], ([] | [er] | [ar]), 
        ([st] | [b]),
        ([] | [r]), [a], ([] | [n]), [d].

kjellerstrand :-
        showall(skjellerstrand).
        

%
% show all the generated names for the grammar Grammar.
% 
showall(Grammar) :-
        findall(X, phrase(Grammar, X), L),
        length(L, Len),
        write(len:Len),nl,
        (
            foreach(X, L) 
        do
            write(X),nl
        ).
