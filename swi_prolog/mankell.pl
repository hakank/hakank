/*

  Generating all spellings of Henning Mankell (and Kjellerstrand) in SWI Prolog

  This is a recuring problem for me: Generating "all" possible
  spellings of Henning Mankell, and Kjellerstrand given a grammar (or
  regular expression).

  I have written about this before:
  - Regular expressions in Gecode
    http://www.hakank.org/constraint_programming_blog/2009/04/regular_expressions_in_gecode.html
    This blog post contains further links and references.

  - Icon program for the Henning Mankell problem:
    http://www.hakank.org/unicon/pattern_generation.icn



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

go :-
        writeln("Mankell"),
        mankell,
        nl,
        writeln("Kjellerstrand"),
        kjellerstrand,
        nl,
        writeln("Kjellerstrand2"),
        kjellerstrand,
        nl.

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

% This is more like the original regular expression. And that may be
% good or bad...
skjellerstrand --> 
        [k], ([je]|["ä"]), [ll], ([] | [er] | [ar]), 
        ([st] | [b]),
        ([] | [r]), [a], ([] | [n]), [d].

kjellerstrand :-
        showall(skjellerstrand).


%
% Alternative version.
%
skjellerstrand2 --> [k], je, [ll], erar, stb, r_star, [a], n_star, [d].
je      --> [je] | ["ä"].
erar    --> [] | [er] | [ar].
stb     --> [st] | [b].
r_star  --> [] | [r].
n_star  --> [] | [n].
d       --> [d]. 


kjellerstrand2 :-
        showall(skjellerstrand2).


%
% show all the generated names for the grammar Grammar.
% 
showall(Grammar) :-
        findall(S, (phrase(Grammar, X),atomics_to_string(X,S)), L),
        length(L, Len),
        writeln(len=Len),
        maplist(writeln,L),
        nl.