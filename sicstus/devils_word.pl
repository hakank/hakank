/*

  Devil's word in SICStus Prolog.

  Translate each character in a word to ASCII value and then try
  to sum its values (either positive or negative) to a total.
  
  E.g. "hakankjellerstrand" and total 666 gives 359 solutions.
  Here is the first:
  +104 +97 +107 +97 +110 +107 +106 -101 +108 +108 -101 +114 +115 +116 -114 -97 -110 -100
 
  Also see 
   * my CGI program "Devil's Word"
     http://www.hakank.org/data_snooping/666.cgi
   * the MiniZinc model http://www.hakank.org/minizinc/devils_words.mzn
   * the Gecode/R model http://www.hakank.org/gecoder/devils_word.rb
   * the ECLiPSe model http://www.hakank.org/eclipse/devils_word.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

%
% Signs is the list of [-1,1] for which
%   List[i]*Signs[i] = Total
%
% SignedRes is the resulting list of 
%   SignedRes[i] = List[i]*Signs[i]
%
devils_word(List, SignedRes, Total) :-
        length(List, Len),
        length(Signs, Len),

        % Allowed are {-1,1}
        domain(Signs,-1,1),
        ( foreach(S,Signs) do
              S #\= 0
        ),
        scalar_product(List,Signs,#=,Total),

        % create the resulting list of ASCII codes with +/- signs
        ( foreach(S, Signs), foreach(R, List), 
          fromto(SignedRes, Out, In,[]) do 
              X #= S*R,
              Out = [X|In]
          ).


% 
% convert a list of atoms to ASCII code
%
convert_list(List, Res) :-
         ( foreach(El,List), fromto(Res,Out, In, []) do 
               char_code(El,I), Out = [I|In]).
        

go :-
        Name = [h,a,k,a,n, k,j,e,l,l,e,r,s,t,r,a,n,d],
        convert_list(Name, Res),
        write(Res),nl,
        Total #= 666,
        devils_word(Res, SignedRes,Total),
        labeling([],SignedRes),
                
        write([Total, SignedRes]),nl.


%
% Let's see how many solutions there are.
%
go2 :-
        Name = [h,a,k,a,n, k,j,e,l,l,e,r,s,t,r,a,n,d],
        convert_list(Name, Res),
        write(Res),nl,
        Total #= 666,
        findall(SignedRes, (devils_word(Res, SignedRes,Total),
                            labeling([ff],SignedRes)
                           ), L),
        length(L, Len),
        format('There are ~d solutions.', [Len]).
