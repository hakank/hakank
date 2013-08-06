/*

  Devil's word in ECLiPSe.
 
  Translate each character in a word to ASCII value and then try
  to sum its values (either positive or negative) to a total.
  
  E.g. "hakankkjellerstrand" and total 666 gives 359 solutions.
  Here is the first:
  +104 +97 +107 +97 +110 +107 +106 -101 +108 +108 -101 +114 +115 +116 -114 -97 -110 -100
 
  Also see 
   * my CGI program "Devil's Word"
     http://www.hakank.org/data_snooping/666.cgi
   * the MiniZinc model http://www.hakank.org/minizinc/devils_words.mzn
   * the Gecode/R model http://www.hakank.org/gecoder/devils_word.rb


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).


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
        Signs :: [-1,1],
        Total #= Signs * List,

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
               iso:char_code(El,I), Out = [I|In]).
        

go :-
        Name = [h,a,k,a,n,k,j,e,l,l,e,r,s,t,r,a,n,d],
        convert_list(Name, Res),
        writeln(Res),
        Total #= 666,
        Total #= devils_word(Res, SignedRes),
        labeling(SignedRes),
                
        writeln([Total, SignedRes]).


%
% Let's see how many solutions there are.
%
go2 :-
        % Name = [h,a,k,a,n,k,j,e,l,l,e,r,s,t,r,a,n,d],
        Name = [h,a,k,a,n, k,j,e,l,l,e,r,s,t,r,a,n,d],
        convert_list(Name, Res),
        writeln(Res),
        Total #= 666,
        findall(SignedRes, (Total #= devils_word(Res, SignedRes),
                            search(SignedRes,0,occurrence,indomain_split,complete,[])
                           ), L),
        length(L, Len),
        printf("There are %d solutions.", [Len]).
