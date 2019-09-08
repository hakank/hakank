%
% Problem 32
% """
% We shall say that an n-digit number is pandigital if it makes use of 
% all the digits 1 to n exactly once; for example, the 5-digit number, 
% 15234, is 1 through 5 pandigital.
%
% The product 7254 is unusual, as the identity, 39 × 186 = 7254, 
% containing multiplicand, multiplier, and product is 1 through 9 
% pandigital.
%
% Find the sum of all products whose multiplicand/multiplier/product 
% identity can be written as a 1 through 9 pandigital.
% HINT: Some products can be obtained in more than one way so be sure 
% to only include it once in your sum.
% """
%
% Answer: 45228 (0.02s)
% 

:- lib(ic).

go :-
  problem32.

%
% converts a number Num to/from a list of integer List given a base Base
%
toNum2(List, Base, Num) :-
        length(List, Len),    
        length(Xs, Len),
        exp_list2(Len, Base, Xs), % calculate exponents
        Num #= List*Xs,!.
        

%
% Exponents for toNum2: [Base^(N-1), Base^(N-2), .., Base^0],
%    e.g. exp_list2(3, 10, ExpList) -> ExpList = [100,10,1]
%
exp_list2(N, Base, ExpList) :-
        length(ExpList, N),
        (for(I, 0, N-1), fromto([], In, Out, ExpList), param(Base)  do 
            B is Base^I,
            Out = [B|In]
        ).



% This is a variant of my ECLiPSe model using CLP:
% http://www.hakank.org/eclipse/pandigital_numbers.ecl
%
pandigital(Res) :-

        % length of numbers
        Len1 :: 1..2,
        Len2 :: 3..4,
        Len3 #= 4,
        % Ensure that there are 9 digits
        Len1 + Len2 + Len3 #= 9,
        
        % This is used to "drive" the different lengths 
        % of the first number/list.
        indomain(Len1),

        % length of the lists of digits
        length(X1, Len1),
        length(X2, Len2),
        length(X3, Len3), % the result
        [X1,X2,X3] :: 1..9,

        % convert to numbers
        Base = 10,
        toNum2(X1, Base, Num1),
        toNum2(X2, Base, Num2),
        toNum2(X3, Base, Res),

        % ensure the result
        Num1 * Num2 #= Res,

        flatten([X1,X2,X3], Vars),
        ic:alldifferent(Vars),

        % search
        search(Vars,0,first_fail,indomain_max,complete,[]),
        writeln([Num1,Num2,Res]).

% (0.02s)
problem32 :-
        setof(Res, pandigital(Res), L),
        writeln(L),
        sum(L,Sum),
        writeln(Sum).
