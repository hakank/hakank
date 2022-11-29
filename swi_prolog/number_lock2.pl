/*

  Number lock problem in SWI Prolog

  From Presh Talwalkar (MindYourDecisions) 
  """
  Puzzles like this have been shared with the dubious claim that "only a
  genius can solve" them. But they are still fun problems so let's work one
  out.

  A number lock requires a 3 digit code. Based on these hints, can you crack
  the code?

    682 - one number is correct and in the correct position
    645 - one number is correct but in the wrong position
    206 - two numbers are correct but in the wrong positions
    738 - nothing is correct
    780 - one number is correct but in the wrong position

  Video:  https://youtu.be/-etLb-8sHBc
  """

  Today Moshe Vardi published a related problem (https://twitter.com/vardi/status/1164204994624741376 )
  where all hints, except for the second, where identical with Presh's problem:

    682 - one number is correct and in the correct position
    614 - one number is correct but in the wrong position    <-- This has different digits.
    206 - two numbers are correct but in the wrong positions
    738 - nothing is correct
    780 - one number is correct but in the wrong position


  In go/0 we solve the two puzzles,
  In go2/0 we generate new hints that can replace the second hint (with a unique solutions).

  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


%
% Test the given puzzles.
% Note that we test for unicity of the solutions.
%
go :-
        between(1,5,P),
        writeln(problem=P),
        data(P,Data),
        maplist(writeln,Data),
        findall(X, number_lock(Data,X),L),
        writeln("Answer"=L),    
        nl,
        fail.

go.

%
% Generate new puzzles, i.e.
% generate a new variant of digits instead of the second hint:
%
%   [[6,4,5],0,1], % - one number is correct but in the wrong position
% or 
%   [[6,1,4],0,1], % - one number is correct but in the wrong position
%
% This might give a new solution (X) as well.
%
% A new puzzle is thus:
% Data = [
%   [[6,8,2],1,1], % - one number is correct and in the correct position
%   %% We will replace these digits:
%   %% [[6,4,5],0,1], % - one number is correct but in the wrong position    
%   [[2,0,6],0,2], % - two numbers are correct but in the wrong positions
%   [[7,3,8],0,0], % - nothing is correct
%   [[7,8,0],0,1]  % - one number is correct but in the wrong position
% ],
% PLUS
%  one of the new hints.
%
% According to model, there are 248 possible new hints.
% Some examples:
% [digits = [1,3,3],x = [0,1,2]]
% [digits = [1,3,6],x = [0,1,2]]
% 
% [digits = [1,1,4],x = [0,4,2]]
% [digits = [3,1,4],x = [0,4,2]]
%
% [digits = [1,1,5],x = [0,5,2]]
% [digits = [3,1,5],x = [0,5,2]]
%
% [digits = [1,1,9],x = [0,9,2]]
% [digits = [3,1,9],x = [0,9,2]]
% 
%
% Interestingly, there are only four possible values of X (solution to the puzzle):
%  - 012
%  - 042
%  - 052
%  - 092
%
%
go2 :-
        length(NewDigits,3),
        NewDigits ins 0..9,
        %% Suggest a new hint
        number_lock_generate(NewDigits, _X),
        
        %% Check if the new hint has a unique solution
        findall(NewDigits=X2,number_lock_generate(NewDigits,X2),L2),
        length(L2,L2Len),
        (
         L2Len == 1
        ->
         L2 = [Digits=X2],
         writeln([digits=Digits,x=X2])
        ;
         true
        ),
        fail,
        nl.

go2.


%%
%% number_lock(Data,X)
%%
%% The main problem.
%%
number_lock(Data, X) :-

        % number of digits
        nth1(1,Data,Ds),
        nth1(1,Ds,D),
        length(D,N),
        
        length(X,N),
        X ins 0..9,
        maplist(check_hints(X),Data),
        labeling([],X).


check_hints(X,[Digits,NumCorrectPosition,NumCorrectNumber]) :-
        check(Digits,X,NumCorrectPosition,NumCorrectNumber).

%%
%% How many 
%%   pos: correct values and positions
%%   val: correct values (regardless if there are correct position or not)
%%
check(A, B, Pos, Val) :-
        length(A,N),
                
        %% number of entries in correct position (and correct values)
        maplist(eq_fd,A,B,Zs),
        sum(Zs,#=,Pos),

        %% number of entries which has correct values
        %% (regardless if there are in correct position or not)
        sum_values(N,A,B,Val).
        
eq_fd(A,B,Z) :-
        Z in 0..1,
        A #= B #<==> Z #= 1.


%%
%% number of entries which has correct values
%% (regardless if there are in correct position or not)
%%
sum_values(N,A,B,Val) :-
        findall([I,J],(between(1,N,I),
                       between(1,N,J)
                      ),
                IJs),
        sum_values_(IJs,A,B,0,Val).
sum_values_([],_A,_B,Sum,Sum).
sum_values_([[I,J]|IJs],A,B,Sum0,Sum) :-
        element(I,A,AI),
        element(J,B,BJ),
        Bool in 0..1,
        AI #= BJ #<==> Bool #= 1,
        Sum1 #= Sum0 + Bool,
        sum_values_(IJs,A,B,Sum1,Sum).


%%
%% number_lock_generate(NewDigits, X)
%% 
%% Generate a new hint to replace the second hint in the two puzzles.
% See go2/0.
% %
number_lock_generate(NewDigits, X) :-

        N = 3,                  % number of digits
  
        length(X,N),
        X ins 0..9,

        Data = [
                [[6,8,2],1,1], % - one number is correct and in the correct position
                %% We will replace these three digits
                %% [[6,4,5],0,1], % - one number is correct but in the wrong position    
                [[2,0,6],0,2], % - two numbers are correct but in the wrong positions
                [[7,3,8],0,0], % - nothing is correct
                [[7,8,0],0,1] % - one number is correct but in the wrong position
               ],

        maplist(check_hints(X),Data),
        
        check(NewDigits,X,0,1),
  
        flatten([X,NewDigits],Vars),
        labeling([],Vars).



%
% Data
%

/*
  From Presh Talwalkar (MindYourDecisions) 
  """
  Puzzles like this have been shared with the dubious claim that "only a
  genius can solve" them. But they are still fun problems so let's work one
  out.

  A number lock requires a 3 digit code. Based on these hints, can you crack
  the code?

    682 - one number is correct and in the correct position
    645 - one number is correct but in the wrong position
    206 - two numbers are correct but in the wrong positions
    738 - nothing is correct
    780 - one number is correct but in the wrong position

  Video:  https://youtu.be/-etLb-8sHBc
  """
*/
data(1,Data) :-
  Data = [
    [[6,8,2],1,1], % - one number is correct and in the correct position
    [[6,4,5],0,1], % - one number is correct but in the wrong position    
    [[2,0,6],0,2], % - two numbers are correct but in the wrong positions
    [[7,3,8],0,0], % - nothing is correct
    [[7,8,0],0,1]  % - one number is correct but in the wrong position
  ].


/*
  Moshe Vardi: https://twitter.com/vardi/status/1164204994624741376

    682 - one number is correct and in the correct position
    614 - one number is correct but in the wrong position    <-- This has different digits
    206 - two numbers are correct but in the wrong positions
    738 - nothing is correct
    780 - one number is correct but in the wrong position
  
*/
data(2,Data) :-
  Data = [
    [[6,8,2],1,1], % - one number is correct and in the correct position
    [[6,1,4],0,1], % - one number is correct but in the wrong position    
    [[2,0,6],0,2], % - two numbers are correct but in the wrong positions
    [[7,3,8],0,0], % - nothing is correct
    [[7,8,0],0,1]  % - one number is correct but in the wrong position
  ].



/*
  https://puzzling.stackexchange.com/questions/97032/5-digit-puzzle-code-looking-for-solution
  """
  Can somebody help me solve this, or can you teach me how?

  4 7 2 9 1 - One number is correct but not in right position
  9 4 6 8 7 - One number is correct but not in right position
  3 1 8 7 2 - Two numbers are correct but only one is in right position
  1 5 7 3 9 - Two numbers are correct and both in right position
  """

  Also see: 
  https://g-ar.github.io/posts/solving-mastermind-like-problems-using-z3-theorem-prover/

  Note: It has two solutions:
    [1,5,8,0,0]
    [6,5,0,3,2]

  If we assume distinctness of the numbers then the answer is
    [6,5,0,3,2]

*/
% This is also modelled in number_lock_5_digits.pi
data(3,Data) :-
  Data = [
    [[4,7,2,9,1],0,1], % - One number is correct but not in right position
    [[9,4,6,8,7],0,1], % - One number is correct but not in right position
    [[3,1,8,7,2],1,2], % - Two numbers are correct but only one is in right position
    [[1,5,7,3,9],2,2]  % - Two numbers are correct and both in right position
  ].


/* 
  From https://twitter.com/sonukg4india/status/1591081634534936576
  """
  A padlock has a 4-digit key code.

  2657: Has two correct digits but neither are in the correct place
  0415: Has one correct digit but it's in the wrong place
  4268: Has no correct digitss.
  1749: Has two correct digits, both in the correct places.
  All the 4 digits in the key are different. 
  What is the code for the padlock?
  """   

*/
data(4,Data) :-
  Data = [
    [[2,6,5,7],0,2], % - Two numbers are correct but not in right position
    [[0,4,1,5],0,1], % - One number is correct but not in right position
    [[4,2,6,8],0,0], % - No correct digit
    [[1,7,4,9],2,2]  % - Two numbers are correct and both in right position
  ].


/* 
  From https://twitter.com/sonukg4india/status/1591343310505115648
  """
  Can you crack the code

  795  One number is correct and well placed
  741  One number is correct but wrong place
  463  Two numbers are correct but in wrong place
  127  Nothing is correct
  169  One number is correct but wrong place
  """

*/
data(5,Data) :-
  Data = [
    [[7,9,5],1,1],
    [[7,4,1],0,1],
    [[4,6,3],0,2],
    [[1,2,7],0,0],
    [[1,6,9],0,1] 
  ].

