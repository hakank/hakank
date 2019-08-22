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
% Test the two given puzzles.
% Note that we test for unicity of the solutions.
%
go :-
        between(1,2,P),
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
