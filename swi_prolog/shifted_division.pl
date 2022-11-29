/*

  Shifted divsion in SWI Prolog

  https://twitter.com/queued_q/status/1315505135246663682
  """
   7903225806451612       7
   -----------------  =   -
    9032258064516128      8
  """

  This is represented as

    x/y = a/b

  Note that a (here 7) is the first digit of x and b (here 8) is the
  last digit of y. These constraints are what make it interesting problem.

  An "interesting division" is when the number x is without any repetition.


  Compare with my Z3 model http://hakank.org/z3/shifted_division.py which has some 
  more discussions about interestingness etc.
  

  *  N=2..100: 41.3s Tested in go/0.
     labeling([ff,enum],[X,Y,A,B,C]) : 402,562,232 inferences, 41.346 CPU in 41.347 seconds (100% CPU, 9736365 Lips)

     Cf my Z3's shifted_division that took 49.9s.

     Example: 

     n=17
     16666666666666666/66666666666666664 = 1/4
     repeating:66666666:8

     19999999999999999/99999999999999995 = 1/5
     repeating:99999999:8

     23529411764705882/35294117647058823 = 2/3
     Interesting division

     26666666666666666/66666666666666665 = 2/5
     repeating:66666666:8

     47058823529411764/70588235294117646 = 4/6
     Interesting division

     48484848484848484/84848484848484847 = 4/7
     repeating:48484848:8

     49999999999999999/99999999999999998 = 4/8
     repeating:99999999:8

     65454545454545454/54545454545454545 = 6/5
     repeating:54545454:8

     74242424242424242/42424242424242424 = 7/4
     repeating:42424242:8

     87671232876712328/76712328767123287 = 8/7
     repeating:87671232:8

     95294117647058823/52941176470588235 = 9/5
     Interesting division

     See go_interesting/0 below for only the interesting divisions.


  * N=1231: 2min 32.699s Tested in go2/0.
    101,215,441 inferences, 152.699 CPU in 152.732 seconds (100% CPU, 662843 Lips)

    Cf Z3's shifted_division that took 2min43.20s

  * N=12345 Tested in go3/0.

    Got 2 solutions in 1000.548 seconds, then stack limit.
    
    Note that my Z3 shifted_division model does not give an answer at all on this.


  Also see my Picat model http://hakank.org/picat/shifted_division.pi

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/
:- use_module(library(clpfd)).

go :-
    % An interesting division is when there's no repeating digits
    % (defined by the following regexp).
    re_compile("(\\d{2,})\\1",Re,[]),
    between(2,100,N),
    writeln(n=N),
    nl,
    shifted_division(N, [X,Y,A,B]),
    % writeln([x=X,y=Y,a=A,b=B]),
    format('~d/~d = ~d/~d~n',[X,Y,A,B]),
    number_string(X,S),
    (re_matchsub(Re,S,Sub,[]) ->
        SS = Sub.1,
        string_length(SS,SSLen),
        writeln(repeating:SS:SSLen)
    ;
        % It can only be interesting if the number is
        % longer than 4 digit.
        N > 5 ->
            writeln("Interesting division")
        ;
            true
    ),
    nl,
    fail,
    
    nl.

/*
  Only showing the solutions for N=2..100 (i.e. no fancy stuff): 43.6s


  % 402,552,303 inferences, 43.626 CPU in 43.651 seconds (100% CPU, 9227319 Lips)

*/
go_loop_only :-
    between(2,100,N),
    writeln(n=N),
    nl,
    shifted_division(N, [X,Y,A,B]),
    format('~d/~d = ~d/~d~n',[X,Y,A,B]),
    fail,
    
    nl.


/*
  Only show the interesting divisions
    x/y = a/b
 i.e. those that does not contain any repetitions in x.

  6: 987804/878048 = 9/8
  7: 1428571/4285713 = 1/3
  7: 2857142/8571426 = 2/6
  7: 3461538/4615384 = 3/4
  7: 4102564/1025641 = 4/1
  7: 4571428/5714285 = 4/5
  7: 5952380/9523808 = 5/8
  7: 6428571/4285714 = 6/4
  7: 6923076/9230768 = 6/8
  7: 7538461/5384615 = 7/5
  7: 8205128/2051282 = 8/2
  7: 8311688/3116883 = 8/3
  9: 876712328/767123287 = 8/7
 14: 67924528301886/79245283018867 = 6/7
 14: 81012658227848/10126582278481 = 8/1
 16: 7903225806451612/9032258064516128 = 7/8
 17: 23529411764705882/35294117647058823 = 2/3
 17: 47058823529411764/70588235294117646 = 4/6
 17: 95294117647058823/52941176470588235 = 9/5
 19: 2105263157894736842/1052631578947368421 = 2/1
 19: 4210526315789473684/2105263157894736842 = 4/2
 19: 6315789473684210526/3157894736842105263 = 6/3
 19: 8421052631578947368/4210526315789473684 = 8/4
 22: 5813953488372093023255/8139534883720930232557 = 5/7
 22: 9418604651162790697674/4186046511627906976744 = 9/4
 23: 39130434782608695652173/91304347826086956521737 = 3/7
 23: 54347826086956521739130/43478260869565217391304 = 5/4
 23: 71014492753623188405797/10144927536231884057971 = 7/1
 29: 31034482758620689655172413793/10344827586206896551724137931 = 3/1
 29: 62068965517241379310344827586/20689655172413793103448275862 = 6/2
 29: 93103448275862068965517241379/31034482758620689655172413793 = 9/3
 34: 7313432835820895522388059701492537/3134328358208955223880597014925373 = 7/3
 42: 975903614457831325301204819277108433734939/759036144578313253012048192771084337349397 = 9/7
 43: 5102040816326530612244897959183673469387755/1020408163265306122448979591836734693877551 = 5/1
 45: 910112359550561797752808988764044943820224719/101123595505617977528089887640449438202247191 = 9/1
 47: 53191489361702127659574468085106382978723404255/31914893617021276595744680851063829787234042553 = 5/3
 59: 61016949152542372881355932203389830508474576271186440677966/10169491525423728813559322033898305084745762711864406779661 = 6/1

*/ 
go_interesting :-
    % An interesting division is when there's no repeating digits
    % (defined by the following regexp).
    re_compile("(\\d{2,})\\1",Re,[]),
    between(5,100,N),
    shifted_division(N, [X,Y,A,B]),
    % writeln([x=X,y=Y,a=A,b=B]),
    number_string(X,S),
    (re_match(Re,S) ->
        true
    ;
        format('~d: ~d/~d = ~d/~d~n',[N, X,Y,A,B])
    ),
    fail,
    nl.

/*
  Printing all 23 solutions for 1231 digit numbers.

  101,215,441 inferences, 152.699 CPU in 152.732 seconds (100% CPU, 662843 Lips)

*/
go2 :-
        N = 1231,
        shifted_division(N, [X,Y,A,B]),
        writeln([X,Y,A,B]),
        fail,
        
        nl.

/*
   After 1000s it has solved two instances
   199...99,99..5
   and
   166..6,66..4
   and then it stopped with
   144,307,765 inferences, 1000.537 CPU in 1000.548 seconds (100% CPU, 144230 Lips)
   ERROR: Stack limit (1.0Gb) exceeded

   Time to find the first solution: 
   % 79,598,205 inferences, 399.911 CPU in 399.917 seconds (100% CPU, 199040 Lips)


   Note that Z3 couldn't handle this size at all.

*/
go3 :-
        N = 12345,
        shifted_division(N, [X,Y,A,B]),
        writeln([X,Y,A,B]),
        % fail,        
        nl.


shifted_division(N, [X,Y,A,B]) :-
        N1 is N-1,
        N2 is N-2,
        CMin is 10^(N2),
        CMax is 10^(N1)-1,
        C in CMin..CMax,        % The common number
        XYMin is 10^(N1),
        XYMax is 10^N-1,
        [X,Y] ins XYMin..XYMax,
        [A,B] ins 0..9,

        X #= C + A*10^(N1),
        Y #= 10*C + B,
        
        X*B #= A*Y,
        X #\= Y,
        % Timing for N=2..100
        % labeling([ffc,up,bisect],[X,Y,A,B,C]). % 660,277,452 inferences, 88.431 CPU in 88.598 seconds (100% CPU, 7466572 Lips)
        labeling([ff,enum],[X,Y,A,B,C]). % 402,552,273 inferences, 44.291 CPU in 44.497 seconds (100% CPU, 9088888 Lips)
        % labeling([ffc,enum],[X,Y,A,B,C]). % 402,576,193 inferences, 44.328 CPU in 44.420 seconds (100% CPU, 9081817 Lips)
