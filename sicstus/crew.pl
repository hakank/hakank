/*

  Crew allocation problem in SICStus Prolog.

  From Gecode example crew
  examples/crew.cc
  """ 
  (Original text from crew.cc)
  * Example: Airline crew allocation
  *
  * Assign 20 flight attendants to 10 flights. Each flight needs a certain
  * number of cabin crew, and they have to speak certain languages.
  * Every cabin crew member has two flights off after an attended flight.
  *
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/crew.mzn
  * Comet   : http://www.hakank.org/comet/crew.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        NumPersons = 20, % number of persons
        Attributes =
        % steward, hostess, french, spanish, german
        [[1,0,0,0,1],   % Tom     = 1
         [1,0,0,0,0],   % David   = 2
         [1,0,0,0,1],   % Jeremy  = 3
         [1,0,0,0,0],   % Ron     = 4
         [1,0,0,1,0],   % Joe     = 5
         [1,0,1,1,0],   % Bill    = 6
         [1,0,0,1,0],   % Fred    = 7
         [1,0,0,0,0],   % Bob     = 8
         [1,0,0,1,1],   % Mario   = 9
         [1,0,0,0,0],   % Ed      = 10

         [0,1,0,0,0],   % Carol   = 11
         [0,1,0,0,0],   % Janet   = 12
         [0,1,0,0,0],   % Tracy   = 13
         [0,1,0,1,1],   % Marilyn = 14
         [0,1,0,0,0],   % Carolyn = 15
         [0,1,0,0,0],   % Cathy   = 16
         [0,1,1,1,1],   % Inez    = 17
         [0,1,1,0,0],   % Jean    = 18
         [0,1,0,1,1],   % Heather = 19
         [0,1,1,0,0]    % Juliet  = 20
        ],
        
        NumFlights = 10,

        /* 
        stewards = {Tom, David, Jeremy, Ron, Joe, Bill, Fred, Bob, Mario, Ed};
        hostesses = {Carol, Janet, Tracy, Marilyn, Carolyn, Cathy, Inez,
                     Jean, Heather, Juliet};
        frenchSpeaking = {Bill, Inez, Jean, Juliet};
        germanSpeaking = {Tom, Jeremy, Mario, Cathy, Juliet};
        spanishSpeaking = {Joe, Bill, Fred, Mario, Marilyn, Inez, Heather};
        */

        /* required crew per flight 
        The columns are in the following order
        staff:     Overall number of cabin crew needed
        stewards:  How many stewards are required
        hostesses: How many hostesses are required
        french:    How many French speaking employees are required
        spanish:   How many Spanish speaking employees are required
        german:    How many German speaking employees are required
        */
        RequiredCrew = [[4, 1,1,1,1,1], % Flight 1
                        [5, 1,1,1,1,1], % Flight 2
                        [5, 1,1,1,1,1], % ..
                        [6, 2,2,1,1,1],
                        [7, 3,3,1,1,1],
                        [4, 1,1,1,1,1],
                        [5, 1,1,1,1,1],
                        [6, 1,1,1,1,1],
                        [6, 2,2,1,1,1],
                        [7, 3,3,1,1,1]  % Flight 10
                       ],

        % split this table
        ( foreach(R,RequiredCrew),
          fromto(TotalRequired,[Tot|In1],In1,[]),
          fromto(Required,[Rest|In2],In2,[]) do
              R = [Tot|Rest]
        ),

        % the crew schedule
        matrix(Crew,[NumFlights, NumPersons]),
        append(Crew,CrewList),
        domain(CrewList,0,1),

        % objective to minimize: 
        % total number of persons working
        transpose(Crew,CrewTransposed),
        ( foreach(C1,CrewTransposed),
          fromto(0,In,Out,Z) do
              sum(C1,#=,This),
              Reif in 0..1,
              This #> 0 #<=> Reif #= 1,
              Out #= In + Reif
         ),


        % calculate the persons for a specific flight
        % that has the proper requirements.
        % Also the number of persons for a flight.
        transpose(Attributes,AttributesTransposed),
        ( foreach(Flight,Crew),
          foreach(Tot2,TotalRequired),
          foreach(Req,Required),
          param(AttributesTransposed) do
              % number of persons for a flight
              sum(Flight,#=,Tot2),

              % require the number of persons with 
              % the proper attributes
              ( foreach(R,Req),
                foreach(Attribute,AttributesTransposed),
                param(Flight) do
                    scalar_product(Attribute,Flight,#>=,R)
              )
        ),

        % after a flight, break for two flights, i.e.
        % for three consecutive flights there can be
        % work for max 1 of these flights.
        ( foreach(C,CrewTransposed) do
              consecutive_sum_to_less_than(C,1)
        ),
        

        % extra contraint: all persons (if at all) 
        % must work at least two times
        ( foreach(C1,Crew) do
              sum(C1,#>=,2)
        ),
        
        % search
        append(CrewList,[Z],Vars),
        labeling([ff,bisect,up], Vars),
        % It seems to be hard do minimize this problem
        % labeling([ff,bisect,up,minimize(Z)], CrewList),

        % output
        format('Number of persons working: ~d\n',[Z]),
        ( foreach(Flight,Crew),
          count(F,1,_) do
              format('Flight ~d: ~w\n', [F,Flight])
        ),
        nl,
        write('Another representation'),nl,
        ( foreach(Flight,Crew),
          foreach(Req, RequiredCrew),
          count(F,1,_) do
              boolean_to_set(Flight,CrewMembers),
              format('Flight ~d: Required: ~w Crew: ~w\n', [F,Req,CrewMembers])
        ),
        nl,
        write('Personell working at flights:'),nl,
        ( foreach(Person,CrewTransposed),
          count(P,1,_) do
              boolean_to_set(Person,PFlights),
              format('Person ~d Flights ~w\n', [P, PFlights])
        ),
          

        nl,
        fd_statistics.
        


boolean_to_set(List,Set) :-
        ( foreach(C,List),
          count(I,1,_),
          fromto(Set,Out,In,[]) do
              C == 1 
        ->
          Out = [I|In]
        ;
          Out = In
        ).

% require that all three consecutive numbers
% sums to =< Sum
consecutive_sum_to_less_than(X,Sum) :-
        ( fromto(X, [This,Next1,Next2|Rest], [Next1,Next2|Rest],[_,_]),
          param(Sum) do
              This + Next1 + Next2 #=< Sum
        ).
        

matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson.
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).

