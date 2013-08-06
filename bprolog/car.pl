/*

  Car sequencing in B-Prolog.

  Based on the OPL3 model car.mod.

  This model is based on the car sequencing model in
  Pascal Van Hentenryck
  'The OPL Optimization Programming Language', page 184ff.
  (Via other implementations:
     http://www.hakank.org/google_or_tools/car.py
     http://www.hakank.org/minizinc/car.mzn
  )

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        
        NbCars = 6,
        Cars = 1..NbCars,
        NbOptions = 5,
        Options = 1..NbOptions,
        NbSlots = 10,
        Slots = 1..NbSlots,
        Demand = [1, 1, 2, 2, 2, 2],
        Option = [[1, 0, 0, 0, 1, 1],
                  [0, 0, 1, 1, 0, 1],
                  [1, 0, 0, 0, 1, 0],
                  [1, 1, 0, 1, 0, 0],
                  [0, 0, 1, 0, 0, 0]],
        Capacity = [[1,2],
                    [2,3],
                    [1,3],
                    [2,5],
                    [1,5]],

        %% This don't work:
        % OptionDemand @= [sum([Demand[J]*Option[I,J] : J in Cars]) : I in Options],
        % Have to use an accumulator ac/2
        foreach(I in Options,ac(OptionDemandTmp,[]),[OD],
                (
                    OD #= sum([ Demand[J]*Option[I,J] : J in Cars]),
                    OptionDemandTmp^1 = [OD|OptionDemandTmp^0]
                )
               ),
        reverse(OptionDemandTmp,OptionDemand),

        %
        % decision variables
        %
        length(Slot,NbSlots),
        Slot :: 1..NbCars,

        new_array(Setup,[NbOptions,NbSlots]),
        array_to_list(Setup,SetupVars),
        SetupVars :: 0..1,

        % To minimize
        Z #= sum([S*Slot[S] : S in Cars]),

        %
        % Constraints
        %
        foreach(C in Cars,
                sum([(Slot[S] #= C) : S in Slots]) #= Demand[C]
               ),
        foreach(O in Options, 
                S in 1..NbSlots - Capacity[O,2] + 1,              
                sum([Setup[O,J] : J in S..S + Capacity[O,2]- 1]) #=< Capacity[O,1]
               ),

        %% This don't work
        % foreach(O in Options, S in Slots,
        %         Setup[O,S] #= Option[O,Slot[S]]
        %        ),
        %% Instead one has to use a couple of element/3.
        foreach(O in Options, S in Slots,
                [SlotS,SS],
                (element(S,Slot,SlotS),
                 matrix_element(Option,O,SlotS,SS),
                 Setup[O,S] #= SS
                )),

        foreach(O in Options, I in 1..OptionDemand[O],
                (
                    sum([Setup[O,S] : S in 1..(NbSlots - I * Capacity[O,2])]) #>=
                    (OptionDemand[O] - I * Capacity[O,1])
                )
              ),

        term_variables([Slot,SetupVars], Vars),
        minof(labeling(Vars),Z),

        writeln(z:Z),
        writeln(slot:Slot),
        SetupRows @= Setup^rows,
        foreach(Row in SetupRows, writeln(Row)),

        nl.

        
matrix_element(X, I, J, Val) :-
        element(I, X, Row),
        element(J, Row, Val).
