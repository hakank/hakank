/*

  Maximum flow problem in SWI Prolog

  From Winston "Operations Research", page 420f, 423f
  Sunco Oil example.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

        Cap =  [2,3,3,4,2,1,100],
        Arcs = [[1, 2],
                [1, 3],
                [2, 3],
                [2, 4],
                [3, 5],
                [4, 5],
                [5, 1]],
        max_flow(Arcs,Cap, Flow,Z),        
        writeln(z=Z),
        maplist(writeln,Flow),
        nl.

max_flow(Arcs,Cap, Flow,Z) :-
        flatten(Arcs,ArcsFlatten),
        max_list(ArcsFlatten,N),
        
        max_list(Cap,CapMax),
        new_matrix(N,N,0..CapMax,Flow),
        
        %% To minimize
        matrix_element(Flow,N,1,Z),
        
        %% Ensure that the flow in arcs are within the capacity
        %% foreach(I in 1..NumArcs) Flow[Arcs[I,1], Arcs[I,2]] #=< Cap[I] end,
        maplist(arcs_constraint(Flow),Arcs,Cap),

        %% Flow In #= Flow Out
        % foreach(I in Nodes)
        %   sum([Flow[Arcs[K,1], Arcs[K,2]] : K in 1..NumArcs,Arcs[K,1] == I])
        %   #=
        %   sum([Flow[Arcs[K,1], Arcs[K,2]] : K in 1..NumArcs,Arcs[K,2] == I])
        % end,
        numlist(1,N,Nodes),
        maplist(flow(Flow,Arcs),Nodes),

        flatten(Flow,Vars),
        labeling([max(Z)], Vars).
        

%%
%% Ensure that the flow in arcs are within the capacity
%%
arcs_constraint(Flow,[A1,A2],Cap) :-
        matrix_element(Flow,A1,A2,C),
        C #=< Cap.
        
%% Flow In #= Flow Out
% foreach(I in Nodes)
%   sum([Flow[Arcs[K,1], Arcs[K,2]] : K in 1..NumArcs,Arcs[K,1] == I])
%   #=
%   sum([Flow[Arcs[K,1], Arcs[K,2]] : K in 1..NumArcs,Arcs[K,2] == I])
% end,
flow(Flow,Arcs,I) :-
        sum_flow(Arcs,I,1,Flow,0,Flow1),
        sum_flow(Arcs,I,2,Flow,0,Flow2),
        Flow1 #= Flow2.

sum_flow([],_I,_Type,_Flow,Sum,Sum).
sum_flow([[Arc1,Arc2]|Arcs],I,Type,Flow,Sum0,Sum) :-
        B in 0..1,
        matrix_element(Flow,Arc1,Arc2,M),
        (
         % Arcs[K,1] == I
         Type == 1 -> 
         Arc1 #= I #<==> B #= 1
         ;
           % Arcs[K,2] == I
           Arc2 #= I #<==> B #= 1
         ),
        Sum1 #= Sum0 + B*M,
        sum_flow(Arcs,I,Type,Flow,Sum1,Sum).
