% 
% Scheduling speakers in ASP.
% 
% 
% From Rina Dechter, Constraint Processing, page 72
% Scheduling of 6 speakers in 6 slots.
% 
% This was created by Hakan Kjellerstrand, hakank@gmail.com
% See also http://www.hakank.org/answer_set_programming/
%

% available(Room, Speaker).
                           % Reasoning:             
available(1, 3;4;5;6).     % 2) the only one with 6 after speaker F -> 1
available(2, 3;4).         % 5) 3 or 4
available(3, 2;3;4;5).     % 3) only with 5 after F -> 1 and A -> 6
available(4, 2;3;4).       % 4) only with 2 after C -> 5 and F -> 1 
available(5, 3;4).         % 5) 3 or 4
available(6, 1;2;3;4;5;6). % 1) the only with 1

rooms(1..6).
speakers(1..6).

1 { x(Room, Speaker) : speakers(Speaker) } 1 :- rooms(Room).

%% 1 { x(Room, Speaker) : rooms(Room)  } 1 :- speakers(Speaker).
%% a speaker must be available for this room
%% :- x(Room, Speaker), not available(Room, Speaker).

% Alternative approach:
1 { x(Room, Speaker) : rooms(Room) : available(Speaker, Room) } 1 :- speakers(Speaker).


#hide.
#show x(Room, Speaker).