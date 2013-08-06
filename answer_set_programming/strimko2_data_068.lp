%
% Strimko set 068
%
#const n = 4.

%
% Streams
% 
% 1,2,2,4,
% 2,1,4,2,
% 3,4,1,3,
% 4,3,3,1
streams(1, 1, 1). % the cell (1,1) belongs to stream 1
streams(1, 2, 2). % the cell (1,2) belongs to stream 2
streams(1, 3, 2). % the cell (1,3) belongs to stream 2
streams(1, 4, 4).
streams(2, 1, 2).
streams(2, 2, 1).
streams(2, 3, 4).
streams(2, 4, 2).
streams(3, 1, 3).
streams(3, 2, 4).
streams(3, 3, 1).
streams(3, 4, 3).
streams(4, 1, 4).
streams(4, 2, 3).
streams(4, 3, 3).
streams(4, 4, 1).

% placed
% 2,2,3,
% 2,3,2,
% 3,3,1
placed(2,2,3).
placed(2,3,2).
placed(3,3,1).
