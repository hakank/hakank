% Test of floats

% Floats
pos(target(1.0,1.5,1.5)).
%% pos(target(2.2,2.2,4.84)). %% If this is included it don't work since 2.2*2.2=4.840000000000001
pos(target(2.1,2.4,5.04)).
pos(target(4.32,5.1,22.032)).
pos(target(3.14,3.14,9.8596)).

neg(target(2.2,2.2,1.23)).   

% Integers
% pos(target(1,2,2)).
% pos(target(2,2,4)).
% pos(target(3,3,9)).
% pos(target(2,4,8)).
   