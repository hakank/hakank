%% From Progol example/append.pl

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Positive examples

%
% hakank: 
% - renamed append/3 to target/3
% - commented some instances.
%       
pos(target([],[],[])).
pos(target([],[a],[a])).
% pos(target([],[b],[b])).
pos(target([],[a,b],[a,b])).
% pos(target([],[a,b,c],[a,b,c])).
pos(target([a],[b],[a,b])).
% pos(target([c],[d],[c,d])).
% pos(target([d],[e],[d,e])).
% pos(target([a],[b,c],[a,b,c])).
% pos(target([b],[c,d],[b,c,d])).
% pos(target([c],[d,e],[c,d,e])).
% pos(target([a],[b,c,d],[a,b,c,d])).
pos(target([c,d],[e,f],[c,d,e,f])).
% pos(target([d,e],[f,g],[d,e,f,g])).
% pos(target([d,e],[f,g,h],[d,e,f,g,h])).
% pos(target([d,e],[f,g,h,i],[d,e,f,g,h,i])).
% pos(target([1,2,3],[4,5,6],[1,2,3,4,5,6])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Negative examples

neg(target([],[b],[a])).
neg(target([a],[],[])).
neg(target([a],[b],[b])).
% neg(target([a],[b],[a])).
neg(target([a],[b],[b,a])).
% neg(target([a],[b],[c,b])).
neg(target([a,b],[c,d],[a,c,d])).
% neg(target([a,c],[c,d],[a,d])).

