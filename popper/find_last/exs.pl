% Find last element

pos(find_last([1,2,3,4],4)).
pos(find_last([2],2)).
pos(find_last([4,3,1],1)).
pos(find_last([1,3],3)).


neg(find_last([1,2,3,4],2)).
neg(find_last([2],8)).
neg(find_last([],2)).
neg(find_last([3,2,4],2)).
neg(find_last([2,3,2,4],2)).

