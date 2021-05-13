% Nim

% From Progol 4.1 examples/nim.pl  

pos(won(b,1,0)).
pos(won(b,2,0)).
pos(won(b,3,0)).
pos(won(b,5,4)).
pos(won(b,6,4)).
pos(won(b,7,4)).
pos(won(b,9,8)).
pos(won(b,10,8)).
pos(won(b,11,8)).
pos(won(b,13,12)).
pos(won(w,5,4)).
pos(won(w,6,4)).
pos(won(w,7,4)).
pos(won(w,1,0)).
pos(won(w,2,0)).
pos(won(w,3,0)).

neg(won(b,2,1)).
neg(won(b,4,0)).
neg(won(b,4,1)).
neg(won(b,4,2)).
neg(won(b,4,3)).
neg(won(b,5,3)).
neg(won(b,8,4)).
