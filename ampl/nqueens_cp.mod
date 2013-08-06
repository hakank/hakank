
param n integer > 0;
var q {1..n} integer >= 1 <= n;


subj to c1: alldiff ({i in 1..n} q[i]);
subj to c2: alldiff ({i in 1..n} q[i]+i);
subj to c3: alldiff ({i in 1..n} q[i]-i);

data;

let n := 500;

printf "n: %d\n", n;
option solver gecode;
option gecode_options 'icl=dom var_branching=size_min val_branching=med outlev=1 outfreq=1 timelimit=60';
solve;

for{i in 1..n} { printf "%2d ", q[i]; };
printf "\n";

