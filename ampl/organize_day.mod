/*

  Organize a day.


ECLiPSe CLP has the following alternative solutions:
Work Mail Shop Bank
[11, 9, 15, 10],   
[11, 10, 15, 9], 
[13, 9, 11, 10], 
[13, 10, 11, 9],
[13, 12, 10, 9]]

i.e.
mail 9, bank 10, work 11, shop 15
bank 9, mail 10, work 11, shop 15
mail 9, bank 10, shop 11, work 13
bank 9, mail 10, shop 11, work 13
bank 9, shop 10, mail 12, work 13

This model

z = 40

y [*,*]
:  bank mail shop work    :=
9    1    0    0    0
10   0    0    1    0
11   0    0    1    0
12   0    1    0    0
13   0    0    0    1
14   0    0    0    1
15   0    0    0    1
16   0    0    0    1
17   0    0    0    0
;

:    begin  end    :=
bank    9    10
mail    9    10
shop   11    13
work   11    15
;

times [*] :=
bank  1
mail  1
shop  2
work  4
;

set before := (bank,shop) (mail,work);

bank 9
shop 10
shop 11
mail 12
work 13
work 14
work 15
work 16

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

/*
# verbose version
var Beg_Work integer >= 9 <= 17;
var Beg_Mail integer >= 9 <= 17;
var Beg_Shop integer >= 9 <= 17;
var Beg_Bank integer >= 9 <= 17;

var End_Work integer >= 9 <= 17;
var End_Mail integer >= 9 <= 17;
var End_Shop integer >= 9 <= 17;
var End_Bank integer >= 9 <= 17;

minimize z: Beg_Work + Beg_Mail+Beg_Shop+Beg_Bank;

c1: End_Work = Beg_Work + 4; # Work 4h
c2: End_Mail = Beg_Mail + 1; # Mail 1h
c3: End_Shop = Beg_Shop + 2; # Shop 2h
c4: End_Bank = Beg_Bank + 1; # Bank 1h
c5: End_Bank <= Beg_Shop;
c6: End_Mail <= Beg_Work;
c7: Beg_Work >= 11;

*/

param start_time;
param end_time;
set todo;
var begin{todo} >= start_time <= end_time;
var end{todo} >= start_time <= end_time;
param M := end_time + 1;
#var y{todo, todo} binary;
var y{start_time..end_time, todo} binary;
#var y{todo, start_time..end_time} binary;

param times{todo};
set before within todo cross todo; # precedence

# minimera starttiderna
minimize z: sum{i in todo} begin[i];
# minimize z: begin['mail'];

set_times{i in todo, t in start_time..end_time}: end[i] = begin[i] + times[i];

# precedence
#do_order{ (i,j) in before}: begin[j] >= begin[i] + times[i] + 1;
do_order{ (i,j) in before}: begin[j] >= end[i] + 1;

# work after 11
work_after_11: begin['work'] >= 11;

        
c_y1{i in todo}:
        sum{t in start_time..end_time} y[t,i] = times[i];

# can't do two things at the same time
c_y2{t in start_time..end_time}:
        sum{i in todo} y[t,i] <= 1;

#c_work_after_11{t in start_time..10}: y[t,'work'] = 0; 

c_order{(i,j) in before, t1 in start_time..end_time, t2 in start_time..end_time}: 
        y[t1,i] <= y[t2,j] + 1
        ;


data;

param start_time := 9;
param end_time   := 17;


param: todo: times := 
   work 4 # 4 
   mail 1 # 1
   shop 2 # 2
   bank 1 # 1
;

# x must be done before y
set before :=
        bank shop # orig
        mail work # orig

#        shop bank  # test 
#        work mail  # test
;


#option presolve 10000;
# option presolve 0;
#option show_stats 1000;
# option cplex_options "sensitivity";
option solver cplex;
#option solver bonmin;
solve;

display z;
display y;
display begin, end;
display times;
display before;
#display begin, end;
# display times;
#for{i in todo} {
#        printf "%s %d %d (time: %d)\n", i, begin[i], end[i], times[i]; 
#}

for{t in start_time..end_time} {
  for{i in todo: y[t,i] > 0} {
    printf "%s %d\n", i, t;
  }
}


# for the verbose version
#display Beg_Work + Beg_Mail + Beg_Shop + Beg_Bank;
#display Beg_Work,Beg_Mail,Beg_Shop,Beg_Bank;
#display End_Work,End_Mail,End_Shop,End_Bank;


#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up, _var.dual;
#display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up, _con.dual,_con.status;




