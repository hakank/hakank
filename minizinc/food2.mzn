% 
% Food manufacture problem in MiniZinc.
% 
% From GLPK example food2.mod:
% """
% Food Manufacture 2, section 12.2 in
% Williams, "Model Building in Mathematical Programming"
% 
% Sebastian Nowozin <nowozin@gmail.com>
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Note: This models gives exactly the same total profit as the GLPK MathProg 
% model (107842.59). It differs, however, in the buying costs and storage costs.
%

int: num_oils;
set of int: oils = 1..num_oils;
int: num_months;
set of int: month = 1..num_months;

float: M;

% Buying prices of the raw oils in the next six month. 
array[month, oils] of float: buyingprices;

% Actual amount bought in each month. 
array[month, oils] of var float: buys;

% Stock for each oil. 
array[month, oils] of var float: stock;

% Price of the produced product 
float: productprice;
float: storagecost;

array[oils] of float: oilhardness;

% Actual amount of output oil produced in each month 
array[month] of var float: production;
% array[month, oils] of var float: useoil;

array[month,oils] of var float: useoil; %  >= 0, <= M;
array[month, oils] of var 0..1: useoilb;


var float: totalprofit = sum(m in month) (productprice*production[m]) 
    - sum(m in month, o in oils) (buyingprices[m,o]*buys[m,o])
    - sum(m in month, o in oils) (storagecost*stock[m,o]);

var float: turnover = sum(m in month) (productprice*production[m]);
var float: buying_costs = sum(m in month, o in oils) (buyingprices[m,o]*buys[m,o]);
var float: storage_costs = sum(m in month, o in oils) (storagecost*stock[m,o]);
% array[month] of var float: hardness; % don't work, see below...

solve maximize totalprofit;

constraint

   % 1. Starting stock 
   forall(o in oils) (
      stock[1,o] = 500.0
   )
   /\
   forall(o in oils) (
      stock[6,o] + buys[6,o] - useoil[6,o] >= 500.0
   )

  /\
  % 2. Stock constraints 
  forall(m in month, o in oils) (
    buys[m,o] >= 0.0
    /\
    stock[m,o] >= 0.0
    /\
    useoil[m,o] >= 0.0
    /\
    useoil[m,o] <= M
    /\
    stock[m,o] <= 1000.0
  )
  /\
  forall(m in month, o in oils) (
    useoil[m,o] <= stock[m,o] + buys[m,o]
  )
  /\
  forall(m1 in month, m2 in month, o in oils where m2 = m1+1) (
    stock[m2,o] = stock[m1,o] + buys[m1,o] - useoil[m1,o]
  )
  /\
  forall(m in month) (
    sum(o in oils) (oilhardness[o]*useoil[m,o]) >= 3.0*production[m]
  )
  /\
  forall(m in month) (
    sum(o in oils) (oilhardness[o]*useoil[m,o]) <= 6.0*production[m]
  )

  /\
  forall(m in month) (
    production[m] >= 0.0
    /\
    production[m] = sum(o in oils) (useoil[m,o])
  )

  /\
  % 3. Refining constraints 
  forall(m in month) (
    useoil[m,VEG1]+useoil[m,VEG2] <= 200.0
  )
  /\

  forall(m in month) (
    useoil[m,OIL1]+useoil[m,OIL2]+useoil[m,OIL3] <= 250.0
  )

  %    % don't work! Gives the following error:
  % /\ % float_times/3 is not supported by the MIP solver backend.
  % forall(m in month) (
  %    (sum(o in oils) (useoil[m,o])) * hardness[m] = (sum(o in oils) (oilhardness[o]*useoil[m,o])) 
  % )

  % 4. Additional conditions [compared to food.mzn]
  %    i) The food may never be made up of more than three oils every month
  /\
  forall(m in month, o in oils) (
     M*int2float(useoilb[m,o]) >= useoil[m,o]
  )
  /\
  forall(m in month) (
     sum(o in oils) (int2float(useoilb[m,o])) <= 3.0
  )
  /\
  %  ii) If an oil is used in a month, at least 20 tons must be used.
  forall(m in month, o in oils) (
     20.0*int2float(useoilb[m,o]) <= useoil[m,o]
  )
  /\
  % iii) If either of VEG1 or VEG2 is used in a month, OIL2 must also be used
  forall(m in month) (
     int2float(useoilb[m,VEG1]) <= int2float(useoilb[m,OIL3])
  )
  /\
  forall(m in month) (
     int2float(useoilb[m,VEG2]) <= int2float(useoilb[m,OIL3])
  )

;


%
% Note: The print function is not as flexible as in GLPK...
%
output
[  
  "\nMonth: " ++ show(m) ++ "\nPRODUCE: " ++ show(production[m]) ++ " tons, hardness xxx\n" ++
  "\tVEG1   VEG2    OIL1    OIL2    OIL3\n" ++
  "STOCK" ++ show(m) ++ "\t" ++ show(stock[m,1]) ++ " " ++ show(stock[m,2]) ++ " " ++ show(stock[m,3]) ++ " " ++ show(stock[m,4]) ++ " " ++ show(stock[m,5]) ++ "\n" ++
  "BUYS" ++ show(m) ++ "\t" ++ show(buys[m,1]) ++ " " ++ show(buys[m,2]) ++ " " ++ show(buys[m,3]) ++ " " ++ show(buys[m,4]) ++ " " ++ show(buys[m,5]) ++ "\n" ++
  "USE" ++ show(m) ++ "\t" ++ show(useoil[m,1]) ++ " " ++ show(useoil[m,2]) ++ " " ++ show(useoil[m,3]) ++ " " ++ show(useoil[m,4]) ++ " " ++ show(useoil[m,5]) ++ "\n"
   
  | m in month
] ++
 [
 "Total profit: ", show(totalprofit), "\n",
 "turnover: ", show(turnover), "\n",
 "byuing costs: ", show(buying_costs), "\n",
 "storage costs: ", show(storage_costs), "\n",

];


%
% data;
% 
num_oils = 5;
int: VEG1 = 1;
int: VEG2 = 2;
int: OIL1 = 3;
int: OIL2 = 4;
int: OIL3 = 5;
% oils =   [VEG1, VEG2, OIL1, OIL2, OIL3];
oilhardness = [8.8, 6.1, 2.0, 4.2, 5.0] ;

num_months = 6;

buyingprices = array2d(month, oils, [
110.0,     120.0,     130.0,     110.0,     115.0,
130.0,     130.0,     110.0,      90.0,     115.0,
110.0,     140.0,     130.0,     100.0,      95.0,
120.0,     110.0,     120.0,     120.0,     125.0,
100.0,     120.0,     150.0,     110.0,     105.0,
 90.0,     100.0,     140.0,      80.0,     135.0]) ;

productprice = 150.0;
storagecost = 5.0;
M = 1000.0;