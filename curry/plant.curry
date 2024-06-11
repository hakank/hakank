{-
  Curry: A Tutorial Introduction, page 17
  """
  Exercise 2 In a manufacturing plant two specialized tasks, cut and polish, are executed
  only by specialized workers, Alex, Bert and Chuck. Not every worker can execute every task.
  Only Alex and Bert are able to Cut, whereas only Bert and Chuck are able to Polish. Code
  a non-deterministic operation, assign, that assigns to a task a worker that can execute it.
  """
-}

{-
  Here's the intended solution:

data Task = Cut | Polish

data Worker =  Alex | Bert | Chuck
 deriving Eq -- to compare workers

assign :: Task -> Worker
assign Cut    = Alex
assign Cut    = Bert
assign Polish = Bert
assign Polish = Chuck

team :: (Worker, Worker)
team | x /= y = (x,y)
  where x = assign Cut 
        y = assign Polish


main :: (Worker, Worker)
main = team


 
-}


data People = Alex | Bert | Chuck deriving(Eq)

data Tasks = Cut | Polish

cut :: People -> Bool
cut Alex = True
cut Bert = True

polish :: People -> Bool
polish Bert = True
polish Chuck = True

assign :: Tasks -> People
assign Cut = Alex
assign Cut = Bert
assign Polish = Bert
assign Polish = Chuck


task :: Tasks -> People -> Bool
task Cut Alex = True
task Cut Bert = True
task Polish Bert = True
task Polish Chuck = True

-- The exact use case is not clear so here are some approaches...
main = assign task =:= person &> (task,person)where task,person free
-- Must use &> here! Why?
main2 = cut p1 &> polish p2 &> p1 /= p2 &> ("cut:",p1,"polish:",p2) where p1,p2 free

main3 = c=:=("Alex"?"Bert") &> p=:=("Bert"?"Chuck") &> c /= p &> ("cut:",c,"polish:",p) where c,p free

main4 = task c t &> (c,t) where c,t free

main5 = task Cut c & task Polish p where c,p free
