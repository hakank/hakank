%
% Operation Research problem in ASP.
%
% Organizing a day.
% 
% Problem formulation:
% Slides on (finite domain) Constraint Logic Programming, page 38f%
% http://www.icparc.ic.ac.uk/eclipse/reports/eclipse.ppt
% (via http://www.icparc.ic.ac.uk/eclipse/reports/ )
% 
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

#const begin = 9.
#const end = 17.
times(begin..end).

tasks(bank;shop;mail;work).

% time of duration for each task
duration(work, 4).
duration(mail, 1).
duration(shop, 2).
duration(bank, 1).

% before(task1, task2): task1 must be finished before task2
before(bank, shop).
before(mail, work).

1 { task(Task, Start, End) : times(Start;End) } 1 :- tasks(Task).

% end time
:- task(Task, Start, End), duration(Task, Duration), Start+Duration != End.

% end times must be <= 17
:- task(Task, Start, End), End > end.

% No overlap of tasks
no_overlap(Task1, Task2) :-
        task(Task1, Start1, End1),
        task(Task2, Start2, End2),
        End1 <= Start2.

no_overlap(Task1, Task2) :-
        task(Task1, Start1, End1),
        task(Task2, Start2, End2),
        End2 <= Start1.

:- tasks(Task1;Task2), Task1 != Task2, not no_overlap(Task1, Task2).

% work start earliest at 11
:- task(work, Start, End), Start < 11.

% before(Task1, Task2): Task1 must be done before Task2
:- before(T1, T2), task(T1, S1, E1), task(T2, S2, E2), S2 < E1.

#hide.
#show task(T, S, E).
