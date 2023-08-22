'\006\call_in_module'(user('/home/hakank/kattis/eligibility'),_1) :-
    !,
    call(_1).

main :-
    user:read_string(user_input,1000000000,S),
    user:split_string(S,"
","
",[_1|Ss]),
    user:s(Ss).

s([]).
s([S|Ss]) :-
    user:split_string(S," ",[],[Name,StudentDate,DOB,CoursesS]),
    user:d(StudentDate,[SY,_SM,_SD]),
    user:d(DOB,[BY,_BM,_BD]),
    user:number_string(Courses,CoursesS),
    user:'s/1/2/$disj/1'(SY,BY,Courses,T),
    format('~s ~s~n',[Name,T]),
    user:s(Ss).

's/1/2/$disj/1'(SY,BY,Courses,T) :-
    user:'s/1/2/$disj/1/4/1/$disj/1'(SY,BY),
    !,
    T="eligible".
's/1/2/$disj/1'(SY,BY,Courses,T) :-
    Courses>=41,
    !,
    T="ineligible".
's/1/2/$disj/1'(SY,BY,Courses,T) :-
    T="coach petitions".

's/1/2/$disj/1/4/1/$disj/1'(SY,BY) :-
    SY>=2010.
's/1/2/$disj/1/4/1/$disj/1'(SY,BY) :-
    BY>=1991.

d(Date,[Y,M,D]) :-
    user:split_string(Date,"/","/",V),
    user:maplist(number_string,[Y,M,D],V).


