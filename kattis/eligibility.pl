% https://open.kattis.com/problems/eligibility
% 2s
% 1.6 Easy

/*
* if the student first began post-secondary studies in 2010 or later, 
  the student is eligible;

* if the student is born in 1991 or later, the student is eligible;

* if none of the above applies, and the student has completed more than 
  an equivalent of 8 semesters of full-time study, the student is ineligible;

* if none of the above applies, the coach may petition for an extension 
  of eligibility by providing the student’s academic and work history.

For “equivalent of 8 semesters of full-time study,” we consider each semester 
of full-time study to be equivalent to a student completing 5 courses. Thus, 
a student who has completed 41 courses or more is considered to have more 
than 8 semesters of full-time study.

*/

main :-
    read_string(user_input,1000000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).
s([]).
s([S|Ss]) :-
    split_string(S," ","",[Name,StudentDate,DOB,CoursesS]),
    d(StudentDate,[SY,_SM,_SD]),
    d(DOB,[BY,_BM,_BD]),
    number_string(Courses,CoursesS),
    ( (SY >= 2010 ; BY >= 1991) ->
        T="eligible"
    ;
        (Courses >= 41 ->
            T="ineligible"
        ;
            T="coach petitions"
        )
    ),
    format('~s ~s~n',[Name,T]),
    s(Ss).

d(Date,[Y,M,D]) :-
    split_string(Date,"/","/",V),
    maplist(number_string,[Y,M,D],V).
