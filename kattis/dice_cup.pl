% https://open.kattis.com/problems/dicecup
% 1s
% 1.4 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","", Ss),
    maplist(number_string,[A,B],Ss),
    findall(Sum,(between(1,A,I),
                 between(1,B,J),
                 Sum is I+J
                 ),All),
    count_elements(All, Counts),
    sort(2,@>=,Counts,Sorted),
    [_-Max|_]=Sorted,
    findall(K,(member(K-Max,Sorted)),Ms),
    sort(Ms,MsSorted),
    maplist(writeln,MsSorted).
main.

% https://stackoverflow.com/questions/71718132/count-each-element-occurence-in-list-in-prolog
count_elements(Lst, LstCount) :-
    % "sort" also removes duplicates
    sort(Lst, LstSorted),
    findall(Elem-Count, (
        member(Elem, LstSorted), elem_in_list_count(Elem, Lst, Count)
    ), LstCount).
    
elem_in_list_count(Elem, Lst, Count) :-
    aggregate_all(count, member(Elem, Lst), Count).
