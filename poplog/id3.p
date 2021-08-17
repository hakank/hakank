/* This is from induce_rules.p */

/*
-- The program code starts here ---------------------------------------
*/

;;; First some utility procedures.

define instance_category(item) -> category;
    ;;; the second element of an instance (item) is its category.
    ;;; could be redefined.
    lvars item, category;
    item(2) -> category
enddefine;


define instance_values(item) -> vals;
    ;;; Given an instance extract its list of test values
    lvars item, vals;
    hd(item) -> vals;
enddefine;

define instance_value(item, index) -> val;
    ;;; given an instance and a test index, return the value of
    ;;; that test for that instance.
    lvars item, index, val;
    instance_values(item)(index) -> val;
enddefine;

define index_of(test) -> index;
    ;;; the second element of a test specification is its index
    lvars test, index;
    test(2) -> index
enddefine;

define test_name(test) -> name;
    lvars test, name;
    hd(test) -> name
enddefine;

define test_values(test) -> vals;
    ;;; the test values come after the first two elements
    lvars test, vals;
    tl(tl(test)) -> vals
enddefine;


define all_same_category(instances) -> category;
    ;;; Does every instance get assigned to the same category?
    ;;; If so return the category, otherwise false

    lvars instances, category, item;

    destpair(instances) -> (item, instances);
    instance_category(item) -> category;
    for item in instances do
        unless instance_category(item) = category do
            false -> category; return;
        endunless;
    endfor;

enddefine;
/*
;;; tests

all_same_category( [ [[1 2] A] [[2 3] A] [[4 5] A]]) =>
    ** A

all_same_category( [ [[1 2] A] [[2 3] A] [[4 5] B]]) =>
    ** <false>


*/

define getall(test, test_val, instances) ->(this_branch, rest);
    ;;; For a given test, e.g. test3 and its possible value e.g. t3a,
    ;;; find all the instances that have that value for that test,
    ;;; and return a list of them as this_branch, leaving the remaining
    ;;; instances in the list rest, which is also returned

    lvars test, test_val, instances,
        this_branch = [],
        rest = [] ;

    lvars item, index = index_of(test);

    for item in instances do
        ;;; Get the test value in position index in this instance,
        ;;; and compare it with test_value, then decide which list
        ;;; to put the the instance into
        if instance_values(item)(index) == test_val then
            [^item ^^this_branch] -> this_branch
        else
            [^item ^^rest] -> rest
        endif
    endfor;

    ;;; Above, We built the lists up from the left end for efficiency,
    ;;; so the items will be in reverse order.
    ;;; Now restore the original order by reversing the lists.
    ;;; It is safe to use fast_ncrev (to save garbage collections) when
    ;;; we know we have built up properly constructed lists.
    fast_ncrev(this_branch) -> this_branch;
    fast_ncrev(rest) -> rest
enddefine;

/*
;;; test

vars
    test1 = [A 1 a1 a2 a3],
    test2 = [B 2 b1 b2 b3],
    data =
    [
        [[a1 b1] X]
        [[a2 b1] Y]
        [[a3 b1] Y]
        [[a1 b2] X]
        [[a2 b2] Y]
        [[a3 b2] X]
        [[a1 b3] X]
        [[a2 b3] Y]
        [[a3 b3] Y]
    ];

;;; NB rest will print first in these two tests. Get all with "a2"
getall(test1, "a2", data) ==> ==>
    ** [[[a1 b1] X]
        [[a3 b1] Y]
        [[a1 b2] X]
        [[a3 b2] X]
        [[a1 b3] X]
        [[a3 b3] Y]]
    ** [[[a2 b1] Y] [[a2 b2] Y] [[a2 b3] Y]]

;;; get all whose outcome for test2 has value "b3"
getall(test2, "b3", data) ==> ==>
    ** [[[a1 b1] X]
        [[a2 b1] Y]
        [[a3 b1] Y]
        [[a1 b2] X]
        [[a2 b2] Y]
        [[a3 b2] X]]
    ** [[[a1 b3] X] [[a2 b3] Y] [[a3 b3] Y]]

*/

/*
-- The main procedure: induce_rules -----------------------------------
*/

define induce_rules(tests, instances) -> tree;
    ;;; for specification see top of file

    lvars tests, instances, tree;

    lvars test, category, test_val, this_branch;
    if all_same_category(instances) ->> category then
        category -> tree;
    elseif tests == [] then
        ;;; A set of instances have come out with the same results for
        ;;; all tests, yet they don't have the same category label. so
        ;;; they are inconsistent.
        ;;; We could cause an error message, but it is more interesting
        ;;; not to.
        ;;; Return a special sub tree with the inconsistent instances
        ;;; collected in a vector (useful for showtree)
        [INCONSISTENT  {^^instances} ] -> tree;
    else
        ;;; Start building the tree. Recursion will do the main work
        [%
            ;;; Get the first test, and divide the instances according
            ;;; to the values they have for that test, and handle
            ;;; other tests by recursion
            hd(tests) -> test;

            ;;; Put test label at top of tree
            test_name(test),

            ;;; build subtrees for each possible test value
            for test_val in test_values(test) do
                quitif(instances == []);
                getall(test, test_val, instances) ->(this_branch,instances);
                unless this_branch == [] then
                    ;;; Build a partial tree for items having this
                    ;;; test_val for this test
                    [%test_val, induce_rules(tl(tests), this_branch)%]
                endunless;
            endfor
        %] -> tree;

        ;;; If the tree has only one branch the test used is spurious.
        ;;; so pull up the lower level sub tree and return that, except
        ;;; where the tree has the label "INCONSISTENT"
        ;;; Try commenting out the next three lines and see what difference
        ;;; it makes to the tests above!
        while islist(tree)
        and hd(tree) /== "INCONSISTENT"
        and listlength(tl(tree)) == 1
        do
            hd(tl(tree)) -> tree
        endwhile;
        ;;; check that all the instances got used!
        if instances /== [] then
            mishap('Unrecognized instance value?',[^test ^instances])
        endif
    endif
enddefine;

/*
-- A program to turn the decision tree into an expert system

Once you have a decision tree of the sort produced by induce_rules you
can use it with the procedure classify_instance defined below to
interrogate a user about a new instance, by asking a series of
questions, using the answers to traverse the tree, and ending with a
classification of the new instance. The program is fairly rigid as it
does not make use of any "don't know" answers, or any test values other
than those found in data used to create the original tree.

*/

define classify_instance(tree) -> category;
    lvars tree, category;
    if atom(tree) then
        ;;; found the answer, as a result of recursing down the tree
        tree -> category
    else
        ;;; the tree will have a test type and a list of subtrees each
        ;;; with a test-value at its root.
        lvars test_type, subtrees, vals, subtree, answer, count = 1;
        destpair(tree) -> (test_type, subtrees);
        ;;; Make a list of possible test values, with numeric codes
        [%'    ', for subtree in subtrees do
                ;;;
                count sys_>< ":", hd(subtree), '  ', count + 1 -> count;
            endfor,
            ;;; Replace last string with newline
            erase(), newline%] -> vals;
        repeat
            ['For test of type <' ^test_type
                    '> what value does the instance have?'] =>
            'Please type the numeric code for the value in this list'=>

            applist(vals, pr);
            'If you don\'t know or it is not one of these, just press RETURN' =>
            readline() -> answer;
            if answer == [] then
                false -> category;
                return()
            endif;
            if listlength(answer) == 1 then hd(answer) -> answer endif;
        quitif(isnumber(answer)
                and answer > 0
                and answer <= listlength(subtrees));
        'Sorry, that\'s not one of the acceptable numbers. Please try again' =>
        endrepeat;
        ;;; Use the number code to select the subtree and recurse
        ;;; on the head of its tail (because of the structure of the
        ;;; output of induce_rules
        hd(tl(subtrees(answer))) -> subtree;
        if islist(subtree) and hd(subtree) == "INCONSISTENT" then
            'Data for this case inconsistent'
        else
            classify_instance(subtree)
        endif -> category
    endif;
enddefine;

/*

-- Further exercises --------------------------------------------------

1. Create several large data-sets and see how well induce rules performs
with them. Can you construct a set of data and a set of tests such that
induce_rules produces a rather silly decision tree, e.g. one for each
each question only sets a single instance apart from the rest, so that
for identification of some instances you have to ask a great many
questions whereas for others you have to ask very few.

2. Try reading the literature on ID3 and see if you can change the
program so as to produce a more balanced decision tree.

3. Try to decide whether the above algorithm provides a sound basis for
predicting the outcome of new cases on the basis of old examples. Is
there any way you could improve its reliability?

4. How does a program like induce_rules compare with training a neural
net on examples?

5. Under what conditions can a tree produced by induce_rules classify an
instance with features that it has never seen before.

6. Produce a version of classify_instance, which, instead of
interrogating the user is given a decision tree and an instance and
produces a category for the instance (or false if it cannot).

7. As shown by the animals example near the beginning of this file, the
output of induce_rules for the same set of instances can be different
depending on the order in which the test specifications are given.
Sometimes the two decision trees produced for a given set of data and a
given set of test specifications will include different tests, or will
use different subsets of possible values for a given test (like the
animals example above). This can imply that the data show that there are
different sufficient conditions for something to be classified, and the
above procedure induce_rules will not find them all. Can you fix this?

8. Can you extend classify_instance so that if the current decision tree
does not enable you to classify an instance, you can have a little
dialogue in which you say how it should be classified, as a result of
which classify_instance modifies the decision tree? The easiest way
would be to make it add the new instance to the original list
of instances and then recreate the decision tree from the enlarged
list. Is there a better way to extend the decision tree incrementally?

9. Consider how induce_rules deals with inconsistent data. I.e. it
produces a node in the decision tree recording the inconsistency. Are
there better ways it could handle such data? (Presumably it will depend
on the problem domain.)

10. The procedure classify_instances has a very poor user interface.
What is wrong with it? Can you improve it? Would it be better to use
pop-up dialogue boxes ? (See HELP * POPUPTOOL)


*/
