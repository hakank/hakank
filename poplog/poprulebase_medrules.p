/*

   Poprulebase medical rule-system in Pop-11.

   From TEACH RULEBASE.

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');
uses teaching;

/*
-- An example medical rule-based system -------------------------------

We now give a very simple rule-based system, which helps you decide
whether you are ill, and if so what to do about it. (Do not take this
seriously!)

We first define a ruleset containing a collection of rules, and then use
the poprulebase procedure prb_run to "run" the rules.

To define the ruleset we start with

    define :ruleset ....

and finish with

    enddefine;

In between there are individual rules, in the format

    RULE <name>
        <conditions>
        ==>
        <actions>

*/

;;; ======= start copying from here ========
uses newkit
uses prblib
uses poprulebase

;;; We are going to define a ruleset called "prb_rules".
;;; This is the first line of the definition, which extends down to
;;; the occurrence of "enddefine" below.

define :ruleset prb_rules;

    ;;; The first rule checks whether the program has started. If
    ;;; not, it requests symptoms from the user, and records that
    ;;; the program has started, to prevent the rule firing again.
    ;;; The name of the rule is "start_diagnose"
  RULE start_diagnose
    [NOT started]
         ==>
    [SAY 'I shall do my best to diagnose your problems']
    [started]

    ;;; The second rule checks to see whether there is a
    ;;; sentence in the database, and if not, asks the user
    ;;; to type something in. It then waits until something
    ;;; is typed in. Whatever is typed in is stored in the
    ;;; database in the form
    ;;;         [sentence <words typed in>]

  RULE get_sentence
    [NOT sentence ==]
         ==>
    [READ 'Please tell me one symptom' [sentence ANSWER]]

    ;;; The next rule determines whether the user has had
    ;;; enough.

  RULE stop
    [sentence bye]
         ==>
    [SAY 'Thank you for using my services. Bye.']
    [STOP]

    ;;; Now we have some rules that do diagnosis.

    ;;; Note that each rule removes the sentence after
    ;;; making the diagnosis, so that a new symptom will be
    ;;; requested on the next cycle of the interpreter.

  RULE stomach
    [sentence == stomach ==]
         ==>
    [NOT sentence ==]
    [SAY 'You have eaten too much. Fast for two days.']

  RULE cough
    [sentence == cough ==]
         ==>
    [NOT sentence ==]
    [SAY 'Buy some cough mixture and go to bed.']

    ;;; The next rule has an OR condition, which includes two
    ;;; conditions, either one of which will cause the rule to fire

  RULE headache
    [OR [sentence == headache ==] [sentence == head == aches]]
         ==>
    [NOT sentence ==]
    [SAY 'You probably have flu. Take an aspirin']

    ;;; hakank: added this as an excercise
  RULE tooth
    [sentence == tooth ==]
         ==>
    [NOT sentence ==]
    [SAY 'Please visit a dentist']

  ;;; hakank: added this as an excercise
  RULE anxiety
    [OR 
       [sentence == anxiety ==]
       [sentence == afraid == ]
       [sentence == frighten ==]
       [sentence == frightened ==]
    ] ==>
    [NOT sentence ==]
    [SAY 'Please don\'t be afraid. We will handle this together.']

    ;;; hakank: added this as an excercise
    ;;; Note:
    ;;;  - use the WHERE clause for Pop-11 expressions
    ;;;  - Matches "better" if there is no "not" before
    ;;;  - we don't use ^ on xxx here
  RULE better
    [sentence ??xxx better ==] [WHERE not(xxx matches [== not ==])]
         ==>
    [NOT sentence ==]
    [SAY 'Glad that you recover now']

    ;;; Now a default rule which deals with any other form
    ;;; of sentence

  RULE default
    [sentence == ]
         ==>
    [NOT sentence ==]
    [SAY 'That is very serious, please see a specialist.']

enddefine;

;;; ===== Copy up to here into your file medrules.p =======

;;; Define a master controlling program, which will set some of the
;;; global variables, then call prb_run, with the above rules and an
;;; empty database
define diagnose();

    ;;; First set some global variables which control the behaviour
    ;;; of the interpreter.

    dlocal
        ;;; Prevent prb_run continually pausing.
        prb_walk = false,

        ;;; Prevent more than one active rule firing.
        prb_allrules = false,

        ;;; Make this true for more tracing
        prb_chatty = false;

    ;;; now run the interpreter
    prb_run(prb_rules, []);

enddefine;

;;; now the command to run it
diagnose();


/*
In response to what the program prints out you should try typing
in some symptoms, e.g.


    My head always aches when I think
    I cough all the time
    My stomach hurts most of the time
    My stomach does not hurt any more
    When I cough my head aches

End by typing just:

    bye

*/
