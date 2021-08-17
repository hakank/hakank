/*
   
   Toy Eliza using poprulebase in Pop-11.

   From HELP RULEBASE.
   
   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');
uses newkit
uses teaching;
uses poprulebase;

define :ruleset eliza_rules;
    ;;; The next bit can specify that prb_allrules should be false when
    ;;; this ruleset runs. I.e. only one rule should fire on each cycle

    ;;; hakank: added num_negative for counting negative things,
    [DLOCAL [prb_allrules = true] [num_negative = 0]];

  RULE start
    ;;; This rule should fire once only
    [NOT started]
         ==>
    [SAY 'Hiya, I am a cool psychotherapist, tell me your problem']
    ;;; Prevent the rule firing again
    [started]

  RULE get_sentence
    ;;; no sentence to reply to
    [NOT sentence ==]
         ==>
    ;;; get a sentence to reply to, and store it in the database
    [READ '' [sentence ANSWER]]

  RULE stop
    ;;; if the user typed only "bye" then stop
    [sentence bye]
         ==>
    [SAY 'The bill is 10 pounds.\nYou can send the money to Riccardo']
    [STOP]

  RULE doctor
    [sentence == doctor ==]
         ==>
    ;;; delete the sentence and respond to it.
    [NOT sentence ==]
    [SAY 'Do you think you need a doctor?']

  RULE hello
    ;;; use several patterns to recognise a greeting
    ;;; This rule will fire if ANY of the patterns matches the
    ;;; sentence just typed in.
    [OR
        [sentence == hi ==]
        [sentence == hello ==]
        [sentence == hiya ==]]
         ==>
    [NOT sentence ==]
    [SAY 'How are you?']

    ;;; hakank: count the negatives
    ;;; Note: I have to embed the increment in the SAY clause,
    ;;; There must be a better way...
  RULE negative
    [OR 
       [sentence == no ==]
       [sentence == not ==]
    ]
      ==>
    [NOT sentence ==]
    [SAY [popval num_negative + 1 -> num_negative; 'You have now said ' >< num_negative >< ' negative things. Please, go on.']]


  RULE family
    ;;; Look for any reference to a close relation.
    ;;; You could add other options.
    ;;; hakank: added brother, sister, son, daughter
    [OR
        [sentence == mother ==]
        [sentence == father ==]
        [sentence == brother ==]
        [sentence == sister ==]
        [sentence == daugher ==]
        [sentence == son ==]
        [sentence == family ==]]
         ==>
    [NOT sentence ==]
    [SAY 'Tell me more about your family']

  ;;; Now some rules using pattern variables
  RULE I_am
    ;;; E.g. if the user types "I am feeling blue today"
    [OR [sentence I am ??x] [sentence i am ??x]]
         ==>
    ;;; delete the sentence so that it will not trigger a reaction
    ;;; again
    [NOT sentence ==]
    ;;; Save it as an "old" sentence for possible use later in
    ;;; the rule pic_old_sentence, below
    [old_sentence you were ??x]
    [SAY 'Why do you say you are' ??x '?']

  RULE I_feel
    [sentence I feel ??x]
         ==>
    [NOT sentence ==]
    [old_sentence you felt ??x]
    [SAY 'Do you often feel' ??x '?']

  ;;; Now a rule with two pattern variables
  RULE someone_said
    [OR
        [sentence ??x1 said ??x2]
        [sentence ??x1 says ??x2]
        [sentence ??x1 thinks ??x2]
    ]
        ==>
    [NOT sentence ==]
    [old_sentence ??x1 thought ??x2]
    [SAY 'Does anyone else think' ??x2 '?']

  RULE pick_old_sentence
    ;;; Something was typed which did not match an earlier rule
    [sentence ==]
    ;;; pick the most recently stored old sentence, use it to
    ;;; generate a response.
    [old_sentence ??x]
         ==>
    ;;; now delete both what has just been typed
    [NOT sentence ==]
    ;;; and the old sentence
    [NOT old_sentence ??x]
    ;;; and use the old sentence to produce some output
    [SAY 'Previously you said ' ??x ', can you elaborate on this?']

  RULE pardon
    [sentence ==]
    [NOT old_sentence ==]
         ==>
    [NOT sentence ==]
    [SAY [popval
            oneof(['Pardon?'
                'Tell me more about it'
                'Can you be a bit clearer on this'])]]
enddefine;

;;; Note the last action included a list of the form
;;;     [popval <pop-11 expression> ]
;;; Such a list is replaced with the result of evaluating the
;;; Pop-11 expression.


;;; -- Now a master procedure to run the eliza rules ----------------------

define run_eliza();

    ;;; First set some global variables which control the behaviour
    ;;; of the interpreter.

    dlocal
        ;;; Prevent prb_run continually pausing. Make it true
        ;;; to slow things down and show what is going on
        prb_walk = false,

        ;;; Make this true for more tracing
        prb_chatty = false;

    ;;; now run the interpreter
    prb_run(eliza_rules, []);

enddefine;

;;; The startup command

run_eliza();

