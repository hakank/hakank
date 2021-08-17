/*
    test av neural
    see TEACH teachneural
*/
compile('/home/hakank/Poplib/init.p');

uses neural;

lib nettraining; 


    vars xor_list = [[false false false]
                     [true false true]
                     [false true true]
                     [true true false]];

    vars xor_template = [[in boolean input1]
                         [in boolean input2]
                         [out boolean output]];

    nn_make_egs("xor_set", xor_template, xor_list);

    nn_generate_egs("xor_set");
    vars eg_rec = nn_example_sets("xor_set");
    eg_targ_data(eg_rec) =>
    eg_in_data(eg_rec) =>
    arrayvector(eg_targ_data(eg_rec)) =>
    arrayvector(eg_in_data(eg_rec)) =>
    make_bpnet(2, {2 1}, 2.0, 0.2, 0.9)
                                -> nn_neural_nets("xor_net");

    pr_bpweights(nn_neural_nets("xor_net"));
    vars results = nn_test_egs("xor_set", "xor_net", false);
    results =>

    nn_learn_egs("xor_set", "xor_net", 1000, true);
    nn_test_egs("xor_set", "xor_net", false) =>

    nn_learn_egs("xor_set", "xor_net", 5000, true);
    nn_test_egs("xor_set", "xor_net", false) =>

    pr_bpweights(nn_neural_nets("xor_net"));


    pr('\n\nDeclaring new data type\n\n');
    nn_declare_range("percent", 0, 100);

    nn_declare_set("colour",
                   [red orange yellow green blue indigo violet]);


    nn_make_egs("colour_set",
                [[in percent red] [in percent green] [in percent blue]
                 [out colour colour]],
                [[100 0 0 red] [40 40 20 yellow] [0 100 0 green]
                 [0 0 100 blue] [40 0 60 violet] [30 0 70 indigo]]);

    nn_generate_egs("colour_set");
    nn_units_needed([percent percent percent]) =>   ;;; for input
    nn_units_needed([colour]) =>                    ;;; for output
    vars eg_rec = nn_example_sets("colour_set");
    eg_in_units(eg_rec) =>
    eg_out_units(eg_rec) => 

    make_bpnet(3, {4 7}, 2.0, 0.2, 0.9) ->
                                        nn_neural_nets("col_net");
    pr_bpweights(nn_neural_nets("col_net"));
    nn_learn_egs("colour_set", "col_net", 5000, false);

    nn_test_egs("colour_set", "col_net", false)=>

