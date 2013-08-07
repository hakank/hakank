/*
 *
 * Symbolic Regression in JGAP.
 *
 * This program is based on the JGAP example MathProblem.java 
 * (by Klaus Meffert) with some more generality.
 *
 * TODO:
 *  - option for ignoring specific variables
 *  - option for stopping:
 *     - running forever
 *     - after a specific time, 
 *  - accept nominal values in the data section; then 
 *    converted to numeric values.
 *  - add more fitness metrics.
 *  - penalty for longer solutions (parsimony pressure)
 *  - support for different "main" return classes, i.e. not just DoubleClass
 *  - more statistical measures, e.g. 
 *    R-squared, mean squared error, mean absolut error, minimum error,
 *    maximum error
 *  - run the program several times and collect statistics
 *  - more/better error checks
 *  - more building blocks, a la Eureqa http://ccsl.mae.cornell.edu/eureqa_ops
 *  - for classifier problems (e.g. iris.conf), show the most probable class
 *  - for classifier problems, create a confusion matrix  
 *  - some kind of plotting the data vs. errors/fitness
 *  - show the N best solutions
 *  - is it possible to change some of the basic parameters if 
 *    running a long time? E.g. population size, max nodes, etc.
 *  - it would be nice with some pretty printing of the best program
 *  - support other bases than 10?
 *  - support for derivata (a la Eureqa)? This may be hard...
 *  - Pareto front?
 *  - integrate with Weka?
 *  - simplify the best solution with a CAS?
 *
 * This program was written by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also, see my JGAP page http://www.hakank.org/jgap/
 *
 */

import org.apache.log4j.*;
import java.util.*;
import java.io.*;
import java.text.*;
import java.util.Collections.*;

import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.function.*;
import org.jgap.gp.impl.*;
import org.jgap.gp.terminal.*;
import org.jgap.util.*;

public class SymbolicRegression
    extends GPProblem {
    
    private transient static Logger LOGGER = Logger.getLogger(SymbolicRegression.class);

    /*
     * public variables which may be changed by configuration file
     *
     */

    // number of variables to use (output variable is excluded)
    public static int numInputVariables;
    // the variables to use (of size numInputVariables)
    public static Variable[] variables;
    // variable name
    public static String[] variableNames;
    // index of the output variable
    public static Integer outputVariable; // default last
    public static int[] ignoreVariables; // TODO

    // constants
    public static ArrayList<Double> constants = new ArrayList<Double>();

    // size of data
    public static int numRows;

    // the data (as Double)
    // Note: the last row is the output variable per default
    protected static Double[][] data;

    // If we have found a perfect solution.
    public static boolean foundPerfect  = false;
    
    // standard GP parameters
    public static int minInitDepth      = 2;
    public static int maxInitDepth      = 4;
    public static int populationSize    = 1000;
    public static int maxCrossoverDepth = 8;
    public static int programCreationMaxTries = 5;
    public static int numEvolutions     = 1800;
    public static boolean verboseOutput = true;
    public static int maxNodes          = 21;
    public static double functionProb = 0.9d;
    public static float reproductionProb = 0.1f; // float
    public static float mutationProb = 0.1f; // float
    public static float crossoverProb = 0.9f; // float
    public static float dynamizeArityProb = 0.08f; // float
    public static double newChromsPercent = 0.3d;
    public static int tournamentSelectorSize = 0;
    public static boolean noCommandGeneCloning = true;
    public static boolean strictProgramCreation = false;
    public static boolean useProgramCache = true;

    // lower/upper ranges for the Terminal
    public static double lowerRange     = -10.0d;
    public static double upperRange     = -10.0d;

    // Should the terminal be a whole numbers (integers as double) or not?
    public static boolean terminalWholeNumbers = true;

    public static String returnType = "DoubleClass"; // not used yet
    public static String presentation   = "";

    // Using ADF
    public static int adfArity = 0;
    public static String adfType = "double";
    public static boolean useADF = false;

    // list of functions (as strings)
    public static String[] functions    = {"Multiply","Divide","Add","Subtract"};
    // list of functions for ADF
    public static String[] adfFunctions    = {"Multiply3","Divide","Add3","Subtract"};

    // if the values are too small we may want to scale
    // the error
    public static double scaleError = -1.0d;

    // timing
    public static long startTime;
    public static long endTime;

    // if >= 0.0d, then stop if the fitness is below or equal
    // this value
    public static double stopCriteriaFitness = -1.0d;

    // shows the whole population (sorted by fitness)
    // in all generations. Mainly for debugging purposes.
    public static boolean showPopulation = false;

    // show similiar solutions with the same fitness
    // as the overall best program
    public static boolean showSimiliar = false;

    // 2010-02-27
    // how to sort the similiar solutions.
    // Valid options:
    //   - occurrence (descending, default)
    //   - length (asccending)
    public static String similiarSortMethod = "occurrence";

    // shows progression as generation number
    public static boolean showProgression = false;

    // show results for all generations
    public static boolean showAllGenerations = false;

    // show all the results for the fittest program
    public static boolean showResults = false;
    public static int resultPrecision = 5;

    // take a sample of the data set (if > 0.0)
    public static double samplePCT = 0.0d;

    // hits criteria: if >= 0.0d then collect and show
    // number of fitness values (errors) that is equal 
    // or below this value.
    public static double hitsCriteria = -1.0d;

    // withheld a percentage for validation of the
    // of the fittest program
    public static double validationPCT = 0.0d;
    protected static Double[][] validationSet;

    // for testing some values
    public static Double[][] testData;

    // for ModuloReplaceD: replacement for 0
    public static int modReplace = 0;

    // 2010-02-27
    // make time series based on a single sequence
    public static boolean makeTimeSeries = false;
    // 2010-02-27
    // make time series based on a single sequence
    // and adding the index of the instance
    public static boolean makeTimeSeriesWithIndex = false;

    // make a sequence of (ix, number) based on a single sequence
    // public static boolean makeIndexSeq = false;

    // 2010-02-22
    // Error method. 
    // Valid options: 
    //    totalError (default)
    //    minError
    //    meanError
    //    medianError
    //    maxError
    // 
    // Note that hitsCriteria > -1 overrides errorMethod
    // 
    public static String errorMethod = "totalError";

    // 2010-02-26: If we don't want to have any Terminals
    public static boolean noTerminals = false;

    // 2010-03-01: Penalty if the number of nodes in a program
    //             is less than minimum required.
    public static int minNodes = -1;
    //  The penalty for less nodes (may be application dependent)
    public static double minNodesPenalty = 0.0d;
    
    // 2010-03-02: Penalty if the variables are not different
    public static boolean alldifferentVariables = false;
    public static double alldifferentVariablesPenalty = 0.0d;


    public SymbolicRegression(GPConfiguration a_conf)
        throws InvalidConfigurationException {
        super(a_conf);
    }
    
    /**
     * This method is used for setting up the commands and terminals that can be
     * used to solve the problem.
     *
     * @return GPGenotype
     * @throws InvalidConfigurationException
     */
    public GPGenotype create()
        throws InvalidConfigurationException {
        GPConfiguration conf = getGPConfiguration();

        // At first, we define the return type of the GP program.
        // ------------------------------------------------------
        // Then, we define the arguments of the GP parts. Normally, only for ADF's
        // there is a specification here, otherwise it is empty as in first case.
        // -----------------------------------------------------------------------
        Class[] types;
        Class[][] argTypes;
        if (useADF) {
            if ("boolean".equals(adfType)) {
                types = new Class[]{CommandGene.DoubleClass, CommandGene.BooleanClass};
            } else if ("integer".equals(adfType)) {
                types = new Class[]{CommandGene.DoubleClass, CommandGene.IntegerClass};
            } else {
                types = new Class[]{CommandGene.DoubleClass, CommandGene.DoubleClass};
            }

            Class[] adfs = new Class[adfArity];
            for (int i = 0; i < adfArity; i++) {
                if ("boolean".equals(adfType)) {
                    adfs[i] = CommandGene.BooleanClass;
                } else if ("integer".equals(adfType)) {
                    adfs[i] = CommandGene.IntegerClass;
                } else {
                    adfs[i] = CommandGene.DoubleClass;
                }
            }

            argTypes = new Class[][]{{}, adfs};
                
        } else {
            types = new Class[]{CommandGene.DoubleClass};
            argTypes = new Class[][]{{}};
        }



        // Configure desired minimum number of nodes per sub program.
        // Same as with types: First entry here corresponds with first entry in
        // nodeSets.
        // Configure desired maximum number of nodes per sub program.
        // First entry here corresponds with first entry in nodeSets.
        //
        // This is experimental!
        int[] minDepths;
        int[] maxDepths;
        if (useADF) {
            minDepths = new int[]{1,1};
            maxDepths = new int[]{9,9};
        } else {
            minDepths = new int[]{1};
            maxDepths = new int[]{9};
        }


        // Next, we define the set of available GP commands and terminals to use.
        // Please see package org.jgap.gp.function and org.jgap.gp.terminal
        // You can easily add commands and terminals of your own.
        // ----------------------------------------------------------------------
        CommandGene[] commands = makeCommands(conf, functions, lowerRange, upperRange, "plain");

        // Create the node sets
        int command_len = commands.length;        
        CommandGene[][] nodeSets = new CommandGene[2][numInputVariables+command_len];

        // the variables:
        //  1) in the nodeSets matrix
        //  2) as variables (to be used for fitness checking)
        // --------------------------------------------------
        variables = new Variable[numInputVariables];
        int variableIndex = 0;
        for(int i = 0; i < numInputVariables+1; i++) {
            String variableName = variableNames[i];
            if (i != outputVariable) {
                if (variableNames != null && variableNames.length > 0) {
                    variableName = variableNames[i];
                }
                variables[variableIndex] = Variable.create(conf, variableName, CommandGene.DoubleClass);
                nodeSets[0][variableIndex] = variables[variableIndex];
                System.out.println("input variable: " + variables[variableIndex]);
                variableIndex++;
            }
        }
        
        // assign the functions/terminals
        // ------------------------------
        for(int i = 0; i < command_len; i++) {
            System.out.println("function1: " + commands[i]);
            nodeSets[0][i+numInputVariables] = commands[i];
        }

        // ADF functions in the second array in nodeSets
        if (useADF) {
            CommandGene[] adfCommands = makeCommands(conf, adfFunctions, lowerRange, upperRange, "ADF");
            int adfLength = adfCommands.length;
            nodeSets[1] = new CommandGene[adfLength];
            for (int i = 0; i < adfLength; i++) {
                System.out.println("ADF function: " + adfCommands[i]);
                nodeSets[1][i] = adfCommands[i];
            }
        }

        // this is experimental.
        boolean[] full;
        if (useADF) {
            full = new boolean[]{true,true};
        } else {
            full = new boolean[]{true};
        }
        boolean[] fullModeAllowed = full;

        // Create genotype with initial population. Here, we use the 
        // declarations made above:
        // ----------------------------------------------------------
        return GPGenotype.randomInitialGenotype(conf, types, argTypes, nodeSets,
                                                maxNodes,verboseOutput);

        // this is experimental
        // return GPGenotype.randomInitialGenotype(conf, types, argTypes, nodeSets,
        //                             minDepths,maxDepths, maxNodes, fullModeAllowed,verboseOutput);

    }
    

    public static void readFile(String file) {
        
        try {

            BufferedReader inr = new BufferedReader(new FileReader(file));
            String str;
            int lineCount = 0;
            boolean gotData = false;
            ArrayList<Double[]> theData = new ArrayList<Double[]>();
            ArrayList<Double[]> theValidationSet = new ArrayList<Double[]>();
            // contains user defined testdata
            ArrayList<Double[]> theTestData = new ArrayList<Double[]>();

            //
            // read the lines
            // 
            while ((str = inr.readLine()) != null) {
                lineCount++;
                str = str.trim();

                // ignore empty lines or comments, i.e. lines starting with either # or %
                // ----------------------------------------------------------------------
                if(str.startsWith("#") || str.startsWith("%") || str.length() == 0) {
                    continue;
                }

                if ("data".equals(str)) {
                    gotData = true;
                    continue;
                }

                if (gotData) {
                    // Read the data rows
                    // ------------------
                    String[] dataRowStr = str.split("[\\s,]+");
                    int len = dataRowStr.length;                   
                    Double[] dataRow = new Double[len];
                    boolean isTestData = false;
                    for (int i = 0; i < len; i++) {
                        if ("?".equals(dataRowStr[i])) {
                            isTestData = true;
                            dataRow[i] = -1.0d; // dummy value
                        } else {
                            dataRow[i] = Double.parseDouble(dataRowStr[i]);
                        }
                    }


                    // check if this row should be in the data
                    // or maybe in the validation set.
                    // A data point may not be in the both data
                    // set and validation set.
                    boolean inData = true;

                    if (isTestData) {
                        inData = false;
                        theTestData.add(dataRow);
                    }

                    // Get sample to use
                    // Keep all the rows where nextFloat is <= samplePCT
                    if (!isTestData && samplePCT > 0.0d) {
                        Random randomGenerator = new Random();
                        float rand = randomGenerator.nextFloat();
                        if (rand > samplePCT) {
                            inData = false;
                        }
                    }

                    // make validation set
                    if (!isTestData && validationPCT > 0.0d) {
                        Random randomGenerator = new Random();
                        float rand = randomGenerator.nextFloat();
                        if (rand < validationPCT) {
                            inData = false;
                            theValidationSet.add(dataRow);
                        }
                    }

                    // put in the data set
                    if (inData) {
                        theData.add(dataRow);
                    }

                } else {

                    // Check for parameters on the form
                    //    parameter: value(s)
                    // --------------------------------
                    if(str.contains(":")) {
                        String row[] = str.split(":\\s*");

                        // Now check each parameter
                        if ("return_type".equals(row[0])) {
                            returnType = row[1];

                        } else if ("presentation".equals(row[0])) {
                            presentation = row[1];
                            
                        } else if ("num_input_variables".equals(row[0])) {
                            numInputVariables = Integer.parseInt(row[1]);
                            
                        } else if ("num_rows".equals(row[0])) {
                            System.out.println("num_rows is not used anymore; it is calculated by the program.");
                            // numRows = Integer.parseInt(row[1]);
                            
                        } else if ("terminal_range".equals(row[0])) { 
                            String[] ranges = row[1].split("\\s+");
                            lowerRange = Double.parseDouble(ranges[0]); 
                            upperRange = Double.parseDouble(ranges[1]); 
                            
                        } else if ("terminal_wholenumbers".equals(row[0])) {
                          terminalWholeNumbers = Boolean.parseBoolean(row[1]);
                            
                        } else if ("max_init_depth".equals(row[0])) {
                            maxInitDepth = Integer.parseInt(row[1]);

                        } else if ("min_init_depth".equals(row[0])) {
                            minInitDepth = Integer.parseInt(row[1]);

                        } else if ("program_creation_max_tries".equals(row[0])) {
                            programCreationMaxTries = Integer.parseInt(row[1]);
                            
                        } else if ("population_size".equals(row[0])) {
                            populationSize = Integer.parseInt(row[1]); 
                            
                        } else if ("max_crossover_depth".equals(row[0])) {
                            maxCrossoverDepth = Integer.parseInt(row[1]);

                        } else if ("function_prob".equals(row[0])) {
                            functionProb = Double.parseDouble(row[1]);

                        } else if ("reproduction_prob".equals(row[0])) {
                            reproductionProb = Float.parseFloat(row[1]);

                        } else if ("mutation_prob".equals(row[0])) {
                            mutationProb = Float.parseFloat(row[1]);

                        } else if ("crossover_prob".equals(row[0])) {
                            crossoverProb = Float.parseFloat(row[1]);

                        } else if ("dynamize_arity_prob".equals(row[0])) {
                            dynamizeArityProb = Float.parseFloat(row[1]);

                        } else if ("new_chroms_percent".equals(row[0])) {
                            newChromsPercent = Double.parseDouble(row[1]);

                        } else if ("num_evolutions".equals(row[0])) {
                            numEvolutions = Integer.parseInt(row[1]); 
                            
                        } else if ("max_nodes".equals(row[0])) {
                            maxNodes = Integer.parseInt(row[1]);
                            
                        } else if ("functions".equals(row[0])) {
                            functions = row[1].split("[\\s,]+");

                        } else if ("adf_functions".equals(row[0])) {
                            adfFunctions = row[1].split("[\\s,]+");

                        } else if ("variable_names".equals(row[0])) {
                            variableNames = row[1].split("[\\s,]+");

                        } else if ("output_variable".equals(row[0])) {
                            outputVariable = Integer.parseInt(row[1]);

                        } else if ("ignore_variables".equals(row[0])) {
                            String[] ignoreVariablesS = row[1].split("[\\s,]+");
                            ignoreVariables = new int[ignoreVariablesS.length];
                            // TODO: make it a HashMap instead
                            for (int i = 0; i < ignoreVariablesS.length; i++) {
                                ignoreVariables[i] = Integer.parseInt(ignoreVariablesS[i]);
                            }

                        } else if ("constant".equals(row[0])) {
                            Double constant = Double.parseDouble(row[1]);
                            constants.add(constant);

                        } else if ("adf_arity".equals(row[0])) {
                            adfArity = Integer.parseInt(row[1]);
                            System.out.println("ADF arity " + adfArity);
                            if (adfArity > 0) {
                                useADF = true;
                            }

                        } else if ("adf_type".equals(row[0])) {
                            adfType = row[1];

                        } else if ("tournament_selector_size".equals(row[0])) {
                            tournamentSelectorSize = Integer.parseInt(row[1]);

                        } else if ("scale_error".equals(row[0])) {
                            scaleError = Double.parseDouble(row[1]);

                        } else if ("stop_criteria_fitness".equals(row[0])) {
                            stopCriteriaFitness = Double.parseDouble(row[1]);

                        } else if ("show_population".equals(row[0])) {
                            showPopulation = Boolean.parseBoolean(row[1]);

                        } else if ("show_similiar".equals(row[0]) || 
                                   "show_similar".equals(row[0])) {
                            // 2010-02-27:added alternative spelling
                            showSimiliar = Boolean.parseBoolean(row[1]);

                        } else if ("similiar_sort_method".equals(row[0]) || 
                                   "similar_sort_method".equals(row[0])) {
                            // 2010-02-27
                            similiarSortMethod = row[1];

                            if (! (
                                   "length".equals(similiarSortMethod) ||
                                   "occurrence".equals(similiarSortMethod))
                                ) {
                                System.out.println("Unknown similiar_sort_method: " + similiarSortMethod);
                                System.exit(1);
                            }


                        } else if ("show_progression".equals(row[0])) {
                            showProgression = Boolean.parseBoolean(row[1]);

                        } else if ("sample_pct".equals(row[0])) {
                            samplePCT = Float.parseFloat(row[1]);

                        } else if ("validation_pct".equals(row[0])) {
                            validationPCT = Float.parseFloat(row[1]);

                        } else if ("hits_criteria".equals(row[0])) {
                            hitsCriteria = Double.parseDouble(row[1]);
                            errorMethod = "hitsCriteria";

                        } else if ("show_all_generations".equals(row[0])) {
                            showAllGenerations = Boolean.parseBoolean(row[1]);

                        } else if ("strict_program_creation".equals(row[0])) {
                            strictProgramCreation = Boolean.parseBoolean(row[1]);

                        } else if ("no_command_gene_cloning".equals(row[0])) {
                            noCommandGeneCloning = Boolean.parseBoolean(row[1]);

                        } else if ("use_program_cache".equals(row[0])) {
                            useProgramCache = Boolean.parseBoolean(row[1]);

                        } else if ("mod_replace".equals(row[0])) {
                            // this is quite experimental
                            modReplace = Integer.parseInt(row[1]);

                        } else if ("show_results".equals(row[0])) {
                            showResults = Boolean.parseBoolean(row[1]);

                        } else if ("result_precision".equals(row[0])) {
                            resultPrecision = Integer.parseInt(row[1]);

                        } else if ("error_method".equals(row[0])) {
                            errorMethod = row[1];

                            if (! (
                                   "maxError".equals(errorMethod) ||
                                   "minError".equals(errorMethod) ||
                                   "medianError".equals(errorMethod) ||
                                   "meanError".equals(errorMethod) ||
                                   "totalError".equals(errorMethod))) {
                                System.out.println("Unknown errorMethod: " + errorMethod);
                                System.exit(1);
                            }

                        } else if ("no_terminals".equals(row[0])) {
                            // Added 2010-02-26
                            noTerminals = Boolean.parseBoolean(row[1]);

                        } else if ("make_time_series".equals(row[0])) {
                            makeTimeSeries = Boolean.parseBoolean(row[1]);

                        } else if ("make_time_series_with_index".equals(row[0])) {
                            makeTimeSeriesWithIndex = Boolean.parseBoolean(row[1]);

                        } else if ("min_nodes".equals(row[0])) {
                            // 2010-03-01: Added this and min_nodes_penalty
                            String[] opt = row[1].split("[\\s,]+");
                            minNodes = Integer.parseInt(opt[0]);

                            if (minNodes > maxNodes) {
                                System.out.println("minNodes (" + minNodes + ") >  maxNodes (" + maxNodes + ") which is weird. Cannot continue. "); 
                                System.exit(1);
                            }
                            minNodesPenalty = Integer.parseInt(opt[1]);

                        } else if ("alldifferent_variables".equals(row[0])) {
                            // added 2010-03-02
                            String[] opt = row[1].split("[\\s,]+");
                            alldifferentVariables = Boolean.parseBoolean(opt[0]);
                            alldifferentVariablesPenalty = Double.parseDouble(opt[1]);
                        // } else if ("make_index_seq".equals(row[0])) {
                        // makeIndexSeq = Boolean.parseBoolean(row[1]);

                        } else {                            
                            System.out.println("Unknown keyword: " + row[0] + " on line " + lineCount);
                            System.exit(1);

                        }
                    }

                } // end if(gotData)

            } // end while

            inr.close();

            // 2010-02-27: if makeTimeSeries is set we
            // makes a timeseries of the first data row.
            // It is undocumented what will happen with more than one
            // data rows.
            //
            // The lag is the number of input variables + 1.
            // 
            if (makeTimeSeries || makeTimeSeriesWithIndex) {

                // last we substitute theData with this
                ArrayList<Double[]> theDataNew = new ArrayList<Double[]>();

                Double[] dat = theData.get(0);
                int numElements = dat.length;
                System.out.println("Making timeseries, #elements: " + numElements);
                int cols = numInputVariables + 1;
                if (makeTimeSeriesWithIndex) {
                    cols++;
                }
                for (int i = 0; i < numElements - cols; i++) {
                    Double[] tmp = new Double[cols];
                    int jStart = 0;
                    if (makeTimeSeriesWithIndex) {
                        System.out.print(i+1 + " ");
                        tmp[0] = (double)i+1;
                        jStart = 1;
                    }
                    for (int j = jStart; j < cols; j++) {
                        System.out.print(dat[i+j] + " ");
                        tmp[j] = dat[i+j];
                    }
                    System.out.println();
                    theDataNew.add(tmp);
                }
                
                theData = theDataNew;
            }


            //
            // Now we know everything to be known.
            // Construct the matrix from the file.
            // -----------------------------------
            int r = theData.size();
            int c = theData.get(0).length;
            int numIgnore = 0;
            if (ignoreVariables != null) {
                // TODO: ignoreVariables should be a HashMap
                numIgnore = ignoreVariables.length;
                // c = c - numIgnore;
            }
            Double[][] dataTmp = new Double[r][c];

            // TODO: ignore the variables in ignoreVariables
            for(int i = 0; i < r; i++) {
                Double[] this_row = theData.get(i);
                for(int j = 0; j < c; j++) {
                    dataTmp[i][j] = this_row[j];
                }
            }

            // Since we calculate the error on the variable we
            // must transpose the data matrix
            // -----------------------------------------------
            data = transposeMatrix(dataTmp);
            numRows = data[0].length;
            System.out.println("It was " + numRows + " data rows");


            //
            // Do the same thing with the validation data
            //
            // TODO: This should really be a separate method
            if (validationPCT > 0.0d && theValidationSet != null && theValidationSet.size() > 0) {
                int r_v = theValidationSet.size();
                int c_v = theValidationSet.get(0).length;
                Double[][] dataTmp_v = new Double[r_v][c_v];
                
                // TODO: ignore the variables in ignoreVariables
                for(int i = 0; i < r_v; i++) {
                    Double[] this_row = theValidationSet.get(i);
                    for(int j = 0; j < c_v; j++) {
                        dataTmp_v[i][j] = this_row[j];
                    }
                }
                validationSet = transposeMatrix(dataTmp_v);
                System.out.println("It was " + validationSet[0].length  + " data rows in the validation data set");
            }


            //
            // Do the same thing with the test data
            //
            if (theTestData.size() > 0) {
                int r_v = theTestData.size();
                int c_v = theTestData.get(0).length;
                Double[][] dataTmp_v = new Double[r_v][c_v];
                
                // TODO: ignore the variables in ignoreVariables
                for(int i = 0; i < r_v; i++) {
                    Double[] this_row = theTestData.get(i);
                    for(int j = 0; j < c_v; j++) {
                        dataTmp_v[i][j] = this_row[j];
                    }
                }
                // testData = transposeMatrix(dataTmp_v);
                testData = dataTmp_v;
                System.out.println("It was " + testData.length  + " data rows in the user defined data set");
            }


        } catch (IOException e) {
            System.out.println(e);
            System.exit(1);
        }
  
    } // end readFile


    //
    // Transpose matrix
    // ----------------
    public static Double [][] transposeMatrix(Double [][] m){
        int r = m.length;
        int c = m[0].length;
        Double [][] t = new Double[c][r];
        for(int i = 0; i < r; ++i){
            for(int j = 0; j < c; ++j){
                t[j][i] = m[i][j];
            }
        }
        return t;

    } // end transposeMatrix


    /*
     *  makeCommands:
     *  makes the CommandGene array given the function listed in the
     *  configurations file
     *  ------------------------------------------------------------
     */
    static CommandGene[] makeCommands(GPConfiguration conf, String[] functions, Double lowerRange, Double upperRange, String type) {
        ArrayList<CommandGene> commandsList = new ArrayList<CommandGene>();
        int len = functions.length;
        boolean isADF = "ADF".equals(type);

        try {
            for(int i = 0; i < len; i++) {

                //
                // Note: all functions may not be applicable...
                //
                if ("Multiply".equals(functions[i])) {
                    commandsList.add(new Multiply(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new Multiply(conf, CommandGene.BooleanClass));
                    }

                } else if ("Multiply3".equals(functions[i])) {
                    commandsList.add(new Multiply3(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new Multiply3(conf, CommandGene.BooleanClass));
                    }

                } else if ("Add".equals(functions[i])) {
                    commandsList.add(new Add(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new Add(conf, CommandGene.BooleanClass));
                    }

                } else if ("Divide".equals(functions[i])) {
                    commandsList.add(new Divide(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new Divide(conf, CommandGene.BooleanClass));
                    }

                } else if ("DivideIntD".equals(functions[i])) {
                    commandsList.add(new DivideIntD(conf, CommandGene.DoubleClass));

                } else if ("DivideProtected".equals(functions[i])) {
                    commandsList.add(new DivideProtected(conf, CommandGene.DoubleClass));

                } else if ("Add3".equals(functions[i])) {
                    commandsList.add(new Add3(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new Add3(conf, CommandGene.BooleanClass));
                    }

                } else if ("Add4".equals(functions[i])) {
                    commandsList.add(new Add4(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new Add4(conf, CommandGene.BooleanClass));
                    }

                } else if ("Subtract".equals(functions[i])) {
                    commandsList.add(new Subtract(conf, CommandGene.DoubleClass));
                    if (useADF && "boolean".equals(adfType)) {
                        commandsList.add(new Subtract(conf, CommandGene.BooleanClass));
                    }

                } else if ("Sine".equals(functions[i])) {
                    commandsList.add(new Sine(conf, CommandGene.DoubleClass));

                } else if ("ArcSine".equals(functions[i])) {
                    commandsList.add(new ArcSine(conf, CommandGene.DoubleClass));

                } else if ("Tangent".equals(functions[i])) {
                    commandsList.add(new Tangent(conf, CommandGene.DoubleClass));

                } else if ("ArcTangent".equals(functions[i])) {
                    commandsList.add(new ArcTangent(conf, CommandGene.DoubleClass));

                } else if ("Cosine".equals(functions[i])) {
                    commandsList.add(new Cosine(conf, CommandGene.DoubleClass));

                } else if ("ArcCosine".equals(functions[i])) {
                    commandsList.add(new ArcCosine(conf, CommandGene.DoubleClass));

                } else if ("Exp".equals(functions[i])) {
                    commandsList.add(new Exp(conf, CommandGene.DoubleClass));

                } else if ("Log".equals(functions[i])) {
                    commandsList.add(new Log(conf, CommandGene.DoubleClass));

                } else if ("Abs".equals(functions[i])) {
                    commandsList.add(new Abs(conf, CommandGene.DoubleClass));

                } else if ("Pow".equals(functions[i])) {
                    commandsList.add(new Pow(conf, CommandGene.DoubleClass));

                } else if ("Round".equals(functions[i])) {
                    commandsList.add(new Round(conf, CommandGene.DoubleClass));

                } else if ("RoundD".equals(functions[i])) {
                    commandsList.add(new RoundD(conf, CommandGene.DoubleClass));

                } else if ("Ceil".equals(functions[i])) {
                    commandsList.add(new Ceil(conf, CommandGene.DoubleClass));

                } else if ("Floor".equals(functions[i])) {
                    commandsList.add(new Floor(conf, CommandGene.DoubleClass));

                } else if ("Modulo".equals(functions[i])) {
                    commandsList.add(new Modulo(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new Modulo(conf, CommandGene.BooleanClass));
                    }

                } else if ("ModuloD".equals(functions[i])) {
                    commandsList.add(new ModuloD(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new ModuloD(conf, CommandGene.BooleanClass));
                    }

                } else if ("ModuloReplaceD".equals(functions[i])) {
                    
                    // this is quite experimental
                    commandsList.add(new ModuloReplaceD(conf, CommandGene.DoubleClass, modReplace));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new ModuloReplaceD(conf, CommandGene.BooleanClass, modReplace));
                    }

                } else if ("Max".equals(functions[i])) {
                    commandsList.add(new Max(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new Max(conf, CommandGene.BooleanClass));
                    }

                } else if ("Min".equals(functions[i])) {
                    commandsList.add(new Min(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new Min(conf, CommandGene.BooleanClass));
                    }

                } else if ("Sqrt".equals(functions[i])) {
                    // Note: This uses my Sqrt.java file
                    commandsList.add(new Sqrt(conf, CommandGene.DoubleClass));

                } else if ("Square".equals(functions[i])) {
                    // Note: This uses my Square.java file
                    commandsList.add(new Square(conf, CommandGene.DoubleClass));

                } else if ("Cube".equals(functions[i])) {
                    // Note: This uses my Cube.java file
                    commandsList.add(new Cube(conf, CommandGene.DoubleClass));

                } else if ("Logistic".equals(functions[i])) {
                    // Note: This uses my Logistic.java file
                    commandsList.add(new Logistic(conf, CommandGene.DoubleClass));

                } else if ("Gaussian".equals(functions[i])) {
                    // Note: This uses my Gaussian.java file
                    commandsList.add(new Gaussian(conf, CommandGene.DoubleClass));

                } else if ("Sigmoid".equals(functions[i])) {
                    // Note: This uses my Sigmoid.java file
                    commandsList.add(new Sigmoid(conf, CommandGene.DoubleClass));

                } else if ("Gamma".equals(functions[i])) {
                    // Note: This uses my Gamma.java file
                    commandsList.add(new Gamma(conf, CommandGene.DoubleClass));

                } else if ("Step".equals(functions[i])) {
                    // Note: This uses my Step.java file
                    commandsList.add(new Step(conf, CommandGene.DoubleClass));

                } else if ("Sign".equals(functions[i])) {
                    // Note: This uses my Sign.java file
                    commandsList.add(new Sign(conf, CommandGene.DoubleClass));

                } else if ("Hill".equals(functions[i])) {
                    // Note: This uses my Hill.java file
                    commandsList.add(new Hill(conf, CommandGene.DoubleClass));

                } else if ("LesserThan".equals(functions[i])) {
                    commandsList.add(new LesserThan(conf, CommandGene.BooleanClass));

                } else if ("LesserThanD".equals(functions[i])) {
                    commandsList.add(new LesserThanD(conf, CommandGene.DoubleClass));

                } else if ("LesserThanOrEqualD".equals(functions[i])) {
                    commandsList.add(new LesserThanOrEqualD(conf, CommandGene.DoubleClass));

                } else if ("GreaterThan".equals(functions[i])) {
                    commandsList.add(new GreaterThan(conf, CommandGene.BooleanClass));

                } else if ("GreaterThanD".equals(functions[i])) {
                    commandsList.add(new GreaterThanD(conf, CommandGene.DoubleClass));

                } else if ("GreaterThanOrEqualD".equals(functions[i])) {
                    commandsList.add(new GreaterThanOrEqualD(conf, CommandGene.DoubleClass));

                } else if ("DifferentD".equals(functions[i])) {
                    commandsList.add(new DifferentD(conf, CommandGene.DoubleClass));

                } else if ("If".equals(functions[i])) {
                    // Note: This is just If on DoubleClass, not a proper Boolean
                    commandsList.add(new If(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new If(conf, CommandGene.BooleanClass));
                    }

                } else if ("IfElse".equals(functions[i])) {
                    commandsList.add(new IfElse(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new IfElse(conf, CommandGene.BooleanClass));
                    }

                } else if ("IfElseD".equals(functions[i])) {
                    commandsList.add(new IfElseD(conf, CommandGene.DoubleClass));


                } else if ("IfLessThanOrEqualD".equals(functions[i])) {
                    // if a < b then c else d
                    commandsList.add(new IfLessThanOrEqualD(conf, CommandGene.DoubleClass));

                } else if ("IfLessThanOrEqualZeroD".equals(functions[i])) {
                    // 2010-02-22: Added IfLessThanOrEqualZeroD
                    // if a < 0 then b else c
                    commandsList.add(new IfLessThanOrEqualZeroD(conf, CommandGene.DoubleClass));

                } else if ("IfDyn".equals(functions[i])) {
                    // Well, this don't work as expected...
                    // System.out.println("IfDyn is not supported yet");
                    commandsList.add(new IfDyn(conf, CommandGene.DoubleClass,1,1,5));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new IfDyn(conf, CommandGene.DoubleClass,1,1,5));
                    }
               // } else if ("IfDynD".equals(functions[i])) {
               //     // Using IfDynD.java
               //     commandsList.add(new IfDynD(conf, CommandGene.DoubleClass,3,1,3));

                } else if ("Loop".equals(functions[i])) { 
                    // experimental
                    commandsList.add(new Loop(conf, CommandGene.DoubleClass, 3));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new Loop(conf, CommandGene.BooleanClass, 3));
                    }

                } else if ("LoopD".equals(functions[i]))  {
                    // experimental
                    commandsList.add(new LoopD(conf, CommandGene.DoubleClass, numInputVariables));

                } else if ("Equals".equals(functions[i])) {
                    // commandsList.add(new Equals(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new Equals(conf, CommandGene.BooleanClass));
                    }

                } else if ("EqualsD".equals(functions[i])) {
                    commandsList.add(new EqualsD(conf, CommandGene.DoubleClass));

                } else if ("ForXLoop".equals(functions[i])) {
                    // experimental
                    commandsList.add(new ForXLoop(conf, CommandGene.IntegerClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new ForXLoop(conf, CommandGene.BooleanClass));
                    } else if (useADF && "integer".equals(adfType)) {
                        commandsList.add(new ForXLoop(conf, CommandGene.IntegerClass));
                    }

                } else if ("ForLoop".equals(functions[i])) {
                    // experimental
                    commandsList.add(new ForLoop(conf, CommandGene.DoubleClass,1,numInputVariables));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new ForLoop(conf, CommandGene.BooleanClass,10));
                    } else if (isADF && "integer".equals(adfType)) {
                        commandsList.add(new ForLoop(conf, CommandGene.IntegerClass,10));
                    }

                } else if ("ForLoopD".equals(functions[i]))  {
                    // added 2010-03-01
                    // ForLoopD.java
                    commandsList.add(new ForLoopD(conf, CommandGene.DoubleClass, numInputVariables*2));

                } else if ("Increment".equals(functions[i])) {
                    commandsList.add(new Increment(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new Increment(conf, CommandGene.BooleanClass));
                    }

                } else if ("Argument".equals(functions[i])) {
                    // experimental
                    /*
                    commandsList.add(new Argument(conf, 1, CommandGene.DoubleClass));
                    if ("boolean".equals(adfType)) {
                        commandsList.add(new Argument(conf, 1, CommandGene.BooleanClass));
                    }
                    */

                } else if ("StoreTerminal".equals(functions[i])) {
                    // experimental
                    commandsList.add(new StoreTerminal(conf, "dmem0", CommandGene.DoubleClass));
                    commandsList.add(new StoreTerminal(conf, "dmem1", CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new StoreTerminal(conf, "bmem0", CommandGene.DoubleClass));
                        commandsList.add(new StoreTerminal(conf, "bmem1", CommandGene.DoubleClass));
                    }


                } else if ("Pop".equals(functions[i])) {
                    // experimental
                    // commandsList.add(new Pop(conf, CommandGene.DoubleClass));
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new Pop(conf, CommandGene.BooleanClass));
                    }

                } else if ("Push".equals(functions[i])) {
                    // experimental
                    commandsList.add(new Push(conf, CommandGene.DoubleClass));

                } else if ("And".equals(functions[i])) {
                    commandsList.add(new And(conf));

                } else if ("Or".equals(functions[i])) {
                    commandsList.add(new Or(conf));

                } else if ("Xor".equals(functions[i])) {
                    commandsList.add(new Xor(conf));

                } else if ("Not".equals(functions[i])) {
                    commandsList.add(new Not(conf));

                } else if ("AndD".equals(functions[i])) {
                    commandsList.add(new AndD(conf));

                } else if ("OrD".equals(functions[i])) {
                    commandsList.add(new OrD(conf));

                } else if ("XorD".equals(functions[i])) {
                    commandsList.add(new XorD(conf));

                } else if ("NotD".equals(functions[i])) {
                    commandsList.add(new NotD(conf));

                } else if ("Replace".equals(functions[i])) {
                    commandsList.add(new Replace(conf));

                } else if ("Id".equals(functions[i])) {
                    // experimental
                    commandsList.add(new Id(conf));

                } else if ("SubProgram".equals(functions[i])) {
                    // experimental
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new SubProgram(conf,new Class[]{CommandGene.BooleanClass, CommandGene.BooleanClass}));
                        commandsList.add(new SubProgram(conf,new Class[]{CommandGene.BooleanClass, CommandGene.BooleanClass,CommandGene.BooleanClass}));
                    }
                    commandsList.add(new SubProgram(conf,new Class[]{CommandGene.DoubleClass, CommandGene.DoubleClass}));
                    commandsList.add(new SubProgram(conf,new Class[]{CommandGene.DoubleClass, CommandGene.DoubleClass,CommandGene.DoubleClass}));

                } else if ("Tupel".equals(functions[i])) {
                    // experimental
                    if (isADF && "boolean".equals(adfType)) {
                        commandsList.add(new Tupel(conf,new Class[]{CommandGene.BooleanClass, CommandGene.BooleanClass}));
                    }

                } else {
                    System.out.println("Unkown function: " + functions[i]);
                    System.exit(1);

                }
            }
            
            if (!noTerminals) {
                commandsList.add(new Terminal(conf, CommandGene.DoubleClass, lowerRange, upperRange, terminalWholeNumbers));
                // commandsList.add(new Terminal(conf, CommandGene.BooleanClass, lowerRange, upperRange, terminalWholeNumbers));
            }

            // ADF
            // Just add the ADF to the "normal" command list (i.e. not to the ADF list)
            if (useADF && !"ADF".equals(type)) {
                commandsList.add(new ADF(conf, 1, adfArity));
                // for (int i = 0; i <= adfArity; i++) {
                //    commandsList.add(new ADF(conf, i, adfArity));
                // }
            }

            if (constants != null) {
                for (int i = 0; i < constants.size(); i++) {
                    Double constant = constants.get(i);
                    commandsList.add(new Constant(conf, CommandGene.DoubleClass, constant));
                }
            }
        } catch (Exception e) {
            System.out.println(e);

        }

        CommandGene[] commands = new CommandGene[commandsList.size()];
        commandsList.toArray(commands);
        
        return commands;
        
    }

    /**
     * Starts the example.
     *
     * @author Hakan Kjellerstrand
     */
    public static void main(String[] args)
        throws Exception {

        // Use the log4j configuration
        // Log to stdout instead of file
        // -----------------------------
        org.apache.log4j.PropertyConfigurator.configure("log4j.properties"); 
        LOGGER.addAppender(new ConsoleAppender(new SimpleLayout(),"System.out"));


        //
        // Read a configuration file, or not...
        //
        if (args.length > 0) {
            readFile(args[0]);

        } else {

            // Default problem 
            // Fibonacci series, with three input variables to make it 
            // somewhat harder.
            // -------------------------------------------------------
            numRows = 21;
            numInputVariables = 3;

            // Note: The last array is the output array
            int[][] indata = { 
                {1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946},
                {1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711},
                {2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657},
                {3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368}
            };

            data = new Double[numInputVariables+1][numRows];
            for (int i = 0; i < numInputVariables+1; i++) {
                for(int j = 0; j < numRows; j++) {
                    data[i][j] = new Double(indata[i][j]);
                }
            }
            populationSize = 100;
            numEvolutions = 100;
            functions = "Multiply,Divide,Add,Subtract".split(",");
            variableNames = "F1,F2,F3,F4".split(",");

            presentation = "Fibonacci series";

        }


        // Present the problem
        // -------------------
        System.out.println("Presentation: " + presentation);

        if (outputVariable == null) {
            outputVariable = numInputVariables;
        }

        if (variableNames == null) {
            variableNames = new String[numInputVariables+1];
            for(int i = 0; i < numInputVariables+1; i++) {
                variableNames[i] = "V" + (i+1);
            }
        }

        System.out.println("output_variable: " + variableNames[outputVariable] + " (index: " + outputVariable + ")" );


        // Setup the algorithm's parameters.
        // ---------------------------------
        GPConfiguration config = new GPConfiguration();

        // We use a delta fitness evaluator because we compute a defect rate, not
        // a point score!
        // ----------------------------------------------------------------------
        config.setGPFitnessEvaluator(new DeltaGPFitnessEvaluator());
        config.setMaxInitDepth(maxInitDepth);
        config.setPopulationSize(populationSize);

        // experimental
        // config.setUseProgramCache(true);

        // Default selectionMethod is is TournamentSelector(3)
        if (tournamentSelectorSize > 0) {
            config.setSelectionMethod(new TournamentSelector(tournamentSelectorSize));
        }

        /**
         * The maximum depth of an individual resulting from crossover.
         */
        config.setMaxCrossoverDepth(maxCrossoverDepth);
        config.setFitnessFunction(new SymbolicRegression.FormulaFitnessFunction());


        // Experimental
        // config.setNodeValidator(new SRNodeValidator());

        /**
         * @param a_strict true: throw an error during evolution in case a situation
         * is detected where no function or terminal of a required type is declared
         * in the GPConfiguration; false: don't throw an error but try a completely
         * different combination of functions and terminals
         */
        config.setStrictProgramCreation(strictProgramCreation);

        
        /**
         * Decide whether to clone command genes when creating a new GP program in
         * ProgramChromosome.
         */
        config.setNoCommandGeneCloning(noCommandGeneCloning);


        config.setUseProgramCache(useProgramCache);

        /**
         * In crossover: If random number (0..1) < this value, then choose a function
         * otherwise a terminal.
         */
        config.setFunctionProb(functionProb);

        /**
         * The probability that a reproduction operation is chosen during evolution.
         * Must be between 0.0d and 1.0d. crossoverProb + reproductionProb must equal
         * 1.0d.
         */
        config.setReproductionProb(reproductionProb);

        config.setCrossoverProb(crossoverProb);

        /**
         * The probability that a node is mutated during growing a program.
         */
        config.setMutationProb(mutationProb);

        /**
         * The probability that the arity of a node is changed during growing a
         * program.
         */
        config.setDynamizeArityProb(dynamizeArityProb);
        /**
         * Percentage of the population that will be filled with new individuals
         * during evolution. Must be between 0.0d and 1.0d.
         */
        config.setNewChromsPercent(newChromsPercent);

        /**
         * The minimum depth of an individual when the world is created.
         */
        config.setMinInitDepth(minInitDepth);

        /**
         * If m_strictProgramCreation is false: Maximum number of tries to construct
         * a valid program.
         */
        config.setProgramCreationMaxTries(programCreationMaxTries);


        GPProblem problem = new SymbolicRegression(config);

        // Create the genotype of the problem, i.e., define the GP commands and
        // terminals that can be used, and constrain the structure of the GP
        // program.
        // --------------------------------------------------------------------
        GPGenotype gp = problem.create();
        // gp.setVerboseOutput(true);
        gp.setVerboseOutput(false);


        startTime = System.currentTimeMillis();


        System.out.println("Creating initial population");
        // System.out.println("Mem free: " + SystemKit.niceMemory(SystemKit.getTotalMemoryMB()) + " MB");

        IGPProgram fittest = null;
        double bestFit = -1.0d;
        String bestProgram = "";
        int bestGen = 0;
        HashMap<String,Integer> similiar = null;
        if (showSimiliar) {
            similiar = new HashMap<String,Integer>();
        }

        int numEvolutions2 = numEvolutions;
        if (stopCriteriaFitness >= 0.0d) {
            // basically forever
            numEvolutions2 = Integer.MAX_VALUE;
        }
        int gen = 0;
        for (gen = 0; gen < numEvolutions2; gen++) {

            gp.evolve(); // evolve one generation
            gp.calcFitness();
            GPPopulation pop = gp.getGPPopulation();

            IGPProgram thisFittest = pop.determineFittestProgram();

            ProgramChromosome chrom = thisFittest.getChromosome(0);
            String program = chrom.toStringNorm(0);
            double fitness = thisFittest.getFitnessValue();

            if (showSimiliar || showPopulation || showAllGenerations) {
                if (showPopulation || showAllGenerations) {
                    System.out.println("Generation " + gen);
                }

                pop.sortByFitness();
                for (IGPProgram p : pop.getGPPrograms()) {
                    double fit = p.getFitnessValue();
                    if (showSimiliar && fit <= bestFit) {
                        String prog = p.toStringNorm(0);
                        if (!similiar.containsKey(prog)) {
                            similiar.put(prog, 1);
                        } else {
                            similiar.put(prog, similiar.get(prog)+1);
                        }
                    } 

                    if (showPopulation) {
                        String prg = p.toStringNorm(0);
                        int sz = p.size();
                        System.out.println("\tprogram: " + prg + " fitness: " + fit);
                    }
                }
            }

            //
            // Yes, I have to think more about this....
            // Right now a program is printed if it has
            // better fitness value than the former best solution.

            // if (gen % 25 == 0) {
            //    myOutputSolution(fittest, gen);
            // }
            if (bestFit < 0.0d || fitness < bestFit || showAllGenerations) {
                if (bestFit < 0.0d || fitness < bestFit) {
                    bestGen = gen;
                    bestFit = fitness;
                    bestProgram = program;
                    fittest = thisFittest;
                    if (showSimiliar) {
                        // reset the hash
                        similiar.clear(); // = new HashMap<String,Integer>();
                        similiar.put(thisFittest.toStringNorm(0),1);
                    }
                }
                myOutputSolution(fittest, gen);
                // Ensure that the best solution is in the population.
                // gp.addFittestProgram(thisFittest); 
            } else {
                /*
                  if (gen % 25 == 0 && gen != numEvolutions) {
                    System.out.println("Generation " + gen + " (This is a keep alive message.)");
                    // myOutputSolution(fittest, gen);
                }
                */
                if (showProgression) {
                    String genStr = "" + (gen-1);
                    for (int i = 0; i <= genStr.length(); i++) {
                        System.out.print("\b");
                    }
                    System.out.print("" + gen);
                }


            }

            if (stopCriteriaFitness >= 0 && fitness <= stopCriteriaFitness) {
                System.out.print("\nFitness stopping criteria (" + stopCriteriaFitness + ") reached with fitness " + fitness + " at generation " + gen + "\n");
                break;
            }
        }

        // Print the best solution so far to the console.
        // ----------------------------------------------
        // gp.outputSolution(gp.getAllTimeBest());

        System.out.println("\nAll time best (from generation " + bestGen + ")");
        myOutputSolution(fittest, gen);

        // Create a graphical tree of the best solution's program and write 
        // it to a PNG file.
        // ----------------------------------------------------------------
        // problem.showTree(gp.getAllTimeBest(), "mathproblem_best.png");

        endTime = System.currentTimeMillis();
        long elapsedTime = endTime - startTime;
        String elapsed = String.format("%5.2f", (endTime - startTime)/1000.0f);
        System.out.println("\nTotal time " + elapsed + "s");

        if (showSimiliar) {
            System.out.println("\nAll solutions with the best fitness (" + bestFit + "):");
            System.out.println("Sort method: " + similiarSortMethod);

            // 2010-02-27:
            // sort according to descending number of occurrences or length of string
            List<String> sorted = new ArrayList<String>(similiar.keySet());
            // must be final in order to be used in compare
            final HashMap<String,Integer> sim = similiar;
            Collections.sort(sorted, new Comparator<String>() {
                    public int compare(String s1, String s2) {
                        if ("length".equals(similiarSortMethod)) {
                            // descending occurrences
                            return s1.length() - s2.length();
                        } else {
                            // descending length of programs
                            return sim.get(s2) - sim.get(s1); 
                        }
                    }
                });

            for(String p : sorted) {
                System.out.println(p + " [" + similiar.get(p) + "]");
            }

            System.out.println("It was " + similiar.size() + " different solutions with fitness " + bestFit);
        }

        if (testData != null && testData.length > 0) {
            System.out.println("\nTesting the fittest program with user defined test data: ");
            int testDataSize = testData.length;
            for (int i = 0; i < testDataSize; i++) {
                for (int j = 0; j < testData[i].length; j++) {
                    if (j != outputVariable) {
                        System.out.print(testData[i][j] + " ");
                    }
                }
                // System.out.println();
                Double testResult = evalData(fittest, testData[i]);
                System.out.println("   Result: " + testResult);
            }
        }

        // 2010-02-22: Print the result of the validation set
        if (validationSet != null && validationSet.length > 0) {
            System.out.println("\nTesting the fittest program with the validation set: ");
            int validationDataSize = validationSet.length;          
            for (int i = 0; i < validationSet[0].length; i++) {
                Double[] val = new Double[validationDataSize];
                for (int j = 0; j < validationDataSize; j++) {
                    val[j] = validationSet[j][i];
                    System.out.print(val[j] + " ");
                }
                Double testResult = evalData(fittest, val);
                double diff = Math.abs(testResult - val[outputVariable]);
                System.out.println("   Result: " + testResult + " should be " + val[outputVariable] + " diff: " + diff);
            }
        }

            
    } // end main
    

    /**
     * Fitness function for evaluating the produced fomulas, represented as GP
     * programs. The fitness is computed by calculating the result (Y) of the
     * function/formula for integer inputs (X's). The sum of the differences
     * between expected Y and actual Y is the fitness, the lower the better (as
     * it is a defect rate here).
     */
    public static class FormulaFitnessFunction
        extends GPFitnessFunction {
        protected double evaluate(final IGPProgram a_subject) {
            return computeRawFitness(a_subject);
        }
        
        public double computeRawFitness(final IGPProgram ind) {
            double error = 0.0d;
            Object[] noargs = new Object[0];

            // 2010-03-01: Allow "penalty"
            // If we have requirements of size etc we will
            // penalty programs with smaller size.

            // normally we don't penalty anything
            double penalty = 0.0;

            ProgramChromosome chrom = ind.getChromosome(0);
            int numTerms =  chrom.numFunctions()  + chrom.numTerminals();

            // penalty if the terms are less than minNodes
            if (minNodes >= 0) {
                if (numTerms < minNodes) {
                    penalty += Math.abs(numTerms - minNodes)*minNodesPenalty; 
                }
            }

            // added 2010-03-20
            // for alldifferent type of constraints
            // (Note: This could surely be done more efficient.)
            if (alldifferentVariables) {
                CommandGene[] functions = chrom.getFunctions();
                HashMap<String,Integer> countVariables = new HashMap<String,Integer>();
                for (int i = 0; i < functions.length; i++) {
                    CommandGene func = functions[i];
                    
                    if (func != null) {
                        int arity = func.getArity(ind);
                        String funcStr = func.toString();
                        if (arity == 0) {
                            Class returnType = func.getReturnType();
                            int subReturnType = func.getSubReturnType();
                            
                            // It is a terminal
                            if (!countVariables.containsKey(funcStr)) {
                                countVariables.put(funcStr, 1);
                            } else {
                                // this variable is already seen so we
                                // punish this program
                                penalty += alldifferentVariablesPenalty;
                                countVariables.put(funcStr, countVariables.get(funcStr)+1);
                            }
                        }
                    }
                }
            }

            // Evaluate function for the input numbers
            // ---------------------------------------
            int thisNumHits = 0;
            double[] results  =  new double[numRows];
            double[] errors   = new double[numRows];
            for (int j = 0; j < numRows; j++) {

                // Provide the variable X with the input number.
                // See method create(), declaration of "nodeSets" for where X is
                // defined.
                // -------------------------------------------------------------

                // set all the input variables
                int variableIndex = 0;
                for(int i = 0; i < numInputVariables+1; i++) {
                    if (i != outputVariable) {
                        variables[variableIndex].set(data[i][j]);
                        variableIndex++;
                    }
                }
                try {

                    double result = ind.execute_double(0, noargs);
                    results[j] = result;

                    // Sum up the error between actual and expected result 
                    // to get a defect rate.
                    // ---------------------------------------------------

                    // original:
                    double res = data[outputVariable][j];
                    double diff = Math.abs(result - res) + penalty;
                    // diff = Math.sqrt(Math.abs(result*result - res*res)); 

                    errors[j] = diff;
                    
                    // double r = data[outputVariable][j];


                    if (hitsCriteria >= 0.0) {
                        // count the number of non hits
                        if (Double.isInfinite(result) || Double.isNaN(result) || Double.isInfinite(diff) || Double.isNaN(diff) || diff > hitsCriteria) {
                            error += 1.0 + penalty;
                        }
                    } else {
                        error += diff;
                    }

                    if (diff <= hitsCriteria) {
                        thisNumHits++;
                    }

                    // If the error is too high, stop evaluation and return 
                    // worst error possible.
                    // ----------------------------------------------------
                    if (Double.isInfinite(error) || Double.isNaN(error)) {
                        return Double.MAX_VALUE;
                    }
                } catch (ArithmeticException ex) {
                    // This should not happen, some illegal operation was 
                    // executed.
                    // ----------------------------------------------------
                    System.out.println(ind);
                    throw ex;
                }
            }

            // 2010-02-22: Calculate different types of errors
            double[] errs = calcAllErrors(error, errors);
            if (hitsCriteria < 0) {
                error = errs[0];
            }
            double totalError  = errs[1]; 
            double minError    = errs[2];
            double maxError    = errs[3];
            double meanError   = errs[4];
            double medianError = errs[5];

            double correlation = correlation(data[outputVariable], results, results.length);
            ApplicationData appData;
            if (showResults) {
                appData = new ApplicationData(error, correlation, thisNumHits, minError, maxError, meanError, medianError, totalError, results);
            } else {
                appData = new ApplicationData(error, correlation, thisNumHits, minError, maxError, meanError, medianError, totalError);
            }
            ind.setApplicationData(appData);

            if (scaleError > 0.0d) {

                return error*scaleError;

            } else {

                return error;

            }

        }
    }

    protected static class ApplicationData {
        double error = 1000.0d;
        int numHits = 0;
        double correlation = 0.0d;
        double[] results;
        double minError = Double.MIN_VALUE;
        double maxError = Double.MAX_VALUE;
        double meanError = Double.MAX_VALUE;
        double medianError = Double.MAX_VALUE;
        double totalError = Double.MAX_VALUE;

        ApplicationData(double _error, double _correlation, int _numHits, double _minError, double _maxError, double _meanError, double _medianError, double _totalError) {
            error       = _error;
            correlation = _correlation;
            numHits     = _numHits;
            minError    = _minError;
            meanError   = _meanError;
            maxError    = _maxError;
            medianError = _medianError;
            totalError  = _totalError;
        }

        ApplicationData(double _error, double _correlation, int _numHits, double _minError, double _maxError, double _meanError, double _medianError, double _totalError, double[] _results) {
            error       = _error;
            correlation = _correlation;
            numHits     = _numHits;
            minError    = _minError;
            meanError   = _meanError;
            maxError    = _maxError;
            medianError = _medianError;
            totalError  = _totalError;
            results     = _results;
        }


        private void setError(double _error) { error = _error; }
        private void setCorrelation(double _correlation) { correlation = _correlation; }
        private void setNumHits(int _numHits) { numHits = _numHits; }
        private void setResults(double[] _results) { results = _results; }
        private void setMinError(double _minError) { minError = _minError; }
        private void setMeanError(double _meanError) { meanError = _meanError; }
        private void setMaxError(double _maxError) { maxError = _maxError; }
        private void setMedianError(double _medianError) { medianError = _medianError; }
        private void setTotalError(double _totalError) { totalError = _totalError; }


        private double getError() { return(error); }
        private double getCorrelation() { return(correlation); }
        private int getNumHits() { return(numHits); }
        private double[] getResults() { return(results); }
        private double getMinError() { return(minError); }
        private double getMeanError() { return(meanError); }
        private double getMaxError() { return(maxError); }
        private double getMedianError() { return(medianError); }
        private double getTotalError() { return(totalError); }
    }

 /**
   * Outputs the best solution until now at standard output.
   *
   * This is stolen (and somewhat edited) from GPGenotype.outputSolution
   * which used log4j.
   *
   * @param a_best the fittest ProgramChromosome
   *
   * @author Hakan Kjellerstrand (originally by Klaus Meffert)
   */
    public static void myOutputSolution(IGPProgram a_best, int gen) {

      String freeMB = SystemKit.niceMemory(SystemKit.getFreeMemoryMB());
      long now = System.currentTimeMillis();
      String elapsedNow = String.format("%5.2f", (now - startTime)/1000.0f);

      if (showProgression) {
          System.out.println();
      }

      System.out.println("\nEvolving generation "
                         + (gen)
                         + "/" + numEvolutions
                         + "(time from start: " + elapsedNow + "s)");
      if (a_best == null) {
          System.out.println("No best solution (null)");
          return;
      }
      double bestValue = a_best.getFitnessValue();
      if (Double.isInfinite(bestValue)) {
          System.out.println("No best solution (infinite)");
          return;
      }
      System.out.print("Best solution fitness: " +
                       NumberKit.niceDecimalNumber(bestValue, 2) + 
                       " (error method: " + errorMethod + ")");
      if (validationPCT > 0.0d && validationSet != null) {
          // validate this program against the validation set
          double validationFitness = validateData(a_best);
          System.out.print("    (validation fitness: " + validationFitness + ")");
      } 
      System.out.println();

      System.out.println("Best solution: " + a_best.toStringNorm(0));
      String depths = "";
      int size = a_best.size();
      for (int i = 0; i < size; i++) {
          if (i > 0) {
              depths += " / ";
          }
          depths += a_best.getChromosome(i).getDepth(0);
      }
      if (size == 1) {
          System.out.print("Depth of chrom: " + depths);
      }
      else {
          System.out.print("Depths of chroms: " + depths);
      }
      ProgramChromosome chrom = a_best.getChromosome(0);
      int numFunctions = chrom.numFunctions();
      int numTerminals = chrom.numTerminals();
      int numTerms =  numFunctions + numTerminals;
      System.out.println(". Number of functions+terminals: " + numTerms + " (" + numFunctions + " functions, " + numTerminals + " terminals)");
      ApplicationData appData = (ApplicationData)a_best.getApplicationData();
      System.out.println("Correlation coefficient: " + appData.getCorrelation());
      // 2010-02-22: Print out the different error variants
      System.out.println("minError: " + appData.getMinError() + " meanError: " + appData.getMeanError() + " medianError: " + appData.getMedianError() + " maxError: " + appData.getMaxError() + " totalError: " + appData.getTotalError());
      if (hitsCriteria >= 0.0d) {
          int numHits = appData.getNumHits();
          String hitsPct = String.format("%5.2f", numHits / (double)numRows);
          System.out.println("Number of hits (<= " + hitsCriteria + "): " + numHits + " (of " + numRows + " = " + hitsPct + ")");
      }
      if (showResults) {
          double[] results = appData.getResults();
          if (results != null) {
              System.out.println("Results for this program:");
              double sumAbsDiff = 0.0d;
              double sumDiff = 0.0d;
              String formatStr = "%5." + resultPrecision + "f";
              int thisNumHits = 0;
              for (int i = 0; i < results.length; i++) {
                  double d = data[outputVariable][i];
                  double r = results[i];
                  double diff = d-r;
                  double absDiff = Math.abs(diff);
                  String hitsStr = "";
                  if (hitsCriteria >= 0.0d && absDiff > hitsCriteria) {
                      hitsStr = " > " + hitsCriteria + "!";
                  } else {
                      thisNumHits++;
                  }
                  
                  System.out.println("(" + i + ") " +  d + ": " + String.format(formatStr,r) + " (diff: " + String.format(formatStr,diff) + ")" + hitsStr);
                  sumDiff += diff;
                  sumAbsDiff += absDiff;
              }
              String numHitsStr = hitsCriteria >= 0 ?  " #hits: " + thisNumHits + " (of " + numRows : "";
              System.out.println("total diff: " + sumAbsDiff + " (no abs diff: " + sumDiff + numHitsStr + ")\n");
          }
      }

  }

  /**
   * Due to lazyness this is borrowed from 
   * Weka's weka/core/Utils.java, slightly edited.
   * [Copyright (C) 1999-2004 University of Waikato, Hamilton, New Zealand]
   *
   * Weka is a great machine learning/data mining system: 
   * http://www.cs.waikato.ac.nz/~ml/weka/index.html
   *
   *
   * Returns the correlation coefficient of two double vectors.
   *
   * @param y1 Double vector 1
   * @param y2 double vector 2
   * @param n the length of two double vectors
   * @return the correlation coefficient
   */
  public static final double correlation(Double y1[],double y2[],int n) {

    int i;
    double av1 = 0.0, av2 = 0.0, y11 = 0.0, y22 = 0.0, y12 = 0.0, c;
    
    if (n <= 1) {
      return 1.0;
    }
    for (i = 0; i < n; i++) {
      av1 += y1[i];
      av2 += y2[i];
    }
    av1 /= (double) n;
    av2 /= (double) n;
    for (i = 0; i < n; i++) {
      y11 += (y1[i] - av1) * (y1[i] - av1);
      y22 += (y2[i] - av2) * (y2[i] - av2);
      y12 += (y1[i] - av1) * (y2[i] - av2);
    }
    if (y11 * y22 == 0.0) {
      c=1.0;
    } else {
      c = y12 / Math.sqrt(Math.abs(y11 * y22));
    }
    
    return c;

  }

  // check fitness for an individual, typically the fittest program
  // in the population
  // 2010-02-22: error is according to the error method
  public static double validateData(final IGPProgram ind) {
      double error = 0.0d;
      double[] errors = new double[validationSet[0].length];
      Object[] noargs = new Object[0];
      if (validationSet != null && validationSet.length > 0) {
          for (int j = 0; j < validationSet[0].length; j++) {
              
              int variableIndex = 0;
              for(int i = 0; i < numInputVariables+1; i++) {
                  if (i != outputVariable) {
                      variables[variableIndex].set(validationSet[i][j]);
                      variableIndex++;
                  }
              }
              try {
                  double result = ind.execute_double(0, noargs);
                  double err = Math.abs(result - validationSet[outputVariable][j]);
                  errors[j] = err;
                  error += err;
                  if (Double.isInfinite(error) || Double.isNaN(error)) {
                      return Double.MAX_VALUE;
                  }
              } catch (ArithmeticException ex) {
                  System.out.println(ind);
                  throw ex;
              }
              
          }
      }

      double[]err = calcAllErrors(error, errors);
      
      return err[0];
      
  }

  // 2010-02-22: Calculate the different error alternatives
  public static double[] calcAllErrors(double error, double[] errors) {
      double totalError = error;
      double[] minMaxErrors = getMinMax(errors);
      double minError = minMaxErrors[0];
      double maxError = minMaxErrors[1];
      double medianError = minMaxErrors[2];
      double meanError = error / errors.length;
      
      // default is totalError
      if ("meanError".equals(errorMethod)) {
          error = meanError;
      } else if ("minError".equals(errorMethod)) {
          error = minError;
      } else if ("maxError".equals(errorMethod)) {
          error = maxError;
      } else if ("medianError".equals(errorMethod)) {
          error = medianError;
      }

      double[] err = {error, totalError, minError, maxError, meanError, medianError};
      return err;

  }

  // evaluate data for a program.
   public static Double evalData(final IGPProgram ind, Double [] data) {
       Double result = 0.0d;
       // Double error = 0.0d;
       Object[] noargs = new Object[0];
       
       int variableIndex = 0;
       for(int i = 0; i < numInputVariables+1; i++) {
           if (i != outputVariable) {
               variables[variableIndex].set(data[i]);
               variableIndex++;
           }
       }
       try {
              
           result = ind.execute_double(0, noargs);
           // error = Math.abs(result - data[outputVariable]);

       } catch (ArithmeticException ex) {
           System.out.println(ind);
           throw ex;
       }
       
       return result;

  }


   public static double[] getMinMax(double[] arr) {
       double min = Double.MAX_VALUE;
       double max = Double.MIN_VALUE;
       int len = arr.length;
       for(int i = 0; i < len; i++) {
           if (arr[i] < min) {
               min = arr[i];
           }
           if (arr[i] > max) {
               max = arr[i];
           } 
       }

       double median = Double.MIN_VALUE;
       int len2 = (int)len / 2;
       if (len == 1) {
           median = arr[0];
       } else if (len == 2) {
           median = (arr[0] + arr[1]) / 2;
       } else if (len % 2 == 1) {
           median = arr[len2];
       } else {
           median = (arr[len2] + arr[len2+1]) / 2;
       }

       return new double[] {min, max, median};
   } 

}
