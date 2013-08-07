/**
  *
  * Thu Feb 21 11:22:23 2002/hakank@netch.se
  *
  *
  */
import java.io.*;
import java.util.*;
import java.text.*;
import weka.core.*;
import weka.classifiers.*;
import weka.classifiers.Sourcable.*;
import weka.classifiers.trees.*;
import weka.classifiers.bayes.*;

public class WekaJ48 {

    // algorithm specific variables
    boolean binarySplits        = false;
    int     minNumObj           = 2;
    int     numFolds            = 3;
    boolean reducedErrorPruning = false;
    boolean subtreeRaising      = true;
    boolean unpruned            = false;
    boolean useLaplace          = false;


    Instances instances;
    BufferedReader reader;
    String fileName = "";
    Classifier classifier;
    String classString = "weka.classifiers.trees.J48";


    // Constructor
    WekaJ48(String fileName) {
        this.fileName = fileName;
    }

    // Constructor
    WekaJ48(String classString, String fileName) {
        this.fileName = fileName;
        this.classString = classString;
    }


    WekaJ48(BufferedReader reader) {
        this.reader = reader;
    }


    public static BufferedReader getReader(String fileName) {
        BufferedReader r = null;

        try {
            r = new BufferedReader(new FileReader(fileName));
        } catch (Exception e) {
            ;
        }

        return r;
    }

    // the weka call
    public List weka() {
        return weka(classString, "<data>");
    }


    public List weka(String classString, String dataSet) {

        this.classString = classString;

        // Classifier classifier = new NaiveBayes();
        // Classifier classifier = new BayesNetK2();

        List result = new ArrayList();

        result.add("Algorithm: " + classString);
        result.add("Data set: " + dataSet);

        try {
              
            instances = new Instances(reader);

            result.add("Using classifier: " + classString);
            classifier = Classifier.forName(classString,null);
            // classifier = new J48();

            // set settings
            // sigh! we must cast the classifier!
            ((J48)classifier).setBinarySplits(binarySplits);
            ((J48)classifier).setMinNumObj(minNumObj);
            ((J48)classifier).setReducedErrorPruning(reducedErrorPruning);
            ((J48)classifier).setSubtreeRaising(subtreeRaising);
            ((J48)classifier).setUnpruned(unpruned);
            ((J48)classifier).setUseLaplace(useLaplace);

            

            // Make the last attribute be the class            
            int classIndex = instances.numAttributes() - 1;
            result.add("Number of Attributes (excl class): " + classIndex);
            instances.setClassIndex(classIndex);
            
            result.add("Num classes: " + instances.numClasses());
            result.add("Num instances: " + instances.numInstances());


            result.add("instances.sumOfWeights():\n" + instances.sumOfWeights());

            result.add("\n\nEvaluation:\n");
            classifier.buildClassifier(instances);


            Evaluation evaluation = new Evaluation(instances);
            evaluation.evaluateModel(classifier, instances);

            result.add("\n\nclassifier.toString():\n" + classifier.toString());

            result.add("\n\ntoSummaryString():\n" + evaluation.toSummaryString());
            result.add("\n\ntoMatrixString():\n " + evaluation.toMatrixString());
            // result.add("toClassDetailsString():\n" + evaluation.toClassDetailsString());


            result.add("\n\nnumTruePositives():");
            // this is the diagonal of the confusion matrix
            for (int i = 0; i <= classIndex; i++) {
                result.add(i + ": " + evaluation.numTruePositives(i));
            }

            result.add("\n\nevaluation.toClassDetailsString(\"Details\")\n" + evaluation.toClassDetailsString("Details"));
            result.add("\n\nCumulativeMarginDistribution():\n" + evaluation.toCumulativeMarginDistributionString());

            result.add("\nThe resulting tree as Java code:\n" + ((Sourcable)classifier).toSource(classString));


        } catch (Exception e) {
            result.add("Exception (sorry!):\n" + e);
        }

        return result; 

    } // end weka

    public void setSettings(boolean binarySplits, int minNumObj, int numFolds, boolean reducedErrorPruning, boolean subtreeRaising, boolean unpruned, boolean useLaplace) {

        this.binarySplits        = binarySplits;
        this.minNumObj           = minNumObj;
        this.numFolds            = numFolds;
        this.reducedErrorPruning = reducedErrorPruning;
        this.subtreeRaising      = subtreeRaising;
        this.unpruned            = unpruned;
        this.useLaplace          = useLaplace;
        
    }

    
} // end class WekaJ48

