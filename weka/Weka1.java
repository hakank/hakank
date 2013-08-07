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
import weka.classifiers.trees.j48.*;
import weka.classifiers.bayes.*;

public class Weka1 {

    Instances instances;
    BufferedReader reader;
    String fileName = "";
    Classifier classifier;
    String classString = "weka.classifiers.bayes.NaiveBayes";

    public static void main(String args[]) {

        // note: the class must be fully qualified including package names.
        String myFileName = "zoo2.arff";
        String myClassString = "weka.classifiers.bayes.NaiveBayes";
        if (args.length > 0) {
            myClassString = args[0];
        }

        if (args.length > 1) {
            myFileName = args[1];
        }


        BufferedReader reader = Weka1.getReader(myFileName);
        Weka1 w = new Weka1(reader);
        List t = w.weka(myClassString, myFileName);
        System.out.println(t);
    }


    // Constructor
    Weka1(String fileName) {
        this.fileName = fileName;
    }

    // Constructor
    Weka1(String classString, String fileName) {
        this.fileName = fileName;
        this.classString = classString;
    }


    Weka1(BufferedReader reader) {
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
        // Classifier classifier = new J48();

        List result = new ArrayList();

        result.add("Algorithm: " + classString);
        result.add("Data set: " + dataSet);

        try {
            // Read all the instances in the file
            // reader = new FileReader("zoo2.arff");
            // classifier = (Classifier)Class.forName(classString).newInstance();
              
            // String[] options = {"-t", "zoo2.arff"}
            // System.out.println(evaluateModel(classifier, options));

            instances = new Instances(reader);

            result.add("Using classifier: " + classString);
            classifier = Classifier.forName(classString,null);

            // Make the last attribute be the class            
            int classIndex = instances.numAttributes() - 1;
            result.add("Number of Attributes (excl class): " + classIndex);
            instances.setClassIndex(classIndex);
            
            result.add("Num classes: " + instances.numClasses());
            result.add("Num instances: " + instances.numInstances());


            // Print header and instances.
            // result.add("\nDataset:\n");
            // result.add(instances);

            // System.out.println("Loopar igenom alla instanser:");
            // for (int i = 0; i < instances.numInstances(); i++) 
            // System.out.println(instances.instance(i));

            result.add("instances.sumOfWeights():\n" + instances.sumOfWeights());

            // Se i Evaluation docen
            result.add("\n\nEvaluation:\n");
            classifier.buildClassifier(instances);
            Evaluation evaluation = new Evaluation(instances);
            evaluation.evaluateModel(classifier, instances);

            result.add("\n\nclassifier.toString():\n" + classifier.toString());

            // if j48
            // This is not interesting to show...
            // if (classifier instanceof Matchable) {
            // result.add("Tree:\n" + ((Matchable)classifier).prefix());
            // }

            result.add("\n\ntoSummaryString():\n" + evaluation.toSummaryString());
            result.add("\n\ntoMatrixString():\n " + evaluation.toMatrixString());
            // result.add("toClassDetailsString():\n" + evaluation.toClassDetailsString());


            result.add("\n\nnumTruePositives():");
            // this is the diagonal of the confusion matrix (?)
            for (int i = 0; i <= classIndex; i++) {
                result.add(i + ": " + evaluation.numTruePositives(i));
            }

            result.add("\n\nevaluation.toClassDetailsString(\"Details\")\n" + evaluation.toClassDetailsString("Details"));
            result.add("\n\nCumulativeMarginDistribution():\n" + evaluation.toCumulativeMarginDistributionString());



        } catch (Exception e) {
            result.add("Exception (sorry!):\n" + e);
        }

        return result; 

    } // end weka
    
} // end class Weka1

