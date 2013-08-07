/**
  *
  * Tue Apr  1 18:11:00 2003/hakank@bonetmail.com
  *
  * For Apriori algorithm, maybe other algorithms.
  *
  */

/*
  TODO:
  We cannot use the WordVector trick here, since apriori can't handle numeric
  values. I.e. must convert to a (basket) matrix.


  Handle the following Apriori parameters :

  - delta double 0.05
  - lowerBoundMinSupport double 0.1
  - metricType: Confidence, Lift, Leverage, Conviction ????
  - minMetric double 0.9
  - numRules int 20
  - significanceLevel double -1.0 (??)
  - upperBoundMinSupport double 1.0  
    


 */

import java.io.*;
import java.util.*;
import weka.core.*;

import weka.associations.*;
import weka.associations.Apriori.*;

public class AssociationRules {

    private String[]    inputText       = null;
    private String      classString     = null; // the classifier
    private FastVector  attributeInfo   = null;
    private Instances   instances       = null;
    private int[][]     productMatrix   = null;
    private String[]    productNames    = null;
    private Attribute[] attributes      = null;

    private Apriori     apriori         = null;
    
    // apriori specific parameters
    private double deltaValue                = 0.05;
    private double lowerBoundMinSupportValue =  0.1;
    // String  metricType: Confidence, Lift, Leverage, Conviction?
    private double minMetricValue            = 0.9;
    private int numRulesValue                = 20;
    private double significanceLevelValue    = -1.0; // (??)
    private double upperBoundMinSupportValue = 1.0;



    //
    // main, mainly for testing
    //
    public static void main(String args[]) {

        int thisNumRules = 20;

        if (args.length > 0) {
            try {
                thisNumRules = Integer.parseInt(args[0]);
            } catch (Exception e) {
                e.printStackTrace();
            }
        } 


        String[] inputText = {"banana apple pear milk", "milk banana apple", "magazine pencil paper", "paper pencil banana"};


        AssociationRules associationRules = new AssociationRules(inputText);

        associationRules.setNumRules(thisNumRules);
        System.out.println(associationRules.associate());


    } // end main


    //
    // constructor
    //
    AssociationRules(String[] inputText) {

        this.inputText      = inputText;
        

        //
        // creates this.productMatrix
        //         this.productNames
        //
        createBasketMatrix(inputText);


        //
        // creates attribute structures
        //
        FastVector occurenceVector = new FastVector(2);
        occurenceVector.addElement("0");
        occurenceVector.addElement("1");
        Attribute occurenceAttribute = new Attribute("occurence", occurenceVector);

        // add all attributes
        attributeInfo = new FastVector();
        attributes = new Attribute[productNames.length];
        for (int i = 0; i < productNames.length; i++) {
            Attribute thisAttribute = new Attribute(productNames[i], occurenceVector);
            attributes[i] = thisAttribute;
            attributeInfo.addElement(thisAttribute);
        }


    } // end AssociationRules


    public StringBuffer associate() {
        return(associate(deltaValue, lowerBoundMinSupportValue, minMetricValue,
                         numRulesValue, significanceLevelValue, upperBoundMinSupportValue));
    }



    //
    // the main association method
    //
    public StringBuffer associate(double deltaValue, double lowerBoundMinSupportValue, double minMetricValue, int numRulesValue, double significanceLevelValue, double upperBoundMinSupportValue) {
        
        StringBuffer result = new StringBuffer();

        this.deltaValue = deltaValue;
        this.lowerBoundMinSupportValue = lowerBoundMinSupportValue;
        this.numRulesValue             = numRulesValue;
        this.minMetricValue            = minMetricValue;
        this.significanceLevelValue    = significanceLevelValue;
        
        this.upperBoundMinSupportValue = upperBoundMinSupportValue;



        // creates an empty instances set
        instances = new Instances("data set", attributeInfo, 100);

        //
        // populate instances
        //
        for (int i = 0; i < productMatrix.length; i++) {
            Instance inst = new Instance(attributes.length);
            for (int j = 0; j < attributes.length; j++) {
                inst.setValue(attributes[j], productMatrix[i][j]);
            }
            instances.add(inst);
        }


        try {

            result.append("DATA SET:\n" + instances + "\n");

 
            //
            // Do the Apriori thing!
            //
            apriori = new Apriori();
            apriori.setDelta(deltaValue);
            apriori.setLowerBoundMinSupport(lowerBoundMinSupportValue);
            apriori.setMinMetric(minMetricValue);
            apriori.setNumRules(numRulesValue);
            apriori.setUpperBoundMinSupport(upperBoundMinSupportValue);
            
            apriori.buildAssociations(instances);
            result.append(apriori.toString() + "\n");


        } catch (Exception e) {
            e.printStackTrace();
            result.append("\nException (sorry!):\n" + e.toString());
        }

        return result;

    } // end associate



    /** 
     * converts a string array to a matrix
     *
     * returns a string array of the products (product names)
     *
     */
    public void createBasketMatrix(String[] basket) {
        
        int numBaskets = basket.length;

        HashMap productMap = new HashMap();
        HashSet productSet = new HashSet();

        //
        // parse the products id's
        // assumption: product id are strings
        //
        for (int i = 0; i < basket.length; i++) {
            String[] thisBasket = basket[i].split("[\\s,]+");
            List theseProducts = new ArrayList();
            Integer basketIx = new Integer(i);
            for (int j = 0; j < thisBasket.length; j++) {
                try {
                    String thisVal = thisBasket[j];
                    productSet.add((String)thisVal);
                    theseProducts.add(thisVal);
                } catch (Exception e) {
                    e.printStackTrace();
                }
                productMap.put(basketIx, theseProducts);
            }            
        }
        
        int numProducts = productSet.size();


        //
        // create the product matrix (numBasket x numProducts)
        //
        String[] products = (String[])productSet.toArray(new String[0]);

        this.productMatrix = new int[numBaskets][numProducts];

        // for each basket
        for (int i = 0; i < numBaskets; i++) {
            ArrayList theseProds = (ArrayList)productMap.get(new Integer(i));

            // for each product
            // note: these are not in sorted order
            for (int j = 0; j < products.length; j++) {
                if (theseProds.contains((String)products[j])) {
                    this.productMatrix[i][j] = 1;
                } else {
                    this.productMatrix[i][j] = 0;
                }

            }
        }
        
        this.productNames = products;

    } // end createBasketMatrix



    //
    // setter for the classifier _string_
    //
    public void setClassifierString(String classString) {
        this.classString = classString;
    }

    public int[][] getProductMatrix() {
        return this.productMatrix;
    }
    

    public void setNumRules(int numRulesValue) {
        this.numRulesValue = numRulesValue;
    }

    public void setMinMetric(double minMetric) {
        this.minMetricValue = minMetric;
    }


    public void setLowerBoundMinSupport(double lowerBoundMinSupportValue) {
        this.lowerBoundMinSupportValue = lowerBoundMinSupportValue;
    }

}
