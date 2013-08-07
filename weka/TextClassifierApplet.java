/**
  *
  * Mon Mar 31 20:37:18 2003/hakank@bonetmail.com
  *
  *
  */

/*
   TODO:

   * try to analyse the weighted value further, for example if there
     is some limit which divides the classes.
   * maybe toLowerCase on "everything" before doing serious stuff?


 */


// for java & applets
import java.io.*;
import java.util.*;
import java.text.*;
import java.applet.Applet;
import java.awt.*;
import java.awt.event.*;
import java.net.*;

// for weka
import weka.core.*;
import weka.classifiers.*;
import weka.classifiers.trees.*;
import weka.classifiers.trees.j48.*;
import weka.classifiers.bayes.*;


public class TextClassifierApplet extends Applet implements ActionListener, ItemListener {

    private String algorithm = "weka.classifiers.bayes.NaiveBayes";
    private String[] classValues = null; // we should calculate this!
    private String[] inputText   = null;
    private String[] inputClasses = null;

    private StringBuffer result = null;

    Button classifyButton;
    Button resetButton;

    Label classifyLabel;
    Label resetLabel;
    Label messageLabel;
    Label algorithmLabel;

    Label classValuesLabel;
    Label inputTextLabel;
    Label inputClassesLabel;
    Label newTestLabel;

    TextField newTestTextField;

    Choice algorithmChoice;

    TextArea resultTextArea;
    // these two should reside beside each other!
    TextArea inputTextTextArea;
    TextArea inputClassesTextArea;

    Panel buttonsPanel;
    Panel inputPanel; // inputChoicePanel; 
    Panel textAndClassesLabelsPanel;
    Panel textAndClassesTextAreasPanel;
    Panel textAndClassesPanel;
    Panel newTestPanel;

    public static void main(String args[]) {


    }

    public void init() {

        // buttons
        classifyButton = new Button("Classify");
        resetButton    = new Button("Reset");

        // listeners
        classifyButton.addActionListener(this);
        resetButton.addActionListener(this);

        // labels
        messageLabel      = new Label("");
        algorithmLabel    = new Label("Select algorithm: ");
        classValuesLabel  = new Label("Class values: ");
        inputTextLabel    = new Label("Text:: ");
        inputClassesLabel = new Label("Class:");
        newTestLabel      = new Label("Classify new text: ");

        newTestTextField  = new TextField("",40);


        algorithmChoice  = new Choice();

        algorithmChoice.addItem("weka.classifiers.bayes.NaiveBayes");
        algorithmChoice.addItem("weka.classifiers.lazy.IB1");
        algorithmChoice.addItem("weka.classifiers.lazy.IBk");
        algorithmChoice.addItem("weka.classifiers.functions.SMO");
        algorithmChoice.addItem("weka.classifiers.functions.MultilayerPerceptron");
        algorithmChoice.addItem("weka.classifiers.functions.Logistic");
        algorithmChoice.addItem("weka.classifiers.lazy.KStar");
        algorithmChoice.addItem("weka.classifiers.misc.HyperPipes");
        algorithmChoice.addItem("weka.classifiers.misc.VFI");
        algorithmChoice.addItem("weka.classifiers.rules.PART");
        algorithmChoice.addItem("weka.classifiers.rules.ConjunctiveRule");
        algorithmChoice.addItem("weka.classifiers.rules.DecisionTable");
        algorithmChoice.addItem("weka.classifiers.rules.JRip");
        algorithmChoice.addItem("weka.classifiers.rules.NNge");
        algorithmChoice.addItem("weka.classifiers.rules.Ridor");
        algorithmChoice.addItem("weka.classifiers.rules.OneR");
        algorithmChoice.addItem("weka.classifiers.trees.J48");
        algorithmChoice.addItem("weka.classifiers.trees.REPTree");

        algorithmChoice.addItemListener(this);


        resultTextArea    = new TextArea(10,20);

        inputTextTextArea  = new TextArea(10,20);
        inputClassesTextArea = new TextArea(10,20);


        buttonsPanel        = new Panel();
        buttonsPanel.setLayout(new FlowLayout());
        buttonsPanel.add(classifyButton);
        buttonsPanel.add(resetButton);


;
        textAndClassesTextAreasPanel = new Panel(new GridLayout(1,2));
        textAndClassesTextAreasPanel.add(inputTextTextArea);
        textAndClassesTextAreasPanel.add(inputClassesTextArea);

        textAndClassesLabelsPanel = new Panel(new FlowLayout());
        textAndClassesLabelsPanel.add(inputTextLabel);
        textAndClassesLabelsPanel.add(inputClassesLabel);


        newTestPanel = new Panel(new FlowLayout());
        newTestPanel.add(newTestLabel);
        newTestPanel.add(newTestTextField);

        textAndClassesPanel = new Panel(new BorderLayout());
        textAndClassesPanel.add("North", textAndClassesLabelsPanel);
        textAndClassesPanel.add("Center", textAndClassesTextAreasPanel);
        textAndClassesPanel.add("South", newTestPanel);


        inputPanel = new Panel();
        inputPanel.setLayout(new BorderLayout());
        inputPanel.add("North", buttonsPanel);
        inputPanel.add("West", algorithmLabel);
        inputPanel.add("Center", algorithmChoice);
        inputPanel.add("South", textAndClassesPanel);

        setLayout(new BorderLayout());
        add("North", inputPanel);
        add("Center", resultTextArea);
        add("South", messageLabel);

        validate();

        classify();

    }

    public void actionPerformed(ActionEvent e) {

        String command = e.getActionCommand();        
        if ("Classify".equals(command)) {
            classify();
        } else if ("Reset".equals(command)) {
            reset();
        }

    } // end actionPerformed



    /**
     * classify cases
     *
     */ 
    public void classify() {

        result = new StringBuffer();

        //
        // get parameters
        //
        ArrayList textParamList = new ArrayList();
        ArrayList classParamList = new ArrayList();
        int paramIx = 1;
        boolean moreParams = true;
        while (moreParams) {
            try {
                String textParam = this.getParameter("text" + paramIx);
                String classParam = this.getParameter("class" + paramIx);
                if (textParam == null || classParam == null) {
                    moreParams = false;
                    break;
                } else {
                    textParamList.add(textParam);
                    classParamList.add(classParam);
                    paramIx++;
                }
            } catch (Exception e) {
                result.append("" + e);
                moreParams = false;
            }
        }


        messageLabel.setText("");
        resultTextArea.setText("");

        TextClassifier textClassifier;
        inputClasses = inputClassesTextArea.getText().split("\\n");
        inputText    = inputTextTextArea.getText().split("\\n");

        if (inputClasses.length != inputText.length) {
            resultTextArea.setText("The text and class fields must be of the same length!");
            return;
        }

        if (inputClasses.length <= 1) {
            inputClasses = (String[])classParamList.toArray(new String[0]);
            inputClassesTextArea.setText(arrayToString(inputClasses,"\n"));
        }
        
        
        if (inputText.length <= 1) {
            inputText = (String[])textParamList.toArray(new String[0]);
            inputTextTextArea.setText(arrayToString(inputText,"\n"));
        }

        // calculate the classValues
        HashSet classSet = new HashSet(Arrays.asList(inputClasses));
        classSet.add("?");
        classValues = (String[])classSet.toArray(new String[0]);

        //
        // create class attribute
        //
        FastVector classAttributeVector = new FastVector();
        for (int i = 0; i < classValues.length; i++) {
            classAttributeVector.addElement(classValues[i]);
        }
        Attribute thisClassAttribute = new Attribute("class", classAttributeVector);

        //
        // create text attribute
        //
        FastVector inputTextVector = null;  // null -> String type
        Attribute thisTextAttribute = new Attribute("text", inputTextVector);
        for (int i = 0; i < inputText.length; i++) {
            thisTextAttribute.addStringValue(inputText[i]);
        }
        
        // add test cases
        // just a singular test string
        String newTextString = newTestTextField.getText();
        String[] newTextArray = new String[1];
        newTextArray[0] = newTextString;
        if (!"".equals(newTextString)) {
            thisTextAttribute.addStringValue(newTextString);
        }

        //
        // create the attribute information
        //
        FastVector thisAttributeInfo = new FastVector(2);
        thisAttributeInfo.addElement(thisTextAttribute);
        thisAttributeInfo.addElement(thisClassAttribute);

        messageLabel.setText("Trying to classify text...");

        try {
            result.append("ALGORITHM: " + algorithm + "\n\n");
            textClassifier = new TextClassifier(inputText, inputClasses, thisAttributeInfo, thisTextAttribute, thisClassAttribute, algorithm);
            result.append(textClassifier.classify(algorithm));

            if (!"".equals(newTextString)) {
                result.append("\nCLASSIFY NEW TEXT:\n" + textClassifier.classifyNewCases(newTextArray));
            }

            messageLabel.setText("OK!");
        } catch(Exception e) {
            result.append("Exception: " + e.toString());
        }


        resultTextArea.setText(result.toString());
   
    } // end classify


    /**
     *  resets the fields
     *
     */
    public void reset() {

        resultTextArea.setText("");
        messageLabel.setText("");
        inputTextTextArea.setText("");
        inputClassesTextArea.setText("");
        newTestTextField.setText("");

    } // end reset



   // 
    // itemStateChanged
    //
    public void itemStateChanged(ItemEvent e) {
        if (e.getSource() == algorithmChoice) {
            algorithm = algorithmChoice.getSelectedItem();
        }

    }




    // from BufferedReader to Vector()
    public java.util.List readerToVector(BufferedReader reader) {
        String s;
        java.util.List v = new ArrayList();
        try {
            while ( (s = reader.readLine()) != null) {
                v.add(s);
            }
        } catch (Exception e) {
            // hmmm ....
        }
        
        return v;
    }


    public static String arrayToString(Object[] vec, String sep) {
        StringBuffer ret = new StringBuffer();
        for (int i = 0; i < vec.length; i++) {
            ret.append((String)vec[i]);
            if (i < vec.length - 1 ) 
                ret.append(sep); 
        }
        
        return (ret.toString());
        
    } // end arrayToString



}

// Local variables:
// compile-command: "javac WekaApplet1.java && java WekaApplet1"
// End: 
