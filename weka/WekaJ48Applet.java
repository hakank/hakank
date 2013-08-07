/**
  *
  * Thu Mar 27 12:46:48 2003/hakank@bonetmail.com
  *
  * This is a applet implementing "all" of the J48 features
  *
  *
  */

/* settings in J48:
   name type default
   -----------------
   binarySplits boolean false
   minNumObj double 2
   numFolds int 3
   reducedErrorPruning boolean false
   (saveInstanceData boolean false) [N/A]
   subtreeRaising boolean true
   unpruned boolean false
   useLaplace boolean false

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
import weka.classifiers.trees.J48;
import weka.classifiers.bayes.*;


public class WekaJ48Applet extends Applet implements ActionListener, ItemListener {

    private String algorithm = "weka.classifiers.trees.J48";
    private String URLString;
    private java.util.List resultList;

    Button generateButton;
    Button resetButton;

    TextField URLTextField;

    // algorithm specific variables
    boolean binarySplits       = false;
    int     minNumObj           = 2;
    int     numFolds            = 3;
    boolean reducedErrorPruning = false;
    boolean subtreeRaising      = true;
    boolean unpruned            = false;
    boolean useLaplace          = false;

    // defaults
    int minNumObjDefault = 2;
    int numFoldsDefault  = 3;

    // algorithm specific TextField
    TextField minNumObjTextField;
    TextField numFoldsTextField;

    Label generateLabel;
    Label resetLabel;
    Label URLLabel;
    Label messageLabel;
    // Label algorithmLabel;
    Label URLChoiceLabel;

    // algorithm specific Labels
    Label binarySplitsLabel;
    Label minNumObjLabel;
    Label numFoldsLabel;
    Label reducedErrorPruningLabel;
    Label subtreeRaisingLabel;
    Label unprunedLabel;
    Label useLaplaceLabel;
     

    // Choice algorithmChoice;
    Choice URLChoice;

    // algorithmic specific Choices (booleans)
    Choice binarySplitsChoice;
    Choice reducedErrorPruningChoice;
    Choice subtreeRaisingChoice;
    Choice unprunedChoice;
    Choice useLaplaceChoice;

    TextArea resultTextArea;

    Panel inputPanel;
    Panel inputChoicePanel;
    Panel settingsPanel;
    Panel allInputPanel;


    public static void main(String args[]) {


    }

    public void init() {

        // buttons
        generateButton = new Button("Generate");
        resetButton    = new Button("Reset");

        // listeners
        generateButton.addActionListener(this);
        resetButton.addActionListener(this);

        URLLabel         = new Label("URL: ");
        messageLabel     = new Label("");
        // algorithmLabel   = new Label("Select algorithm: ");
        URLChoiceLabel   = new Label("Select data set: ");

        // algoritmic specific Labels
        binarySplitsLabel        = new Label("binarySplits");
        minNumObjLabel           = new Label("minNumObj");
        numFoldsLabel            = new Label("numFolds"); 
        reducedErrorPruningLabel = new Label("reducedErrorPruning");
        subtreeRaisingLabel      = new Label("subtreeRaising");
        unprunedLabel            = new Label("unpruned");
        useLaplaceLabel          = new Label("useLaplace");


        // algoritmic specific Choice 
        binarySplitsChoice         = new Choice();
        binarySplitsChoice.add("false");
        binarySplitsChoice.add("true");
        reducedErrorPruningChoice  = new Choice();
        reducedErrorPruningChoice.add("false");
        reducedErrorPruningChoice.add("true");
        subtreeRaisingChoice       = new Choice();
        subtreeRaisingChoice.add("true");
        subtreeRaisingChoice.add("false");
        unprunedChoice             = new Choice();
        unprunedChoice.add("false");
        unprunedChoice.add("true");
        useLaplaceChoice           = new Choice();
        useLaplaceChoice.add("false");
        useLaplaceChoice.add("true");

        URLTextField     = new TextField("",30);

        // algorithmic specific TextField
        minNumObjTextField = new TextField("" + minNumObjDefault, 4);
        numFoldsTextField  = new TextField("" + numFoldsDefault,4);

        URLChoice        = new Choice();
        URLChoice.add("http://www.hakank.org/weka/zoo2.arff");
        URLChoice.add("http://www.hakank.org/weka/zoo2_x.arff");
        URLChoice.add("http://www.hakank.org/weka/sunburn.arff");
        URLChoice.add("http://www.hakank.org/weka/ticdata_categ.arff");
        URLChoice.add("http://www.hakank.org/weka/wine.arff");
        URLChoice.add("http://www.hakank.org/weka/iris.arff");
        URLChoice.add("http://www.hakank.org/weka/iris_discretized.arff");
        URLChoice.add("http://www.hakank.org/weka/shape.arff");
        URLChoice.add("http://www.hakank.org/weka/titanic.arff");
        URLChoice.add("http://www.hakank.org/weka/disease.arff");
        URLChoice.add("http://www.hakank.org/weka/zoo.arff");
        URLChoice.add("http://www.hakank.org/weka/monk3.arff");
        URLChoice.add("http://www.hakank.org/weka/monk2.arff");
        URLChoice.add("http://www.hakank.org/weka/monk1.arff");
        URLChoice.add("http://www.hakank.org/weka/credit.arff");
        URLChoice.add("http://www.hakank.org/weka/contact-lenses.arff");
        URLChoice.add("http://www.hakank.org/weka/labor.arff");
        URLChoice.add("http://www.hakank.org/weka/labor_discretized.arff");
        URLChoice.add("http://www.hakank.org/weka/weather.arff");
        URLChoice.add("http://www.hakank.org/weka/weather.nominal.arff");
        URLChoice.add("http://www.hakank.org/weka/BC.arff");
        URLChoice.add("http://www.hakank.org/weka/G2.arff");
        URLChoice.add("http://www.hakank.org/weka/GL.arff");
        URLChoice.add("http://www.hakank.org/weka/HD.arff");
        URLChoice.add("http://www.hakank.org/weka/HE.arff");
        URLChoice.add("http://www.hakank.org/weka/HO.arff");
        URLChoice.add("http://www.hakank.org/weka/IR.arff");
        URLChoice.add("http://www.hakank.org/weka/LA.arff");
        URLChoice.add("http://www.hakank.org/weka/LY.arff");
        URLChoice.add("http://www.hakank.org/weka/SO.arff");
        URLChoice.add("http://www.hakank.org/weka/V1.arff");
        URLChoice.add("http://www.hakank.org/weka/VO.arff");
        URLChoice.add("http://www.hakank.org/weka/auto93.arff");
        URLChoice.add("http://www.hakank.org/weka/badges_plain.arff");
        URLChoice.add("http://www.hakank.org/weka/badges2.arff");

        URLChoice.addItemListener(this);

        // algorithmChoice  = new Choice();
        // algorithmChoice.addItem("weka.classifiers.trees.j48.J48");
        // algorithmChoice.addItemListener(this);


        settingsPanel    = new Panel(new GridLayout(7,2));
        settingsPanel.add(binarySplitsLabel);
        settingsPanel.add(binarySplitsChoice);
        settingsPanel.add(minNumObjLabel);
        settingsPanel.add(minNumObjTextField);
        settingsPanel.add(numFoldsLabel);
        settingsPanel.add(numFoldsTextField);
        settingsPanel.add(reducedErrorPruningLabel);
        settingsPanel.add(reducedErrorPruningChoice);
        settingsPanel.add(subtreeRaisingLabel);
        settingsPanel.add(subtreeRaisingChoice);
        settingsPanel.add(unprunedLabel);
        settingsPanel.add(unprunedChoice);
        settingsPanel.add(useLaplaceLabel);
        settingsPanel.add(useLaplaceChoice);

        resultTextArea    = new TextArea(10,20);

        inputPanel        = new Panel();
        inputPanel.setLayout(new FlowLayout());
        inputPanel.add(generateButton);
        inputPanel.add(resetButton);
        inputPanel.add(URLLabel);
        inputPanel.add(URLTextField);

        allInputPanel = new Panel(new BorderLayout());
        allInputPanel.add("North", inputPanel);
        allInputPanel.add("West", URLChoiceLabel);
        allInputPanel.add("Center", URLChoice);

        inputChoicePanel = new Panel();
        inputChoicePanel.setLayout(new BorderLayout());
        inputChoicePanel.add("North", allInputPanel);
        // inputChoicePanel.add("West", algorithmLabel);
        // inputChoicePanel.add("Center", algorithmChoice);
        inputChoicePanel.add("South", settingsPanel);

        setLayout(new BorderLayout());
        add("North", inputChoicePanel);
        add("Center", resultTextArea);
        add("South", messageLabel);

        validate();

    }

    public void actionPerformed(ActionEvent e) {

        String command = e.getActionCommand();        
        if ("Generate".equals(command)) {
            generate();
        } else if ("Reset".equals(command)) {
            reset();
        }

    } // end actionPerformed

    
    /**
     * generate the sequence
     *
     */ 
    public void generate() {

        messageLabel.setText("");

        URLString = URLTextField.getText().trim();
        resultTextArea.setText("");
        if ("".equals(URLString)) {
            URLString = "http://www.hakank.org/weka/zoo2.arff";
            resultTextArea.setText("Using the default ARFF file!");
        }

        try {
            minNumObj = Integer.parseInt(minNumObjTextField.getText());
        } catch (Exception e) {
            minNumObj = minNumObjDefault;
        }

        try {
            numFolds = Integer.parseInt(numFoldsTextField.getText());
        } catch (Exception e) {
            numFolds = numFoldsDefault;
        }


        resultList = new ArrayList();
 
        // do something here!

        BufferedReader reader = null;
        int available = 0;

        messageLabel.setText("Trying to read URL...");
        // OK, this is how to read a URL as a BufferedReader
        WekaJ48 weka;
        try {
            reader = readURL(URLString);
            weka = new WekaJ48(reader);
            messageLabel.setText("Have read URL. Now generating result. It can take some time....");
            weka.setSettings(binarySplits, minNumObj, numFolds, reducedErrorPruning, subtreeRaising, unpruned, useLaplace);
            resultList = weka.weka(algorithm, URLString);
            messageLabel.setText("OK!");
        } catch(Exception e) {
            ; // System.out.println("Exception:  " + e);
        }

        StringBuffer result = new StringBuffer();

        for (int i = 0; i < resultList.size(); i++) {
            result.append(resultList.get(i) + "\n");
        }

        resultTextArea.setText(result.toString());
   
    } // end generate


    /**
     *  resets the fields
     *
     */
    public void reset() {

        resultTextArea.setText("");
        messageLabel.setText("");

    } // end reset



   // 
    // itemStateChanged
    //
    public void itemStateChanged(ItemEvent e) {
        /*
        if (e.getSource() == algorithmChoice) {
            algorithm = algorithmChoice.getSelectedItem();
        }
        */
        if (e.getSource() == URLChoice) {
            URLString = URLChoice.getSelectedItem();
            URLTextField.setText(URLString);
        }

        // algorithm specific 
        if (e.getSource() == binarySplitsChoice) {
            binarySplits = new Boolean(binarySplitsChoice.getSelectedItem()).booleanValue();
        }
        if (e.getSource() == reducedErrorPruningChoice) {
            reducedErrorPruning = new Boolean(reducedErrorPruningChoice.getSelectedItem()).booleanValue();
        }
        if (e.getSource() == reducedErrorPruningChoice) {
            reducedErrorPruning = new Boolean(reducedErrorPruningChoice.getSelectedItem()).booleanValue();
        }
        if (e.getSource() == unprunedChoice) {
            unpruned = new Boolean(unprunedChoice.getSelectedItem()).booleanValue();
        }
        if (e.getSource() == useLaplaceChoice) {
            useLaplace = new Boolean(useLaplaceChoice.getSelectedItem()).booleanValue();
        }
    }


    // wrapper for reading an URL
    public BufferedReader readURL(String urlString) {
        BufferedReader reader = null;
        try {
            URL url = new URL(urlString);
            reader = new BufferedReader ( new InputStreamReader( (InputStream) url.getContent() ) );
        } catch (Exception e) {
            ; 
        }
        return reader;

    } // readURL


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

}

// Local variables:
// compile-command: "javac WekaJ48Applet.java && java WekaJ48Applet"
// End: 
