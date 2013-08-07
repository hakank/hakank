/**
  *
  * Thu Mar 27 12:46:48 2003/hakank@bonetmail.com
  *
  * TODO:
  *   - get more information about the resulting structure!
  * 
  *
  *
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


public class WekaApplet1 extends Applet implements ActionListener, ItemListener {

    private String algorithm = "weka.classifiers.bayes.NaiveBayes";
    private String URLString;
    private java.util.List resultList;

    Button generateButton;
    Button resetButton;

    TextField URLTextField;
    
    Label generateLabel;
    Label resetLabel;
    Label URLLabel;
    Label messageLabel;
    Label algorithmLabel;
    Label URLChoiceLabel;

    Choice algorithmChoice;
    Choice URLChoice;

    TextArea resultTextArea;

    Panel inputPanel;
    Panel inputChoicePanel;
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

        URLLabel         = new Label("Data set (URL): ");
        messageLabel     = new Label("");
        algorithmLabel   = new Label("Select algorithm: ");
        URLChoiceLabel   = new Label("Select data set: ");

        URLTextField     = new TextField("",30);

        URLChoice        = new Choice();
        URLChoice.add("http://www.hakank.org/weka/zoo2.arff");
        URLChoice.add("http://www.hakank.org/weka/golf.arff");
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

        algorithmChoice  = new Choice();

        algorithmChoice.addItem("weka.classifiers.bayes.NaiveBayes");
        algorithmChoice.addItem("weka.classifiers.trees.J48");
        algorithmChoice.addItem("weka.classifiers.rules.OneR");
        algorithmChoice.addItem("weka.classifiers.rules.Ridor");
        algorithmChoice.addItem("weka.classifiers.bayes.BayesNet");
        algorithmChoice.addItem("weka.classifiers.bayes.NaiveBayesSimple");
        algorithmChoice.addItem("weka.classifiers.bayes.NaiveBayesUpdateable");
        algorithmChoice.addItem("weka.classifiers.functions.MultilayerPerceptron");
        algorithmChoice.addItem("weka.classifiers.rules.ZeroR");
        algorithmChoice.addItem("weka.classifiers.functions.SMO");
        algorithmChoice.addItem("weka.classifiers.functions.VotedPerceptron");
        algorithmChoice.addItem("weka.classifiers.functions.LeastMedSq");
        algorithmChoice.addItem("weka.classifiers.functions.LinearRegression");
        algorithmChoice.addItem("weka.classifiers.functions.Logistic");
        algorithmChoice.addItem("weka.classifiers.functions.UnivariateLinearRegression");
        algorithmChoice.addItem("weka.classifiers.functions.Winnow");
        algorithmChoice.addItem("weka.classifiers.functions.PaceRegression");
        algorithmChoice.addItem("weka.classifiers.lazy.KStar");
        algorithmChoice.addItem("weka.classifiers.lazy.IB1");
        algorithmChoice.addItem("weka.classifiers.lazy.IBk");
        algorithmChoice.addItem("weka.classifiers.lazy.LBR");
        algorithmChoice.addItem("weka.classifiers.lazy.LWL");
        algorithmChoice.addItem("weka.classifiers.misc.HyperPipes");
        algorithmChoice.addItem("weka.classifiers.misc.VFI");
        algorithmChoice.addItem("weka.classifiers.rules.PART");
        algorithmChoice.addItem("weka.classifiers.trees.ADTree");
        algorithmChoice.addItem("weka.classifiers.trees.M5P");
        algorithmChoice.addItem("weka.classifiers.trees.REPTree");
        algorithmChoice.addItem("weka.classifiers.rules.ConjunctiveRule");
        algorithmChoice.addItem("weka.classifiers.rules.DecisionTable");
        algorithmChoice.addItem("weka.classifiers.rules.JRip");
        algorithmChoice.addItem("weka.classifiers.rules.M5Rules");
        algorithmChoice.addItem("weka.classifiers.rules.Prism");

        algorithmChoice.addItemListener(this);


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
        inputChoicePanel.add("West", algorithmLabel);
        inputChoicePanel.add("Center", algorithmChoice);

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

        resultList = new ArrayList();
 
        // do something here!

        BufferedReader reader = null;
        int available = 0;

        messageLabel.setText("Trying to read URL...");
        // OK, this is how to read a URL as a BufferedReader
        Weka1 weka;
        try {
            reader = readURL(URLString);
            // matchedWords = readerToVector(reader);
            weka = new Weka1(reader);
            messageLabel.setText("Have read URL. Now generating result. It can take some time....");
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
        if (e.getSource() == algorithmChoice) {
            algorithm = algorithmChoice.getSelectedItem();
        }
        if (e.getSource() == URLChoice) {
            URLString = URLChoice.getSelectedItem();
            URLTextField.setText(URLString);
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
// compile-command: "javac WekaApplet1.java && java WekaApplet1"
// End: 
