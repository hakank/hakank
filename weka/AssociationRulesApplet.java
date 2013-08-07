/**
  *
  * Tue Apr  1 18:11:44 2003/hakank@bonetmail.com
  *
  *
  */

/*
  TODOs:
  * the user can change the baskets: one line a basket
  * the program calculate the Apriori stuff and show the result in a text area

  * maybe: random generate a couple of baskets
  * maybe: use Tertius as well
  * maybe: some more parameter for Apriori


  */

import java.io.*;
import java.util.*;
import java.text.*;
import java.applet.Applet;
import java.awt.*;
import java.awt.event.*;

// public class AssociationRulesApplet extends Applet implements ActionListener, ItemListener {
public class AssociationRulesApplet extends Applet implements ActionListener {

    private String[] inputText   = null;

    private StringBuffer result = null;

    Button associateButton;
    Button resetButton;

    Label associateLabel;
    Label resetLabel;
    Label messageLabel;
    Label numRulesLabel;
    Label minSupportLabel;
    Label minConfidenceLabel;

    Label inputTextLabel;


    TextField numRulesTextField;
    TextField minSupportTextField;
    TextField minConfidenceTextField;

    TextArea inputTextTextArea;
    TextArea resultTextArea;


    Panel buttonsPanel;
    Panel inputPanel; // inputChoicePanel; 
    Panel textPanel;
    Panel fieldsPanel;


    // apriori specific parameters
    int numRulesValue               = 20;
    String numRulesValueString      = "20";
    double minSupportValue          = 0.1; // lowerBoundMinSupport
    String minSupportValueString    = "0.1";
    double minConfidenceValue       =  0.9;      // minMetric
    String minConfidenceValueString = "0.9";



    public static void main(String args[]) {


    }

    public void init() {

        // buttons
        associateButton = new Button("Associate");
        resetButton    = new Button("Reset");

        // listeners
        associateButton.addActionListener(this);
        resetButton.addActionListener(this);

        // labels
        messageLabel       = new Label("");
        inputTextLabel     = new Label("Baskets: ");
        numRulesLabel      = new Label("Num. rules: ");
        minSupportLabel    = new Label("Min. support: ");
        minConfidenceLabel = new Label("Min. confidence: ");

        // textFields
        numRulesTextField = new TextField(numRulesValueString, 3);
        minSupportTextField = new TextField(minSupportValueString, 3);
        minConfidenceTextField = new TextField(minConfidenceValueString, 3);

        inputTextTextArea  = new TextArea(10,60);

        resultTextArea     = new TextArea(10,20);


        buttonsPanel = new Panel(new FlowLayout());
        buttonsPanel.add(associateButton);
        buttonsPanel.add(resetButton);

        textPanel = new Panel();
        textPanel.add(inputTextTextArea);

        fieldsPanel = new Panel(new FlowLayout());
        fieldsPanel.add(numRulesLabel);
        fieldsPanel.add(numRulesTextField);
        fieldsPanel.add(minSupportLabel);
        fieldsPanel.add(minSupportTextField);
        fieldsPanel.add(minConfidenceLabel);
        fieldsPanel.add(minConfidenceTextField);


        inputPanel = new Panel(new BorderLayout());
        inputPanel.add("North",  buttonsPanel);
        inputPanel.add("Center", fieldsPanel);
        inputPanel.add("South",  textPanel);

        setLayout(new BorderLayout());
        add("North",  inputPanel);
        add("Center", resultTextArea);
        add("South",  messageLabel);

        validate();

        associate();

    }

    public void actionPerformed(ActionEvent e) {

        String command = e.getActionCommand();        
        if ("Associate".equals(command)) {
            associate();
        } else if ("Reset".equals(command)) {
            reset();
        }

    } // end actionPerformed



    /**
     * associate cases
     *
     */ 
    public void associate() {

        result = new StringBuffer();

        //
        // get parameters
        //
        ArrayList textParamList = new ArrayList();

        int paramIx = 1;
        boolean moreParams = true;
        while (moreParams) {
            try {
                String textParam = this.getParameter("text" + paramIx);
                if (textParam == null) {
                    moreParams = false;
                    break;
                } else {
                    textParamList.add(textParam);
                    paramIx++;
                }
            } catch (Exception e) {
                result.append("" + e);
                moreParams = false;
            }
        }


        messageLabel.setText("");
        resultTextArea.setText("");

        AssociationRules associator;
        inputText    = inputTextTextArea.getText().split("\\n");
        
        if (inputText.length <= 1) {
            inputText = (String[])textParamList.toArray(new String[0]);
            inputTextTextArea.setText(arrayToString(inputText,"\n"));
        }

        messageLabel.setText("Trying to associate baskets...");

        try {
            numRulesValueString = numRulesTextField.getText();
            numRulesValue = Integer.parseInt(numRulesValueString);
        } catch (Exception e) {
            messageLabel.setText("Num rules: Please type an integer!");
            return;
        }


        try {
            minSupportValueString = minSupportTextField.getText();
            minSupportValue = Double.parseDouble(minSupportValueString);
        } catch (Exception e) {
            messageLabel.setText("Min support: Please type an integer!");
            return;
        }

        try {
            minConfidenceValueString = minConfidenceTextField.getText();
            minConfidenceValue = Double.parseDouble(minConfidenceValueString);
        } catch (Exception e) {
            messageLabel.setText("Min confidence: Please type an integer!");
            return;
        }


        try {

            associator = new AssociationRules(inputText);
            associator.setNumRules(numRulesValue);
            associator.setMinMetric(minConfidenceValue);
            associator.setLowerBoundMinSupport(minSupportValue);
            result.append(associator.associate());

            messageLabel.setText("OK!");

        } catch(Exception e) {
            result.append("Exception: " + e.toString());
        }


        resultTextArea.setText(result.toString());
   
    } // end associate


    /**
     *  resets the fields
     *
     */
    public void reset() {

        resultTextArea.setText("");
        messageLabel.setText("");
        inputTextTextArea.setText("");

    } // end reset


    //
    // array -> String[]
    //
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


