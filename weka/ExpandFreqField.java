/**
  *
  * 2007-03-08/Hakan Kjellerstrand (hakank@bonetmail.com)
  *
  * Expands the number of instances depending on the value in the attribute
  *  given in freqField (parameter -F).
  *
  */

package weka.filters.unsupervised.instance;
import java.io.*;
import java.util.*;
import weka.core.*;

import weka.filters.*;
import weka.core.Capabilities;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.Option;
import weka.core.OptionHandler;
import weka.core.Utils;
import weka.core.SingleIndex;
import weka.core.Capabilities.Capability;
import weka.filters.Filter;
import weka.filters.UnsupervisedFilter;

import java.util.Enumeration;
import java.util.Vector;


/** 
 <!-- globalinfo-start -->
 * An instance filter that copies the instances according to a frequency attribute.
 * <p/>
 <!-- globalinfo-end -->
 * 
 <!-- options-start -->
 * Valid options are: <p/>
 * 
 * <pre> -F &lt;num&gt;
 *  Specify the attribute where the frequency resides  (default last)</pre>
 * 
 <!-- options-end -->
 *
 * @author Hakan Kjellerstrand
 * @version $Revision: 1.0 $
 */
public class ExpandFreqField 
  extends Filter 
  implements UnsupervisedFilter, OptionHandler {

  /* The attribute position for the frequency */
  private SingleIndex m_FreqField = new SingleIndex("last");


  /**
   * Returns a string describing this filter
   *
   * @return a description of the filter suitable for
   * displaying in the explorer/experimenter gui
   */
  public String globalInfo() {
    return "A filter to duplicate instances with a frequency attribute.";
  }

  /**
   * Returns the tip text for this property
   *
   * @return tip text for this property suitable for
   * displaying in the explorer/experimenter gui
   */
  public String freqFieldTipText() {
    return "The attribute to fetch the frequency, 1-based. last is default";
  }


  /**
   * Returns an enumeration describing the available options.
   *
   * @return an enumeration of all the available options.
   */
  public Enumeration listOptions() {
    Vector newVector = new Vector(1);
    newVector.addElement(new Option(
                                    "\tSpecify the frequency attribute. last is default",
                                    "F", 1, "-F <num>"));
    return newVector.elements();
  }


  /**
   * Parses a given list of options. <p/>
   * 
   <!-- options-start -->
   * Valid options are: <p/>
   * 
   * <pre> -F &lt;num&gt;
   *  Specify the attribute position of the frequency (default last)</pre>
   * 
   <!-- options-end -->
   *
   * @param options the list of options as an array of strings
   * @throws Exception if an option is not supported
   */
  public void setOptions(String[] options) throws Exception {
    String freqFieldString = Utils.getOption("F", options);

    if (freqFieldString.length() != 0) {
      setFreqField(freqFieldString);
    } else {
      setFreqField("last");
    }
  }


  /**
   * Gets the current settings of the filter.
   *
   * @return an array of strings suitable for passing to setOptions
   */
  public String[] getOptions() {
    String[] options = new String[2];
    int current = 0;

    options[current++] = "-F";
    options[current++] = "" + getFreqField();

    return options;
  }


  /**
   * Get the instance's frequency, i.e. the number of instances to copy.
   *
   * @return the frequency attribute position
   */
  public String getFreqField() {
    return m_FreqField.getSingleIndex();
  }


  /**
   * Sets the instance's frequency, i.e. the number of instances to copy.
   *
   * @param newFreqField the attribute position for the frequency field, default last.
   */
  public void setFreqField(String newFreqField) {
    m_FreqField.setSingleIndex(newFreqField);
  }


  /** 
   * Returns the Capabilities of this filter.
   *
   * @return            the capabilities of this object
   * @see               Capabilities
   */
  public Capabilities getCapabilities() {
    Capabilities result = super.getCapabilities();
    
    // attributes
    result.enableAllAttributes();
    result.enable(Capability.MISSING_VALUES);
    
    // class
    result.enableAllClasses();
    result.enable(Capability.MISSING_CLASS_VALUES);
    result.enable(Capability.NO_CLASS);
    
    return result;
  }
  

  /**
   * Sets the format of the input instances.
   *
   * @param instanceInfo an Instances object containing the input instance
   * structure (any instances contained in the object are ignored - only the
   * structure is required).
   * @return true if the outputFormat may be collected immediately
   * @throws Exception if format cannot be processed
   */
  public boolean setInputFormat(Instances instanceInfo) throws Exception {

    m_FreqField.setUpper(instanceInfo.numAttributes() - 1);
    
    if (!instanceInfo.attribute(m_FreqField.getIndex()).isNumeric()) {
      throw new Exception("Attribute " + m_FreqField + " must be numeric");
    }
        
    super.setInputFormat(instanceInfo);
    setOutputFormat(instanceInfo);
    
    return true;
  }


  /**
   * Input an instance for filtering. Ordinarily the instance is processed
   * and made available for output immediately. Some filters require all
   * instances be read before producing output.
   *
   * @param instance the input instance
   * @return true if the filtered instance may now be
   * collected with output().
   * @throws IllegalStateException if no input format has been defined.
   */
  public boolean input(Instance instance) throws Exception {
    if (getInputFormat() == null) {
      throw new IllegalStateException("No input instance format defined");
    }


    if (m_NewBatch) {
      resetQueue();
      m_NewBatch = false;
    }
    Instance inst = (Instance) instance.copy();

    // Now copy the number in freqNum - 1 instances.
    try {
      int attIndex = m_FreqField.getIndex();
      int freqNum = new Double(inst.value(attIndex)).intValue();
      for (int i = 0; i < freqNum - 1; i++) {
        push(inst);
      }

    } catch (Exception e) {
      e.printStackTrace();
      return false;
    }

    // The normal push
    push(inst);

    return true;
  }  


  /**
   * Main method for testing this class.
   *
   * @param argv should contain arguments to the filter: use -h for help
   */

  public static void main(String argv[]) {

    try {
      runFilter(new ExpandFreqField(), argv);
    } catch(Exception e) {
      e.printStackTrace();
    }

  } // end main

} // end class

