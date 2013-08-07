/*
 *
 * A double version of ForLoopD, based on
 *   org.jgap.gp.function.ForLoopD
 * 
 * 
 */

import org.apache.commons.lang.builder.*;
import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * The for-loop. You can preset the start index and the end index. If the latter
 * is not given, it is dynamically computed from a child.
 * hakank: I adapted it for doubleClass
 *
 * @author Hakan Kjellerstrand
 */
public class ForLoopD
    extends CommandGene implements ICloneable {

  private static String INTERNAL_COUNTER_STORAGE = "FORLOOPSTORAGE_INT";

  private Class m_typeVar;

  private int m_startIndex;

  private int m_endIndex;

  private int m_increment;

  private int m_maxLoop;

  private String m_memory_name_int;

  private String m_varName;

  /**
   * Constructor.
   *
   * @param a_conf the configuration to use
   * @param a_typeVar Class of the loop counter terminakl (e.g. IntegerClass)
   * @param a_maxLoop the maximum number of loops to perform
   * @throws InvalidConfigurationException
   *
   * @author Klaus Meffert
   */
  public ForLoopD(final GPConfiguration a_conf, Class a_typeVar, int a_maxLoop)
      throws InvalidConfigurationException {
    this(a_conf, a_typeVar, 0, a_maxLoop);
  }

  /**
   * Constructor allowing to preset the starting index of the loop.
   *
   * @param a_conf the configuration to use
   * @param a_typeVar Class of the loop counter terminakl (e.g. IntegerClass)
   * @param a_startIndex index to start the loop with (normally 0)
   * @param a_maxLoop the maximum number of loops to perform
   * @throws InvalidConfigurationException
   *
   * @author Klaus Meffert
   * @since 3.0
   */
  public ForLoopD(final GPConfiguration a_conf, Class a_typeVar,
                 int a_startIndex, int a_maxLoop)
      throws InvalidConfigurationException {
    this(a_conf, a_typeVar, a_startIndex, a_maxLoop, "i");
  }

  public ForLoopD(final GPConfiguration a_conf, Class a_typeVar,
                 int a_startIndex, int a_maxLoop, String a_varName)
      throws InvalidConfigurationException {
    super(a_conf, 2, CommandGene.DoubleClass);
    m_typeVar = a_typeVar;
    m_maxLoop = a_maxLoop;
    m_startIndex = a_startIndex;
    m_endIndex = -1;
    m_increment = 1;
    m_varName = a_varName;
    init();
  }

  /**
   * Constructor allowing to preset the starting and the ending index of the
   * loop.
   *
   * @param a_conf the configuration to use
   * @param a_typeVar Class of the loop counter terminal (e.g. IntegerClass)
   * @param a_startIndex index to start the loop with
   * @param a_endIndex index to end the loop with
   * @param a_increment the maximum number of loops to perform
   * @param a_varName informal textual name of the loop counter variable
   * @throws InvalidConfigurationException
   *
   * @author Klaus Meffert
   * @since 3.2
   */
  public ForLoopD(final GPConfiguration a_conf, Class a_typeVar,
                 int a_startIndex, int a_endIndex, int a_increment,
                 String a_varName)
      throws InvalidConfigurationException {
    this(a_conf, a_typeVar, a_startIndex, a_endIndex, a_increment, a_varName, 0,
         0);
  }

  public ForLoopD(final GPConfiguration a_conf, Class a_typeVar,
                 int a_startIndex, int a_endIndex, int a_increment,
                 String a_varName, int a_subReturnType, int a_subChildType)
      throws InvalidConfigurationException {
    super(a_conf, 1, CommandGene.DoubleClass, a_subReturnType, a_subChildType);
    m_typeVar = a_typeVar;
    m_increment = a_increment;
    m_startIndex = a_startIndex;
    m_endIndex = a_endIndex;
    m_varName = a_varName;
    init();
  }

  protected void init() {
    super.init();
    // Generate unique name.
    // ---------------------
    m_memory_name_int = INTERNAL_COUNTER_STORAGE;
    m_memory_name_int += m_varName;
    m_memory_name_int += getGPConfiguration().getRandomGenerator().nextDouble();
  }

  public String toString() {
    if (m_endIndex == -1) {
      return "for(int i=" + m_startIndex + ";i<&1;i++) { &2 }";
    }
    else {
      String incrString;
      if (m_increment == 1) {
        incrString = m_varName + "++";
      }
      else {
        incrString = m_varName + "=" + m_varName + "+1";
      }
      return "for(int " + m_varName + "=" + m_startIndex + ";" + m_varName +
          "<" + m_endIndex + ";" +
          incrString + ") { &1 }";
    }
  }

  /**
   * @return textual name of this command
   *
   * @author Klaus Meffert
   * @since 3.2
   */
  public String getName() {
    return "ForLoopD";
  }

  public Object execute_object(ProgramChromosome c, int n, Object[] args) {
    StringBuffer value = new StringBuffer();
    value = value.append("for(int "
                         + m_varName
                         + "="
                         + m_startIndex
                         + ";"
                         + m_varName
                         + "<"
                         + m_endIndex + ";"
                         + m_varName + "++) {");
    for (int i = 0; i < size(); i++) {
      value = value.append( (StringBuffer) c.execute_object(n, i, args));
    }
    value = value.append("}");
    return value;
  }

  public void execute_void(ProgramChromosome c, int n, Object[] args) {
    // Determine the end index of the loop (child at index 0).
    // -------------------------------------------------------
    int x;
    if (m_endIndex == -1) {
      if (m_typeVar == CommandGene.IntegerClass) {
        x = c.execute_int(n, 0, args);
      }
      else if (m_typeVar == CommandGene.LongClass) {
        x = (int) c.execute_long(n, 0, args);
      }
      else if (m_typeVar == CommandGene.DoubleClass) {
        x = (int) Math.round(c.execute_double(n, 0, args));
      }
      else if (m_typeVar == CommandGene.FloatClass) {
        x = (int) Math.round(c.execute_float(n, 0, args));
      }
      else {
        throw new RuntimeException("Type "
                                   + m_typeVar
                                   + " not supported by ForLoopD");
      }
      if (x > m_maxLoop) {
        x = m_maxLoop;
      }
      // Repeatedly execute the second child (index = 1).
      // ------------------------------------------------
      for (int i = m_startIndex; i < x; i++) {
        c.execute_void(n, 1, args);
      }
    }
    else {
      // Repeatedly execute the first child (index = 0).
      // -----------------------------------------------
      for (int i = m_startIndex; i < m_endIndex; i = i + m_increment) {
        // Store counter in memory.
        // ------------------------
        getGPConfiguration().storeInMemory(ForLoopD.INTERNAL_COUNTER_STORAGE,
            new Integer(i));
        c.execute_void(n, 0, args);
      }
    }
  }


    // hakank: added this double version
  public double execute_double(ProgramChromosome c, int n, Object[] args) {
    // Determine the end index of the loop (child at index 0).
    // -------------------------------------------------------
    int x;
    if (m_endIndex == -1) {

      if (m_typeVar == CommandGene.DoubleClass) {
        x = (int) Math.round(c.execute_double(n, 0, args));
      } else {
        throw new RuntimeException("Type "
                                   + m_typeVar
                                   + " not supported by ForLoopD");
      }
      if (x > m_maxLoop) {
        x = m_maxLoop;
      }
      double res = 0.0;
      // Repeatedly execute the second child (index = 1).
      // ------------------------------------------------
      for (int i = m_startIndex; i < x; i++) {
          res += c.execute_double(n, 1, args);
      }
      return res;
    }
    else {
        double res = 0.0;
      // Repeatedly execute the first child (index = 0).
      // -----------------------------------------------
      for (int i = m_startIndex; i < m_endIndex; i = i + m_increment) {
        // Store counter in memory.
        // ------------------------
        getGPConfiguration().storeInMemory(ForLoopD.INTERNAL_COUNTER_STORAGE,
            new Integer(i));
        res += c.execute_double(n, 0, args);
      }
      return res;
    }
    
  }


  public boolean isValid(ProgramChromosome a_program) {
    return true;
  }

  public Class getChildType(IGPProgram a_ind, int a_chromNum) {
    if (m_endIndex == -1) {
      // Variant A: dynamic end index
      if (a_chromNum == 0) {
        // Loop counter variable.
        // ----------------------
        return m_typeVar;
      }
      else {
        // Subprogram.
        // -----------
        return CommandGene.DoubleClass;
      }
    }
    else {
      // Variant B: fixed end index
      return CommandGene.DoubleClass;
    }
  }

  /**
   * @return symbolic name of the variable name used in the for header.
   *
   * @author Klaus Meffert
   */
  public String getVarName() {
    return m_varName;
  }

  /**
   * The compareTo-method.
   *
   * @param a_other the other object to compare
   * @return -1, 0, 1
   *
   * @author Klaus Meffert
   * @since 3.0
   */
  public int compareTo(Object a_other) {
    int result = super.compareTo(a_other);
    if (result != 0) {
      return result;
    }
    ForLoopD other = (ForLoopD) a_other;
    return new CompareToBuilder()
        .append(m_typeVar, other.m_typeVar)
        .append(m_maxLoop, other.m_maxLoop)
        .append(m_startIndex, other.m_startIndex)
        .append(m_endIndex, other.m_endIndex)
        .append(m_increment, other.m_increment)
        .toComparison();
  }

  /**
   * The equals-method.
   *
   * @param a_other the other object to compare
   * @return true if the objects are seen as equal
   *
   * @author Klaus Meffert
   */
  public boolean equals(Object a_other) {
    try {
      ForLoopD other = (ForLoopD) a_other;
      return super.equals(a_other) && new EqualsBuilder()
          .append(m_typeVar, other.m_typeVar)
          .append(m_maxLoop, other.m_maxLoop)
          .append(m_startIndex, other.m_startIndex)
          .append(m_endIndex, other.m_endIndex)
          .append(m_increment, other.m_increment)
          .isEquals();
    } catch (ClassCastException cex) {
      return false;
    }
  }

  /**
   * @return name of the memory cell where the current value of the loop
   * variable is stored
   *
   * @author Klaus Meffert
   */
  public String getCounterMemoryName() {
    return m_memory_name_int;
  }

  /**
   * Clones the object. Simple and straight forward implementation here.
   *
   * @return cloned instance of this object
   *
   * @author Klaus Meffert
   */
  public Object clone() {
    try {
      ForLoopD result;
      if (getArity(null) == 1) {
        result = new ForLoopD(getGPConfiguration(), m_typeVar,
                             m_startIndex, m_endIndex, m_increment,
                             m_varName, getSubReturnType(),
                             getSubChildType(0));
      }
      else {
        result = new ForLoopD(getGPConfiguration(), m_typeVar,
                             m_startIndex, m_maxLoop,
                             m_varName);
      }
      return result;
    } catch (Exception ex) {
      throw new CloneException(ex);
    }
  }
}
