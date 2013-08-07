/*
 *
 * Loop as doubleClass (instead of voidClass)
 *
 */
import org.apache.commons.lang.builder.*;
import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * A loop that executes a given number of times on double values.
 *
 * @author Hakan Kjellerstrand
 */
public class LoopD
    extends CommandGene implements ICloneable {

  private Class m_typeVar;

  private int m_count;

  /**
   * Constructor.
   *
   * @param a_conf the configuration to use
   * @param a_typeVar Class of the loop counter terminakl (e.g. IntegerClass)
   * @param a_count the number of loops to perform
   * @throws InvalidConfigurationException
   *
   * @author Hakan Kjellerstrand (based on code from Klaus Meffert)
   */
  public LoopD(final GPConfiguration a_conf, Class a_typeVar, int a_count)
      throws InvalidConfigurationException {
    this(a_conf, a_typeVar, a_count, 0, 0);
  }

  public LoopD(final GPConfiguration a_conf, Class a_typeVar, int a_count,
              int a_subReturnType, int a_subChildType)
      throws InvalidConfigurationException {
    super(a_conf, 1, CommandGene.DoubleClass, a_subReturnType, a_subChildType);
    m_typeVar = a_typeVar;
    m_count = a_count;
  }

  public String toString() {
    return "loop(" + m_count + ", &1 }";
  }

  /**
   * @return textual name of this command
   *
   * @author Hakan Kjellerstrand
   */
  public String getName() {
    return "Loop";
  }

  public double execute_double(ProgramChromosome c, int n, Object[] args) {
    // Repeatedly execute the child.
    // -----------------------------

      // hakank: This is highly experimental!
      double x = 0.0d;
    for (int i = 0; i < m_count; i++) {
        x += c.execute_double(n, 0, args);
    }
    return x;
  }

  public boolean isValid(ProgramChromosome a_program) {
    return true;
  }

  public Class getChildType(IGPProgram a_ind, int a_chromNum) {
    return CommandGene.DoubleClass;
  }

  /**
   * The compareTo-method.
   *
   * @param a_other the other object to compare
   * @return -1, 0, 1
   *
   * @author Hakan Kjellerstrand (based on code by Klaus Meffert)
   */
  public int compareTo(Object a_other) {
    int result = super.compareTo(a_other);
    if (result != 0) {
      return result;
    }
    LoopD other = (LoopD) a_other;
    return new CompareToBuilder()
        .append(m_typeVar, other.m_typeVar)
        .append(m_count, other.m_count)
        .toComparison();
  }

  /**
   * The equals-method.
   *
   * @param a_other the other object to compare
   * @return true if the objects are seen as equal
   *
   * @author Hakan Kjellerstrand (based on code by Klaus Meffert)
   */
  public boolean equals(Object a_other) {
    try {
      LoopD other = (LoopD) a_other;
      return super.equals(a_other) && new EqualsBuilder()
          .append(m_typeVar, other.m_typeVar)
          .append(m_count, other.m_count)
          .isEquals();
    } catch (ClassCastException cex) {
      return false;
    }
  }

  /**
   * Clones the object. Simple and straight forward implementation here.
   *
   * @return cloned instance of this object
   *
   * @author Hakan Kjellerstrand (based on code by Klaus Meffert)
   */
  public Object clone() {
    try {
      LoopD result = new LoopD(getGPConfiguration(), m_typeVar, m_count,
                             getSubReturnType(), getSubChildType(0));
      return result;
    } catch (Exception ex) {
      throw new CloneException(ex);
    }
  }
}
