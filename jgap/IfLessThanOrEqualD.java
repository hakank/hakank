/*
 *
 * If Less Than of Equal Then ... Else for doubleClass.
 *
 * if a <= b then 
 *   c 
 * else 
 *   d
 *
 * This function was inspired by John Koza's function IFLTE in his first GP book
 * "Genetic Programming - On the Programming of Computers by Means of Natural Selection" 
 *
 */
import org.apache.commons.lang.builder.*;
import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * The if-less-than-or-equals-then-else construct for double.
 *
 * @author Hakan Kjellerstrand
 * 
 */
public class IfLessThanOrEqualD
    extends CommandGene implements ICloneable {

  private Class m_type;

  public IfLessThanOrEqualD(final GPConfiguration a_conf, Class a_type)
      throws InvalidConfigurationException {
      this(a_conf, a_type, 0, null);
  }

  public IfLessThanOrEqualD(final GPConfiguration a_conf, Class a_type, int a_subReturnType,
                int[] a_subChildTypes)
      throws InvalidConfigurationException {
    super(a_conf, 4, CommandGene.DoubleClass, a_subReturnType, a_subChildTypes);
    m_type = a_type;
  }


  public String toString() {
    return "if(&1 <= &2) then (&3) else(&4)";
  }

  /**
   * @return textual name of this command
   *
   * @author Hakan Kjellerstrand
   */
  public String getName() {
    return "IfLessThanOrEqualD";
  }

  public double execute_double(ProgramChromosome c, int n, Object[] args) {
    check(c);
    boolean condition;
    double d1 = c.execute_double(n, 0, args);
    double d2 = c.execute_double(n, 1, args);

    if (d1 <= d2) {
        return c.execute_double(n, 2, args);
    } else {
        return c.execute_double(n, 3, args);
    }
  }

  /**
   * Determines which type a specific child of this command has.
   *
   * @param a_ind ignored here
   * @param a_chromNum index of child
   * @return type of the a_chromNum'th child
   *
   * @author Hakan Kjellerstrand
   */
  public Class getChildType(IGPProgram a_ind, int a_chromNum) {
    if (a_chromNum == 0) {
      return m_type;
    }
    return CommandGene.DoubleClass;
  }

  /**
   * Clones the object. Simple and straight forward implementation here.
   *
   * @return cloned instance of this object
   */
  public Object clone() {
    try {
      IfLessThanOrEqualD result = new IfLessThanOrEqualD(getGPConfiguration(), m_type,
                                 getSubReturnType(), getSubChildTypes());
      return result;
    } catch (Exception ex) {
      throw new CloneException(ex);
    }
  }

  /**
   * The equals method.
   *
   * @param a_other the other object to compare
   * @return true if the objects are seen as equal
   *
   */
  public boolean equals(Object a_other) {
    if (a_other == null || ! (a_other instanceof IfLessThanOrEqualD)) {
      return false;
    }
    if (!super.equals(a_other)) {
      return false;
    }
    IfLessThanOrEqualD other = (IfLessThanOrEqualD) a_other;
    return new EqualsBuilder().append(m_type, other.m_type).isEquals();
  }
}
