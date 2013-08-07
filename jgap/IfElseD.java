/*
 *
 * If Then Else for doubleClass.
 *
 */
import org.apache.commons.lang.builder.*;
import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * The if-then-else construct.
 *
 * @author Hakan Kjellerstrand
 * 
 */
public class IfElseD
    extends CommandGene implements ICloneable {
  /** String containing the CVS revision. Read out via reflection!*/
  // private final static String CVS_REVISION = "$Revision: 1.13 $";

  private Class m_type;

  public IfElseD(final GPConfiguration a_conf, Class a_type)
      throws InvalidConfigurationException {
    this(a_conf, a_type, 0, null);
  }

  public IfElseD(final GPConfiguration a_conf, Class a_type, int a_subReturnType,
                int[] a_subChildTypes)
      throws InvalidConfigurationException {
    super(a_conf, 3, CommandGene.DoubleClass, a_subReturnType, a_subChildTypes);
    m_type = a_type;
  }


  public String toString() {
    return "if(&1) then (&2) else(&3)";
  }

  /**
   * @return textual name of this command
   *
   * @author Hakan Kjellerstrand
   */
  public String getName() {
    return "IfElseD";
  }

  public double execute_double(ProgramChromosome c, int n, Object[] args) {
    check(c);
    boolean condition;
    if (c.execute_double(n, 0, args) > 0) {
        return c.execute_double(n, 1, args);
    } else {
        return c.execute_double(n, 2, args);
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
      IfElseD result = new IfElseD(getGPConfiguration(), m_type,
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
    if (a_other == null || ! (a_other instanceof IfElseD)) {
      return false;
    }
    if (!super.equals(a_other)) {
      return false;
    }
    IfElseD other = (IfElseD) a_other;
    return new EqualsBuilder().append(m_type, other.m_type).isEquals();
  }
}
