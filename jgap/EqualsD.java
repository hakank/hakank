/*
 * 
 * Equals for double class. 
 * Returns 1.0 if abs(a-b) < DELTA, else 0.0.
 * 
 */
import org.apache.commons.lang.builder.*;
import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * The equals operation for double.
 *
 * @author Hakan Kjellerstrand
 */
public class EqualsD
    extends MathCommand implements ICloneable {

  private Class m_type;

  public EqualsD(final GPConfiguration a_conf)
      throws InvalidConfigurationException {
    this(a_conf, CommandGene.DoubleClass);
  }

  public EqualsD(final GPConfiguration a_conf, Class a_returnType)
      throws InvalidConfigurationException {
    super(a_conf, 2, a_returnType);
  }


  public String toString() {
    return "&1 == &2";
  }

  /**
   * @return textual name of this command
   *
   * @author Klaus Meffert
   * @since 3.2
   */
  public String getName() {
    return "EqualsD";
  }

  public double execute_double(ProgramChromosome c, int n, Object[] args) {
      double d1 = c.execute_double(n, 0, args);
      double d2 = c.execute_double(n, 1, args);
      if (Math.abs(d1-d2) < DELTA) {
          return 1.0d; 
      } else {
          return 0.0d;
      }
  }

  /**
   * Clones the object. Simple and straight forward implementation here.
   *
   * @return cloned instance of this object
   *
   * @author Klaus Meffert
   * @since 3.4
   */
  public Object clone() {
    try {
      EqualsD result = new EqualsD(getGPConfiguration(), getReturnType());
      return result;
    } catch (Exception ex) {
      throw new CloneException(ex);
    }
  }

}
