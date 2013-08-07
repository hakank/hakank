/*
 *
 * This version of Divide converts to Integer and makes an integer division,
 * and then converts to Double again.
 * Also, it is a protected version, i.e. returns 1 if the divisor is 0.
 *
 */

import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.gp.function.*;
import org.jgap.util.*;

/**
 * The divide operation on double, by first convert to int and then divides.
 *
 * @author Hakan Kjellerstrand
 */
public class DivideIntD
    extends MathCommand implements IMutateable, ICloneable {

  public DivideIntD(final GPConfiguration a_conf, Class a_returnType)
      throws InvalidConfigurationException {
    super(a_conf, 2, a_returnType);
  }

  public CommandGene applyMutation(int index, double a_percentage)
      throws InvalidConfigurationException {
    Multiply mutant = new Multiply(getGPConfiguration(), getReturnType());
    return mutant;
  }

  /**
   * Clones the object. Simple and straight forward implementation here.
   *
   * @return cloned instance of this object
   *
   * @author Hakan Kjellerstrand
   */
  public Object clone() {
    try {
      DivideIntD result = new DivideIntD(getGPConfiguration(), getReturnType());
      return result;
    } catch (Exception ex) {
      throw new CloneException(ex);
    }
  }

  public String toString() {
    return "/";
  }

  /**
   * @return textual name of this command
   *
   * @author Hakan Kjellerstrand
   */
  public String getName() {
    return "DivideIntD";
  }

  public double execute_double(ProgramChromosome c, int n, Object[] args) {
      double d1 = c.execute_double(n, 0, args);
      double d2 = c.execute_double(n, 1, args);

      Integer i1 = (int)d1;
      Integer i2 = (int)d2;
      
      // protect division by 0
      if (i2 == 0) {
          return 1;
      }

      return i1/i2;
  }

  public Object execute_object(ProgramChromosome c, int n, Object[] args) {
    return ( (Compatible) c.execute_object(n, 0, args)).execute_divide_int_d(c.
        execute_object(n, 1, args));
  }

  protected interface Compatible {
    public Object execute_divide_int_d(Object o);
  }
}
