/*
 *
 * Modulo on double values.
 *
 */
import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * The modulo operation.
 *
 * @author Hakan Kjellerstrand
 */
public class ModuloD
    extends MathCommand implements ICloneable {

  public ModuloD(final GPConfiguration a_conf, Class a_returnType)
      throws InvalidConfigurationException {
    super(a_conf, 2, a_returnType);
  }

  public String toString() {
    return "&1 mod &2";
  }

  /**
   * @return textual name of this command
   *
   * @author Hakan Kjellerstrand
   */
  public String getName() {
    return "ModuloD";
  }

  // hakank: First truncate to Integer and then take the modulo
  public double execute_double(ProgramChromosome c, int n, Object[] args) {
    double v1 = c.execute_double(n, 0, args);
    double v2 = c.execute_double(n, 1, args);
    if (Math.abs(v2) < DELTA) {
      return 0;
    }

    Integer i1 = (int)v1;
    Integer i2 = (int)v2;

    if (i2 == 0) {
        return 0;
    }

    return i1 % i2;
  }

  public Object execute_object(ProgramChromosome c, int n, Object[] args) {
    try {
      return ( (Compatible) c.execute_object(n, 0, args)).execute_mod_d(c.
          execute_object(n, 1, args));
    } catch (ArithmeticException aex) {
      throw new IllegalStateException("mod with illegal arguments");
    }
  }

  protected interface Compatible {
    public Object execute_mod_d(Object o);
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
      ModuloD result = new ModuloD(getGPConfiguration(), getReturnType());
      return result;
    } catch (Exception ex) {
      throw new CloneException(ex);
    }
  }
}
