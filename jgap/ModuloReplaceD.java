/*
 *
 * Modulo function on double values, where 0 is replaced with another value.
 * 
 */
import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * The modulo operation on double, replace 0 with another value.
 *
 * @author Hakan Kjellerstrand
 */
public class ModuloReplaceD

    extends MathCommand implements ICloneable {

    // the value to replace 0 with
    Integer replace = 0;

  public ModuloReplaceD(final GPConfiguration a_conf, Class a_returnType, int _replace)
      throws InvalidConfigurationException {
    super(a_conf, 2, a_returnType);
    replace = _replace;
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
    return "ModuloReplaceD";
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

    if (i1 == Integer.MAX_VALUE || i2 == Integer.MAX_VALUE ||
        i1 == Integer.MIN_VALUE || i2 == Integer.MIN_VALUE
        ) {
        return Integer.MAX_VALUE;
    }

    if (i2 == 0) {
        return 0;
    }

    Integer res = i1 % i2;
    if (res == 0) {
        return replace;
    }
    // System.out.println(i1 + " % " + i2 + " = " + res);
    return res;
  }

  public Object execute_object(ProgramChromosome c, int n, Object[] args) {
    try {
      return ( (Compatible) c.execute_object(n, 0, args)).execute_mod_replace_d(c.
          execute_object(n, 1, args));
    } catch (ArithmeticException aex) {
      throw new IllegalStateException("mod with illegal arguments");
    }
  }

  protected interface Compatible {
    public Object execute_mod_replace_d(Object o);
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
        ModuloReplaceD result = new ModuloReplaceD(getGPConfiguration(), getReturnType(), replace);
      return result;
    } catch (Exception ex) {
      throw new CloneException(ex);
    }
  }
}
