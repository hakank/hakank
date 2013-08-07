/*
 * My version of round for double
 *
 */
import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * Returns the closest value to the argument.
 *
 * @author Hakan Kjellerstrand
 */
public class RoundD
    extends MathCommand implements ICloneable {

  public RoundD(final GPConfiguration a_conf, Class a_returnType)
      throws InvalidConfigurationException {
    super(a_conf, 1, a_returnType);
  }

  public String toString() {
    return "round &1";
  }

  /**
   * @return textual name of this command
   *
   * @author Hakan Kjellerstrand
   */
  public String getName() {
    return "RoundD";
  }

  public float execute_float(ProgramChromosome c, int n, Object[] args) {
    float f = c.execute_float(n, 0, args);
    return Math.round(f);
  }

  public double execute_double(ProgramChromosome c, int n, Object[] args) {
    double d = c.execute_double(n, 0, args);
    return Math.round(d);
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
      RoundD result = new RoundD(getGPConfiguration(), getReturnType());
      return result;
    } catch (Exception ex) {
      throw new CloneException(ex);
    }
  }
}
