/*
 *
 * Boolean operator Not on double value.
 *
 */
import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * The boolean not operation on double value.
 *
 * @author Hakan Kjellerstrand
 */
public class NotD
    extends MathCommand implements ICloneable {

  public NotD(final GPConfiguration a_conf)
      throws InvalidConfigurationException {
    super(a_conf, 1, CommandGene.DoubleClass);
  }

  public String toString() {
    return "!&1";
  }

  /**
   * @return textual name of this command
   *
   * @author Hakan Kjellerstrand
   */
  public String getName() {
    return "NotD";
  }

  public double execute_double(ProgramChromosome c, int n, Object[] args) {
      double d = c.execute_double(n, 0, args);
      boolean b = true;
      if (d < 1.0d) {
          b = false;
      }

      if (b) {
          return 0.0d; // not true: false
      } else {
          return 1.0d; // not false: true
      }
  }


  /**
   * Clones the object. Simple and straight forward implementation here.
   *
   * @return cloned instance of this object
   *
   * @author Hakan Kjellerstrand
   * @since 3.4
   */
  public Object clone() {
    try {
      NotD result = new NotD(getGPConfiguration());
      return result;
    } catch (Exception ex) {
      throw new CloneException(ex);
    }
  }
}
