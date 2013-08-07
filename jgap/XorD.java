/*
 * Boolean Xor on double values.
 *
 */
import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * The boolean xor operation on double values.
 *
 * @author Hakan Kjellerstrand
 */
public class XorD
    extends MathCommand
implements ICloneable {

  public XorD(final GPConfiguration a_conf)
      throws InvalidConfigurationException {
    super(a_conf, 2, CommandGene.DoubleClass);
  }

  public CommandGene applyMutation(int index, double a_percentage)
      throws InvalidConfigurationException {
    CommandGene mutant;
    if (a_percentage < 0.5d) {
      mutant = new AndD(getGPConfiguration());
    }
    else {
      mutant = new OrD(getGPConfiguration());
    }
    return mutant;
  }

  public String toString() {
    return "&1 ^ &2";
  }

  /**
   * @return textual name of this command
   *
   * @author Hakan Kjellerstrand
   */
  public String getName() {
    return "XorD";
  }

  public double execute_double(ProgramChromosome c, int n, Object[] args) {
      double d1 = c.execute_double(n, 0, args);
      double d2 = c.execute_double(n, 1, args);
      boolean b1 = true;
      boolean b2 = true;
      if (d1 < 1.0d) {
          b1 = false;
      }
      if (d2 < 1.0d) {
          b2 = false;
      }
      if (b1^b2) {
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
   * @author Hakan Kjellerstrand
   */
  public Object clone() {
    try {
      XorD result = new XorD(getGPConfiguration());
      return result;
    } catch (Exception ex) {
      throw new CloneException(ex);
    }
  }
}
