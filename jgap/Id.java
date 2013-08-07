/*
 * 
 * The no op id function on a double value.
 * 
 */
import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * The No op Id operation.
 *
 * @author Hakan Kjellerstrand
 */
public class Id
    extends MathCommand implements ICloneable {

  public Id(final GPConfiguration a_conf)
      throws InvalidConfigurationException {
    super(a_conf, 1, CommandGene.DoubleClass);
  }

  public String toString() {
    return "id &1";
  }

  /**
   * @return textual name of this command
   *
   * @author Hakan Kjellerstrand
   */
  public String getName() {
    return "Id";
  }

  public double execute_double(ProgramChromosome c, int n, Object[] args) {
      double d = c.execute_double(n, 0, args);
      return d;
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
      Id result = new Id(getGPConfiguration());
      return result;
    } catch (Exception ex) {
      throw new CloneException(ex);
    }
  }
}
