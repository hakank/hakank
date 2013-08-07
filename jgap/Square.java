/*
  hakank: This is my take of the Square function.

*/


import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * Returns the square of a double value.
 *
 * @author Hakan Kjellerstrand
 */
public class Square
    extends MathCommand implements ICloneable {
    
    public Square(final GPConfiguration a_conf, Class a_returnType)
        throws InvalidConfigurationException {
        super(a_conf, 1, a_returnType);
    }
    
    public String toString() {
        return "&1^2";
    }
    
    /**
     * @return textual name of this command
     *
     * @author Hakan Kjellerstrand (based on Klaus Meffert's Log file)
     */
    public String getName() {
        return "Square";
    }
    
    public float execute_float(ProgramChromosome c, int n, Object[] args) {
        float f = c.execute_float(n, 0, args);
        return (float) f*f;
    }
    
    public double execute_double(ProgramChromosome c, int n, Object[] args) {
        double d = c.execute_double(n, 0, args);
        return d*d;
    }
    
    public Object execute_object(ProgramChromosome c, int n, Object[] args) {
    return ( (Compatible) c.execute_object(n, 0, args)).execute_square();
    }
    
  protected interface Compatible {
      public Object execute_square();
  }
    /**
     * Clones the object. Simple and straight forward implementation here.
     *
     * @return cloned instance of this object
     *
     * @author Hakan Kjellerstrand (based on Klaus Meffert's Log file)
     */
    public Object clone() {
        try {
            Square result = new Square(getGPConfiguration(), getReturnType());
            return result;
        } catch (Exception ex) {
            throw new CloneException(ex);
        }
    }
}
