/*
  hakank: This is my take of the Logistic function.

  I just copied the org.jgap.gp.function.Log and changed
  accordingly...

*/


import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * Returns the natural logarithm (base e) of a double value.
 *
 * @author Klaus Meffert
 * @since 3.3.4
 */
public class Logistic
    extends MathCommand implements ICloneable {
    /** String containing the CVS revision. Read out via reflection!*/
    // private final static String CVS_REVISION = "$Revision: 1.3 $";
    
    public Logistic(final GPConfiguration a_conf, Class a_returnType)
        throws InvalidConfigurationException {
        super(a_conf, 1, a_returnType);
    }
    
    public String toString() {
        return "logistic &1";
    }
    
    /**
     * @return textual name of this command
     *
     * @author Hakan Kjellerstrand (based on Klaus Meffert's Log file)
     */
    public String getName() {
        return "Logistic";
    }
    
    public float execute_float(ProgramChromosome c, int n, Object[] args) {
        float f = c.execute_float(n, 0, args);
        return (float) (1.0/((float)1.0+Math.exp(-f)));
    }
    
    public double execute_double(ProgramChromosome c, int n, Object[] args) {
        double d = c.execute_double(n, 0, args);
        return 1.0/(1.0+Math.exp(-d));
    }
    
    public Object execute_object(ProgramChromosome c, int n, Object[] args) {
    return ( (Compatible) c.execute_object(n, 0, args)).execute_logistic();
    }
    
  protected interface Compatible {
      public Object execute_logistic();
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
            Logistic result = new Logistic(getGPConfiguration(), getReturnType());
            return result;
        } catch (Exception ex) {
            throw new CloneException(ex);
        }
    }
}
