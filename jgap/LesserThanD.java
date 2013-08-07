/*
 *
 * LesserThanD: < for doubleClass
 *
 * Makes it applicable to Integer and Double
 *
 */


import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * LesserThan
 *
 * @author Hakan Kjellerstrand
 *
 */
public class LesserThanD
    extends MathCommand implements ICloneable {
    /** String containing the CVS revision. Read out via reflection!*/
    // private final static String CVS_REVISION = "$Revision: 1.8 $";
    
    public LesserThanD(final GPConfiguration a_conf)
        throws InvalidConfigurationException {
        this(a_conf, CommandGene.DoubleClass);
    }
    
    public LesserThanD(final GPConfiguration a_conf, Class a_returnType)
        throws InvalidConfigurationException {
        super(a_conf, 2, a_returnType);
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
            LesserThanD result = new LesserThanD(getGPConfiguration(), getReturnType());
            return result;
        } catch (Exception ex) {
            throw new CloneException(ex);
        }
    }
    
    public String toString() {
        return "&1 < &2";
    }
    
    /**
     * @return textual name of this command
     *
     * @author Klaus Meffert
     * @since 3.2
     */
    public String getName() {
        return "LesserThan";
    }
    
    public double execute_double(ProgramChromosome c, int n, Object[] args) {
        double d1 = c.execute_double(n, 0, args);
        double d2 = c.execute_double(n, 1, args);
        if (d1 < d2) {
            return 1.0d;
        } else {
            return 0.0d;
        }
    }
}
