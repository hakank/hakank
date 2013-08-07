/*
 *
 * GreaterThanOrEqualD (>=) for doubleClass
 *
 *
 */


import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * The GreaterThan on double values.
 *
 * @author Hakan Kjellerstrand
 *
 */
public class GreaterThanOrEqualD
    extends MathCommand implements ICloneable {
    
    public GreaterThanOrEqualD(final GPConfiguration a_conf)
        throws InvalidConfigurationException {
        this(a_conf, CommandGene.DoubleClass);
    }
    
    public GreaterThanOrEqualD(final GPConfiguration a_conf, Class a_returnType)
        throws InvalidConfigurationException {
        super(a_conf, 2, a_returnType);
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
            GreaterThanOrEqualD result = new GreaterThanOrEqualD(getGPConfiguration(), getReturnType());
            return result;
        } catch (Exception ex) {
            throw new CloneException(ex);
        }
    }
    
    public String toString() {
        return "&1 >= &2";
    }
    
    /**
     * @return textual name of this command
     *
     * @author Hakan Kjellerstrand
     */
    public String getName() {
        return "GreaterThanOrEqual";
    }
    
    public double execute_double(ProgramChromosome c, int n, Object[] args) {
        double d1 = c.execute_double(n, 0, args);
        double d2 = c.execute_double(n, 1, args);
        if (d1 >= d2) {
            return 1.0d;
        } else {
            return 0.0d;
        }
    }
}
