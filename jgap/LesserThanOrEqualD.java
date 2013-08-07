/*
 *
 * LesserThanOrEqualD: < for doubleClass
 *
 */


import org.jgap.*;
import org.jgap.gp.*;
import org.jgap.gp.impl.*;
import org.jgap.util.*;

/**
 * LesserThan on double values.
 *
 * @author Hakan Kjellerstrand
 *
 */
public class LesserThanOrEqualD
    extends MathCommand implements ICloneable {
    
    public LesserThanOrEqualD(final GPConfiguration a_conf)
        throws InvalidConfigurationException {
        this(a_conf, CommandGene.DoubleClass);
    }
    
    public LesserThanOrEqualD(final GPConfiguration a_conf, Class a_returnType)
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
            LesserThanOrEqualD result = new LesserThanOrEqualD(getGPConfiguration(), getReturnType());
            return result;
        } catch (Exception ex) {
            throw new CloneException(ex);
        }
    }
    
    public String toString() {
        return "&1 <= &2";
    }
    
    /**
     * @return textual name of this command
     *
     * @author Hakan Kjellerstrand
     * @since 3.2
     */
    public String getName() {
        return "LesserThanOrEqual";
    }
    
    public double execute_double(ProgramChromosome c, int n, Object[] args) {
        double d1 = c.execute_double(n, 0, args);
        double d2 = c.execute_double(n, 1, args);
        if (d1 <= d2) {
            return 1.0d;
        } else {
            return 0.0d;
        }
    }
}
