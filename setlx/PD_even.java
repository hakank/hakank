package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.exceptions.SetlException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.Om;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.Rational;
import org.randoom.setlx.utilities.ParameterDef;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

// even(n)     : returns true if n is even

public class PD_even extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_even();
  
  private PD_even() {
    super("even");
    addParameter("n");
  }
  
  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException, SetlException {
    if ( args.get(0).isInteger() == SetlBoolean.FALSE) {
      throw new IncompatibleTypeException("Argument '" +
                                          args.get(0) + "' is not an integer.");
    }

    // Return n % 2 == 0
    Value mod = args.get(0).modulo(new Rational(2));
    return mod.isEqual(new Rational(0));

  }

}

