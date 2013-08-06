package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.SetlString;
import org.randoom.setlx.types.Rational;
import org.randoom.setlx.utilities.ParameterDef;

import java.util.List;

// rpad(string, n)     : SNOBOL string scanning function:  return the string padded to right with " " to be length n.

public class PD_rpad extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_rpad();

  private PD_rpad() {
    super("rpad");
    addParameter("string");
    addParameter("n");
  }

  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException {

    if ( ! (args.get(0) instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          args.get(0) + "' is not a string.");
    }

    if ( args.get(1).isInteger() == SetlBoolean.FALSE) {
      throw new IncompatibleTypeException("Argument '" +
                                          args.get(1) + "' is not an integer.");
    }

    String s = args.get(0).getUnquotedString();
    try {

      int n = ((Rational)args.get(1)).intValue();    
      if ( n > 0 && s.length() < n) {
        s = String.format("%1$-" + n + "s", s);
      }

    } catch (Exception e) {

    }
    
    return new SetlString(s);

  }

}

