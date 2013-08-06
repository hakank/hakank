package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.SetlString;
import org.randoom.setlx.types.Rational;
import org.randoom.setlx.utilities.ParameterDef;

import java.util.List;

// len(string, n)     : SNOBOL string scanning function:  removes and returns first n characters of string.

public class PD_len extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_len();

  private PD_len() {
    super("len");
    addParameter("string", ParameterDef.READ_WRITE);
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

    try {
      int n = ((Rational)args.get(1)).intValue();
      String s = args.get(0).getUnquotedString();
    
      String result = "";
      if (s.length() > n) {
        result = s.substring(0, n);
        s = s.substring(n);
      }
      
      // write the reduced collection back into the outer environment
      writeBackVars.add(new SetlString(s));
      return new SetlString(result);

    } catch (Exception e) {

    }
    
    writeBackVars.add(args.get(0));

    return new SetlString();

  }

}

