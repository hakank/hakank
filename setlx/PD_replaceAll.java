package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.SetlString;

import java.util.List;

// replaceAll(string, regex, replacement) : return a string with regexp replaced with replacement

public class PD_replaceAll extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_replaceAll();

  private PD_replaceAll() {
    super("replaceAll");
    addParameter("string");
    addParameter("regex");
    addParameter("replacement");
  }

  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException {
    Value string  = args.get(0);
    Value regex  = args.get(1);
    Value replacement  = args.get(2);
    if ( ! (string instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          string + "' is not a string.");
    }

    if ( ! (regex instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          regex + "' is not a string.");
    }

    if ( ! (replacement instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          replacement + "' is not a string.");
    }

    
    return new SetlString(string.getUnquotedString().replaceAll(regex.getUnquotedString(),replacement.getUnquotedString()));
  }

}


