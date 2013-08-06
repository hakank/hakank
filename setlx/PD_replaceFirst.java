package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.SetlString;

import java.util.List;

// replaceFirst(string, regex, replacement)     : return a string with first match of regex replaced with replacement

public class PD_replaceFirst extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_replaceFirst();

  private PD_replaceFirst() {
    super("replaceFirst");
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

    
    return new SetlString(string.getUnquotedString().replaceFirst(regex.getUnquotedString(),replacement.getUnquotedString()));
  }

}

