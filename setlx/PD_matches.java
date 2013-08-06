package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.SetlString;

import java.util.List;

// matches(string, regex)     : return true if string matches the regular expression regex

public class PD_matches extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_matches();

  private PD_matches() {
    super("matches");
    addParameter("string");
    addParameter("regex");
  }

  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException {
    Value string  = args.get(0);
    Value regex  = args.get(1);
    if ( ! (string instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          string + "' is not a string.");
    }

    if ( ! (regex instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          regex + "' is not a string.");
    }
    
    if (string.getUnquotedString().matches(regex.getUnquotedString())) {
      return SetlBoolean.TRUE; 
    } else {
      return SetlBoolean.FALSE; 
    }
  }

}

