package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.SetlString;

import java.util.List;

// trim(string)     : return a trimmed string 

public class PD_trim extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_trim();
  
  private PD_trim() {
    super("trim");
    addParameter("string");
  }
  
  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException {
    Value string  = args.get(0);
    if ( ! (string instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          string + "' is not a string.");
    }
    return new SetlString(string.getUnquotedString().trim());
  }

}

