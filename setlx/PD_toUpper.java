package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.SetlString;

import java.util.List;

// toUpper(string)     : return a string in upper case

public class PD_toUpper extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_toUpper();

  private PD_toUpper() {
    super("toUpper");
    addParameter("string");
  }
  
  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException {
    final Value string  = args.get(0);
    if ( ! (string instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          string + "' is not a string.");
    }
    return new SetlString(string.getUnquotedString().toUpperCase());
  }
  
}

