package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.SetlString;

import java.util.List;

// startsWith(string, prefix)     : return TRUE if string starts with prefix

public class PD_startsWith extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_startsWith();

  private PD_startsWith() {
    super("startsWith");
    addParameter("string");
    addParameter("prefix");
  }
  
  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException {
    Value string  = args.get(0);
    Value prefix  = args.get(1);
    if ( ! (string instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          string + "' is not a string.");
    }

    if ( ! (prefix instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          prefix + "' is not a string.");
    }

   
    if (string.getUnquotedString().startsWith(prefix.getUnquotedString())) {
      return SetlBoolean.TRUE;
    } else {
      return SetlBoolean.FALSE;
    }
  }

}

