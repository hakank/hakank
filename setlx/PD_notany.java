package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.SetlString;
import org.randoom.setlx.utilities.ParameterDef;

import java.util.List;

// notany(string, pattern)     : SNOBOL string scanning function:  removes and returns first character in string if it is not in pattern. 

public class PD_notany extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_notany();

  private PD_notany() {
    super("notany");
    addParameter("string", ParameterDef.READ_WRITE);
    addParameter("pattern");
  }

  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException {

    if ( ! (args.get(0) instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          args.get(0) + "' is not a string.");
    }

    if ( ! (args.get(1) instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          args.get(1) + "' is not a string.");
    }

    Value string  = args.get(0);
    Value pattern = args.get(1);

    String s = string.getUnquotedString();

    String result = "";
    char c = s.charAt(0);
    if (pattern.containsMember(new SetlString(String.valueOf(c))) == SetlBoolean.FALSE) {
      result += c;
      s = s.substring(1);
    }
    
    // write the reduced collection back into the outer environment
    writeBackVars.add(new SetlString(s));

    return new SetlString(result);

  }

}

