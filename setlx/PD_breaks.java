package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.SetlString;
import org.randoom.setlx.utilities.ParameterDef;

import java.util.List;

// breaks(string, pattern)     : SNOBOL string scanning function:  removes and returns all initial characters from string that is not in pattern. 

public class PD_breaks extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_breaks();

  private PD_breaks() {
    super("breaks");
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
    int lastIndex = -1;
    for(int i = 0; i < s.length(); i++) {
      char c = s.charAt(i);
      if (pattern.containsMember(new SetlString(String.valueOf(c))) == SetlBoolean.FALSE) {
        result += c;
        lastIndex = i;
      } else {
        break;
      }
    }
    
    // remove the found characters
    if (lastIndex > -1) {
      s = s.substring(lastIndex+1);
    }

    // write the reduced collection back into the outer environment
    writeBackVars.add(new SetlString(s));

    return new SetlString(result);

  }

}

