package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.SetlString;
import org.randoom.setlx.utilities.ParameterDef;

import java.util.List;

// matchs(string, pattern)     : SNOBOL scanning function : Removes pattern from string and returns pattern of string starts with pattern.

public class PD_matchs extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_matchs();

  private PD_matchs() {
    super("matchs");
    addParameter("string", ParameterDef.READ_WRITE);
    addParameter("pattern");
  }
  
  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException {
    Value string  = args.get(0);
    Value pattern  = args.get(1);
    if ( ! (string instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          string + "' is not a string.");
    }

    if ( ! (pattern instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          pattern + "' is not a string.");
    }

    String s = string.getUnquotedString();
    String p = pattern.getUnquotedString();
    
    if (s.startsWith(p)) {
      s = s.substring(p.length());
    } else {
      p = "";
    }

    writeBackVars.add(new SetlString(s));

    return new SetlString(p);

  }

}

