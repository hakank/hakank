package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.SetlString;
import org.randoom.setlx.types.Rational;
import org.randoom.setlx.utilities.ParameterDef;

import java.util.List;

// ltrim(string, n)     : SNOBOL string scanning function:  return the string with initial white space removed

public class PD_ltrim extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_ltrim();

  private PD_ltrim() {
    super("ltrim");
    addParameter("string");
  }

  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException {

    if ( ! (args.get(0) instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          args.get(0) + "' is not a string.");
    }

    return new SetlString(args.get(0).getUnquotedString().replaceFirst("^\\s+",""));

  }

}

