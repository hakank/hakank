package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.exceptions.SetlException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.SetlString;
import org.randoom.setlx.types.SetlList;
import org.randoom.setlx.utilities.ParameterDef;

import java.util.List;

// join(list, sep)     : returns a string with all the elements in list separated by sep.

public class PD_join extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_join();

  private PD_join() {
    super("join");
    addParameter("list");
    addParameter("sep");
  }

  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException, SetlException {

    if ( ! (args.get(0) instanceof SetlList)) {
      throw new IncompatibleTypeException("Argument '" +
                                          args.get(0) + "' is not a list.");
    }

    if ( ! (args.get(1) instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          args.get(1) + "' is not a string.");
    }

    SetlList list = (SetlList) args.get(0).clone();
    String sep    = args.get(1).getUnquotedString();

    String result = list.firstMember().getUnquotedString();
    list.removeFirstMember();
    for (Value e : list) {
      result += sep + e.getUnquotedString();
    }
    
    return new SetlString(result);


  }

}

