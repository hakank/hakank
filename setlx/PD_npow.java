package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.Om;
import org.randoom.setlx.types.Rational;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.SetlSet;
import org.randoom.setlx.utilities.ParameterDef;

import java.lang.Integer;
import java.util.Iterator;
import java.util.List;

// npow(set, n)     : returns a set of all subsets of size n

public class PD_npow extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_npow();
  
  private PD_npow() {
    super("npow");
    addParameter("n");
    addParameter("set");
  }
  
  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException {

    if (args.get(0).isInteger() == SetlBoolean.FALSE) {
      throw new IncompatibleTypeException("Argument '" +
                                          args.get(0) + "' is not an integer.");

    }

    if ( ! (args.get(1) instanceof SetlSet)) {
      throw new IncompatibleTypeException("Argument '" +
                                          args.get(1) + "' is not a set.");
    }

    //
    // Simple variant: create the fulle power set and remove all subsets not of size n
    //
    try {
      int n = ((Rational)args.get(0)).intValue();
      SetlSet pow = args.get(1).powerSet();
      SetlSet npow = new SetlSet();
      for (final Value subSet : pow) {
        if (subSet.size() == n) {
          npow.addMember(subSet);
        }
      }
      
      return npow;

      
    } catch (Exception e) {

      return Om.OM;
    }

  }

}

