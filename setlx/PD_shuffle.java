package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.Om;
import org.randoom.setlx.types.SetlList;
import org.randoom.setlx.types.SetlString;
import org.randoom.setlx.utilities.ParameterDef;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;

// shuffle(collectionValue)     : returns a shuffled version of the collectionValue.

public class PD_shuffle extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_shuffle();
  
  private PD_shuffle() {
    super("shuffle");
    addParameter("collectionValue");
  }
  
  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException {
    if ( ! (args.get(0) instanceof SetlList || args.get(0) instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          args.get(0) + "' is not a list or string.");
    }

    if (args.get(0) instanceof SetlList) {

      // SetlList

      SetlList collectionValue  = (SetlList)args.get(0);
      
      int size = collectionValue.size();
      List<Value> s = new ArrayList<Value>(size);
      for(Value e: collectionValue) {
        s.add(e);
      }
      Collections.shuffle(s); // Java inline shuffle
      
      SetlList s2 = new SetlList(size);
      for(Value e: s) {
        s2.addMember(e);
      }
      
      return s2;

    } else {
      
      // SetlString

      String str = args.get(0).getUnquotedString();
      List shuffled = Arrays.asList(str.split(""));
      Collections.shuffle(shuffled);
      StringBuilder sb = new StringBuilder();
      for (int i = 0; i < shuffled.size(); i++) {
        sb.append(shuffled.get(i));
      }
     
      return new SetlString(new String(sb));
    }

  }

}

