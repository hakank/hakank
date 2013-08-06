package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.exceptions.SetlException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.Om;
import org.randoom.setlx.types.SetlList;
import org.randoom.setlx.types.SetlString;

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;

// next_permutation(list) : returns the next permutation of the list 

public class PD_next_permutation extends PreDefinedFunction {
    public final static PreDefinedFunction DEFINITION = new PD_next_permutation();

    private PD_next_permutation() {
        super("next_permutation");
        addParameter("collectionValue");
    }

    public Value execute(List<Value> args, List<Value> writeBackVars) throws SetlException {

      if ( ! (args.get(0) instanceof SetlList) ) {
        throw new IncompatibleTypeException("Argument '" +
                                            args.get(0) + "' is not a list or string.");
      }
      

      SetlList collectionValue  = (SetlList)args.get(0);
      int len = collectionValue.size();
      ArrayList<Value> p = new ArrayList<Value>();
      for (Value v : collectionValue) {
        p.add(v);
      }
      
      // Inspired by permutation from 
      // http://code.google.com/p/algorithms-java/source/browse/trunk/src/main/java/com/google/code/Permutations.java?r=3
      int a = p.size() - 2;
      while (a >= 0 && p.get(a).compareTo(p.get(a + 1)) >= 0) {
        a--;
      }
      if (a == -1) {
        // return false;
        return Om.OM;
      }
      int b = p.size() - 1;
      while (p.get(b).compareTo(p.get(a)) <= 0) {
        b--;
      }
      Value t = p.get(a);
      p.set(a, p.get(b));
      p.set(b, t);
      for (int i = a + 1, j = p.size() - 1; i < j; i++, j--) {
        t = p.get(i);
        p.set(i,p.get(j));
        p.set(j,t);
      }
      
      SetlList result = new SetlList(p.size());
      for (Value v : p) {
        result.addMember(v);
      }
      
      return result;
      
    } 

}

