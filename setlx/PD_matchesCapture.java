package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.Om;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.SetlString;
import org.randoom.setlx.types.SetlList;

import java.util.List;
import java.util.regex.*;

// matchesCapture(string, regex) : return SetlList with all the matched patterns in regex

public class PD_matchesCapture extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_matchesCapture();

  private PD_matchesCapture() {
    super("matchesCapture");
    addParameter("string");
    addParameter("regex");
  }

  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException {
    Value string  = args.get(0);
    Value regex  = args.get(1);
    if ( ! (string instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          string + "' is not a string.");
    }

    if ( ! (regex instanceof SetlString)) {
      throw new IncompatibleTypeException("Argument '" +
                                          regex + "' is not a string.");
    }

    try {

      Pattern pattern = Pattern.compile(regex.getUnquotedString());
      Matcher matcher = pattern.matcher(string.getUnquotedString());
      boolean found   = matcher.find();
      
      if (found) {
        int num_found = matcher.groupCount();
        SetlList result = new SetlList(num_found);
        for(int i = 1; i <= num_found; i++) {
          result.addMember(new SetlString((String)matcher.group(i)));
        }

        return result;
        
      } else {
        
        return new SetlList();
      }

    } catch(Exception e) {

      System.out.println(e);
      return Om.OM;

    }

  }

}


