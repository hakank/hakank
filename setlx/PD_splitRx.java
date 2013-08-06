package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.SetlString;
import org.randoom.setlx.types.SetlList;

import java.util.List;

// splitRx(string, regex)     : splits a string around regex

public class PD_splitRx extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_splitRx();

  private PD_splitRx() {
    super("splitRx");
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

    //
    // Most of this has been borrowed from strSplit in SetlString.java
    //
    String[] strings = string.getUnquotedString().split(regex.getUnquotedString());
    final SetlList result  = new SetlList(strings.length);
    for (final String str : strings) {
      result.addMember(new SetlString(str));
    }

    final String    s       = string.getUnquotedString();
    final String    p       = regex.getUnquotedString();

    // fix split("foo", "") => ["", "f", "o", "o"], should be ["", "f", "o", "o"]
    if (strings.length >= 1 && strings[0].equals("") && p.equals("")) {
      result.removeFirstMember();
    }
    // fix split(";", ";") => [], should be ["", ""]
    else if (s.equals(p)) {
      result.addMember(new SetlString());
      result.addMember(new SetlString());
    }
    // fix split(";f;o;o;", ";") => ["", "f", "o", "o"], should be ["", "f", "o", "o", ""]
    else if (s.endsWith(p)) {
      result.addMember(new SetlString());
    }

    return result;

  }

}

