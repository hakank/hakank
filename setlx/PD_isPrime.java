package org.randoom.setlx.functions;

import org.randoom.setlx.exceptions.IncompatibleTypeException;
import org.randoom.setlx.types.Value;
import org.randoom.setlx.types.Om;
import org.randoom.setlx.types.SetlBoolean;
import org.randoom.setlx.types.Rational;
import org.randoom.setlx.utilities.ParameterDef;

import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

// isPrime(n)     : returns true if n is a prime, false otherwise

public class PD_isPrime extends PreDefinedFunction {
  public final static PreDefinedFunction DEFINITION = new PD_isPrime();
  
  private PD_isPrime() {
    super("isPrime");
    addParameter("n");
  }
  
  public Value execute(final List<Value> args, final List<Value> writeBackVars) throws IncompatibleTypeException {
    if ( args.get(0).isInteger() == SetlBoolean.FALSE) {
      throw new IncompatibleTypeException("Argument '" +
                                          args.get(0) + "' is not an integer.");
    }

    // int version: this just checks for int's, i.e. up to 2**31-1.
    try {

      int n = ((Rational)args.get(0)).intValue();
      if (n <= 1) {
        return SetlBoolean.FALSE;
      }
      if (n == 2) {
        return SetlBoolean.TRUE;
      }
      if (n % 2 == 0) {
        return SetlBoolean.FALSE;
      }
      for (int i = 3; i <= Math.sqrt(n) + 1; i = i + 2) {
        if (n % i == 0) {
          return SetlBoolean.FALSE;
        }
      }
      return SetlBoolean.TRUE;
      
    } catch (Exception e) {

    }

    // If int version fails, try the BigInteger version.
    try {

      Rational n = (Rational)args.get(0);
      
      Rational zero = new Rational(0);
      Rational one  = new Rational(1);
      Rational two  = new Rational(2);
      
      if (n.compareTo(one) <= 0) {
        return SetlBoolean.FALSE;
      }
      if (n.compareTo(two) == 0) {
        return SetlBoolean.TRUE;
      }
      if (n.modulo(two).compareTo(zero) == 0) {
        return SetlBoolean.FALSE;
      }
      Value i = new Rational(3);
      while (i.multiply(i).compareTo(n) <= 0) {
        if (n.modulo(i).compareTo(zero) == 0) {
          return SetlBoolean.FALSE;
        }       
        i = i.sum(two);
      }
      
      return SetlBoolean.TRUE;

    } catch (Exception e) {
      
      System.out.println(e);
      return Om.OM;

    }


  }

}

