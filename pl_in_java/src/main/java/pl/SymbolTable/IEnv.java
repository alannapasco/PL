package pl.SymbolTable;

// composite pattern 

// an environment pairs Strings with 'things' of Type T in a linear order

// An IEnv is one of:
// -- MtEnv()
// -- ExEnv(String var, T thing, IEnv old)

interface IEnv<T> {

  // create new IEnv from this and (var, thing)
  IEnv extend(String var, T thing); 

  // determine how deep the `var` is in this `Env`
  int lookupDepth(String var) throws Exception;

  // retrieve the `thing` assoicated with the first `var` in this 
  T lookup(String var) throws Exception;
}

abstract class AEnv<T> implements IEnv<T> {
  public IEnv extend(String var, T thing) {
    return new ExEnv<T>(var, thing, this);
  }

  public int lookupDepth(String var) throws Exception {
    return lookupDepth(var,0);
  }

  abstract int lookupDepth(String var, int count) throws Exception; 
}

// -----------------------------------------------------------------------------
class MtEnv<T> extends AEnv<T> {

  int lookupDepth(String var, int count) throws Exception {
    throw new Exception("can't happen");
  }

  public T lookup(String var) throws Exception {
    throw new Exception("can't happen");
  }
}

// -----------------------------------------------------------------------------
class ExEnv<T> extends AEnv<T> {

  private String var;
  private T thing;
  private IEnv old; 

  ExEnv(String var, T thing, IEnv old) {
    this.var   = var;
    this.thing = thing;
    this.old   = old; 
  }

  int lookupDepth(String var, int count) throws Exception {
    if (this.var.equals(var)) {
      return count;
    }
    else {
      return lookupDepth(var, count+1);
    }
  }

  public T lookup(String var) throws Exception {
    if (this.var.equals(var)) {
      return thing;
    }
    else {
      return lookup(var);
    }
  }
}
