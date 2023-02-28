package pl.SymbolTable;

abstract class AEnvironment<T> implements IEnvironment<T>{

    @Override
    public IEnvironment<T> extend(String name, T thing) {
        return new Environment<T>(name, thing, this);
    }

    @Override
    public int lookupDepth(String name) throws Exception {
        return lookupDepth(name,0);
    }

    abstract int lookupDepth(String var, int count) throws Exception;

}
