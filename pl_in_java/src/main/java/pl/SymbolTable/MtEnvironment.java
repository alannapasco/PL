package pl.SymbolTable;

public class MtEnvironment<T> extends AEnvironment<T> {

    int lookupDepth(String var, int count) throws Exception {
        throw new Exception("Reached end of environment and did not find record.");
    }

    @Override
    public T get(String var) throws Exception {
        throw new Exception("Reached end of environment and did not find record to return.");
    }

    @Override
    public T update(String name, T newData) throws Exception {
        throw new Exception("Reached end of environment and did not find record to update.");
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        return o instanceof MtEnvironment;
    }
}
