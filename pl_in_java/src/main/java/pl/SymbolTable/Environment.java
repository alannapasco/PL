package pl.SymbolTable;

public class Environment<T> extends AEnvironment<T> {

    private final String name;
    private T data;
    private IEnvironment<T> rest;

    public Environment(String name, T data, IEnvironment<T> rest) {
        this.name = name;
        this.data = data;
        this.rest = rest;
    }

    int lookupDepth(String name, int count) throws Exception {
        if (this.name.equals(name)) {
            return count;
        }
        else {
            return lookupDepth(name, count+1);
        }
    }

    @Override
    public T get(String name) throws Exception {
        if (this.name.equals(name)) {
            return this.data;
        }
        else {
            return this.rest.get(name);
        }
    }

    @Override
    public T update(String name, T newData) throws Exception {
        if (this.name.equals(name)) {
            T temp = this.data;
            this.data = newData;
            return temp;
        }
        else {
            return this.rest.update(name, newData);
        }
    }
}