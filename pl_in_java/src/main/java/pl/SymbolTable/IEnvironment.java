package pl.SymbolTable;

// composite pattern
// an IEnvironment pairs Strings (names) with data of Type T in a linear order

// An IEnv is one of:
// -- MtEnv()
// -- ExEnv(String name, T data, IEnv rest)

public interface IEnvironment<T> {

    // create new IEnvironment from this and (name, data)
    IEnvironment<T> extend(String name, T data);

    // determine how deep 'name' is in this IEnv
    int lookupDepth(String name) throws Exception;

    // retrieve the `thing` associated with the first `name` in this IEnv
    T get(String name) throws Exception;

    // assigns `newElement` to the first `name` in this IEnv
    T update(String name, T newData) throws Exception;
}
