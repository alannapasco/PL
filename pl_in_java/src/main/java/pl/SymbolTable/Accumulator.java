package pl.SymbolTable;

public class Accumulator<T> {
    public final String name;
    public final T data;
    public final Accumulator<T> rest;

    public Accumulator(String name, T data, Accumulator<T> rest){
        this.name = name;
        this.data = data;
        this.rest = rest;
    }

    public Accumulator(){
        this.name = "";
        this.data = null;
        this.rest = null;
    }

    public T get(String name) throws Exception {
        if (this.data == null || this.rest == null) {
            throw new Exception("End of Table");
        }
        if (this.name.equals(name)) {
            return this.data;
        } else {
            return this.rest.get(name);
        }
    }
}