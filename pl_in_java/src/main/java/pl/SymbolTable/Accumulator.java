package pl.SymbolTable;

public class Accumulator<T> {
    public final String name;
    public T data;
    public Accumulator<T> rest;

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

    public void update(String name, T newElement) throws Exception {
        if (this.data == null || this.rest == null) {
            throw new Exception("End of Table");
        }
        if (this.name.equals(name)) {
            this.data = newElement;
        } else {
            this.rest.update(name, newElement);
        }
    }
}
