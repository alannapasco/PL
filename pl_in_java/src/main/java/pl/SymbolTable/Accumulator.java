package pl.SymbolTable;

public class Accumulator<T> {
    public final String name;

  // MF: `data` is implicitly initialized to `null`
  //      which data-represents the "empty accumulator" (cactus stack)
  //      and `get` exploits this below.
  //      (1) Remember "null was my billion dollar mistake" -- Hoare 1990s
  //          (with our current inflation it's probably closer to trilion)
  //      (2) This choice of data representation won't extend easily,
  //          meaning it imposes a serious social cost across time.

  // MF: at a min, the class should come with an isEmpty() method,
  //     which returns `true` is data == null and `false` otherwise.
  //     Then `null` is encapsulated in a single spot, two occurrences.

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
      // MF: here is the `null` check mention above. 
        if (this.data == null || this.rest == null) {
            throw new Exception("End of Table");
        }
        if (this.name.equals(name)) {
            return this.data;
        } else {
            return this.rest.get(name);
        }
    }

    public T update(String name, T newElement) throws Exception {
        if (this.data == null || this.rest == null) {
            throw new Exception("End of Table");
        }
        if (this.name.equals(name)) {
            T temp = this.data;
            this.data = newElement;
            return temp;
        } else {
            return this.rest.update(name, newElement);
        }
    }
}
