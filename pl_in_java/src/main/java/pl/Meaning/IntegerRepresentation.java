package pl.Meaning;

public class IntegerRepresentation implements IMeaning {
    public final int value;

    public IntegerRepresentation(int value){
        this.value = value;
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof IntegerRepresentation)) {
            return false;
        }
        IntegerRepresentation ir = (IntegerRepresentation) o;
        return this.value == ir.value;
    }
}
