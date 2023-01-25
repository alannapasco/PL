package pl.Meaning;

public class BooleanRepresentation implements IMeaning {
    public final boolean value;

    public BooleanRepresentation(boolean value){
        this.value = value;
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof BooleanRepresentation)) {
            return false;
        }
        BooleanRepresentation br = (BooleanRepresentation) o;
        return this.value == br.value;
    }

}
