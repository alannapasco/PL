package pl.TypePrediction;

public class BooleanType extends AType {

    @Override
    public String toString(){
        return "BOOL";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        return o instanceof BooleanType;
    }
}
