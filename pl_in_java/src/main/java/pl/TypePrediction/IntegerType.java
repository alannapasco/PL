package pl.TypePrediction;

public class IntegerType extends AType {
    @Override
    public String toString(){
        return "INT";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        return o instanceof IntegerType;
    }
}
