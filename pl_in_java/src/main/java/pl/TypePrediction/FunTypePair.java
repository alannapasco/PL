package pl.TypePrediction;

public class FunTypePair implements Type {
    public final Type argType;
    public final Type retType;

    public FunTypePair(Type argType, Type retType){
        this.argType = argType;
        this.retType = retType;
    }

    @Override
    public String toString(){
        return "[FunTypePair: " + argType + ", " + retType + "]";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof FunTypePair x)) {
            return false;
        }
        return this.argType.equals(x.argType) && this.retType.equals(x.retType);
    }
}
