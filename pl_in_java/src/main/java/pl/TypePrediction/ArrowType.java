package pl.TypePrediction;

public class ArrowType implements Type {
    public final Type argType;
    public final Type retType;

    public ArrowType(Type argType, Type retType){
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
        if (!(o instanceof ArrowType x)) {
            return false;
        }
        return this.argType.equals(x.argType) && this.retType.equals(x.retType);
    }
}
