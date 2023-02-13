package pl.TypePrediction;

public class FunTypePair implements Type {
    public final Type argType;
    public final Type retType;

    public FunTypePair(Type argType, Type retType){
        this.argType = argType;
        this.retType = retType;
    }
}
