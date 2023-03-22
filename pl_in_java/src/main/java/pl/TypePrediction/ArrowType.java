package pl.TypePrediction;

public class ArrowType implements Type {
    public Type argType;
    public Type retType;

    public ArrowType(Type argType, Type retType){
        this.argType = argType;
        this.retType = retType;
    }

    @Override
    public Type substitute(GenericPlaceholder placeholder, Type actual) {
        Type groundedArgType = this.argType.substitute(placeholder,actual);
        Type groundedRetType =  this.retType.substitute(placeholder, actual);
        return new ArrowType(groundedArgType, groundedRetType);
    }

    @Override
    public String toString(){
        return "[" + argType + "->" + retType + "]";
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
