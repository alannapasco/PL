package pl.TypePrediction;

public class ArrowTypeGeneric implements Type {
    GenericPlaceholder genericPlaceholder;
    Type argType;
    Type returnType;

    public ArrowTypeGeneric(GenericPlaceholder genericPlaceholder, Type argType, Type returnType) {
        this.genericPlaceholder = genericPlaceholder;
        this.argType = argType;
        this.returnType = returnType;
    }

    public Type apply(Type genericActual){
        Type groundedArgType = this.argType.substitute(this.genericPlaceholder, genericActual);
        Type groundedRetType = this.returnType.substitute(this.genericPlaceholder, genericActual);
        return new ArrowType(groundedArgType, groundedRetType);
    }

    @Override
    public Type substitute(GenericPlaceholder placeholder, Type actual) {
        //change here IF you allow same names for different generics
        Type groundedArgType = this.argType.substitute(this.genericPlaceholder, actual);
        Type groundedRetType = this.returnType.substitute(this.genericPlaceholder, actual);
        return new ArrowTypeGeneric(this.genericPlaceholder, groundedArgType, groundedRetType);
    }

    @Override
    public String toString(){
        return "[FA" + this.genericPlaceholder + " " + this.argType + " " + this.returnType + "]";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof ArrowTypeGeneric x)) {
            return false;
        }
        return this.argType.equals(x.argType.substitute(x.genericPlaceholder, this.genericPlaceholder))
                && this.returnType.equals(x.returnType.substitute(x.genericPlaceholder, this.genericPlaceholder));
    }

}
