package pl.AST;

import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.IntegerType;
import pl.TypePrediction.Type;

public class ASTInteger implements AST {
    private final int value;

    public ASTInteger(int value){
        this.value = value;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) {
        return new IntegerType();
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) {
        return new IntegerRepresentation(this.value);
    }

    @Override
    public String toString(){
        return String.valueOf(this.value);
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof ASTInteger x)) {
            return false;
        }
        return this.value == x.value;
    }
}
