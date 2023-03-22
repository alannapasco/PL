package pl.AST;
import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.BooleanType;
import pl.TypePrediction.Type;

public class ASTBoolean implements AST {
    final boolean value;

    public ASTBoolean(boolean value){
        this.value = value;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) {
        return new BooleanType();
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) {
        return new BooleanRepresentation(this.value);
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
        if (!(o instanceof ASTBoolean x)) {
            return false;
        }
        return this.value == x.value;
    }

}
