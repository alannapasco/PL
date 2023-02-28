package pl.AST;
import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.Type;
import pl.TypePrediction.VarType;

public class ASTBoolean implements AST {
    final boolean value;

    public ASTBoolean(boolean value){
        this.value = value;
    }

    @Override
    public Type typeCheck(IEnvironment<Type> env) {
        return VarType.BOOLEAN;
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
        if (!(o instanceof ASTBoolean)) {
            return false;
        }
        ASTBoolean x = (ASTBoolean) o;
        return this.value == x.value;
    }

}
