package pl.AST;
import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;

public class ASTBoolean implements AST {
    final boolean value;

    public ASTBoolean(boolean value){
        this.value = value;
    }

    @Override
    public Type typeCheck(Accumulator<Type> accumulator) {
        return Type.BOOLEAN;
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) {
        return new BooleanRepresentation(this.value);
    }

    @Override
    public AST staticDistance(Accumulator<Integer> accumulator) {
        return this;
    }

    @Override
    public int countNumLetsInAST(int count) {
        return count;
    }

    @Override
    public IMeaning valueSD(IMeaning[] acc, int nextFreeSlot) {
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
