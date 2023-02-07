package pl.AST;

import pl.Meaning.BooleanRepresentation;
import pl.Meaning.IMeaning;
import pl.Meaning.IntegerRepresentation;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;

public class ASTInteger implements AST {
    private final int value;

    public ASTInteger(int value){
        this.value = value;
    }

    @Override
    public Type typeCheck(Accumulator<Type> accumulator) {
        return Type.INTEGER;
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) {
        return new IntegerRepresentation(this.value);
    }

    @Override
    public AST staticDistance(String[] acc, int tailIdx) {
        return this;
    }

    @Override
    public int countNumLets(int count) {
        return count;
    }

    @Override
    public IMeaning valueSD(IMeaning[] acc, int tailIdx) {
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
        if (!(o instanceof ASTInteger)) {
            return false;
        }
        ASTInteger x = (ASTInteger) o;
        return this.value == x.value;
    }
}
