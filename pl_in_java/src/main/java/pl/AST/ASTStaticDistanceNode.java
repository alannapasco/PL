package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;

public class ASTStaticDistanceNode implements AST {
    public final int staticDistance;

    public ASTStaticDistanceNode(int staticDistance) throws Exception {
        if (staticDistance<0){
            throw new Exception("Must be a natural number");
        }
        this.staticDistance = staticDistance;
    }

    @Override
    public Type typeCheck(Accumulator<Type> accumulator) throws Exception {
        //call typecheck on the parsed tree before turning it into a SD tree?
        throw new Exception("This method is not allowed on SD ASTs");
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        throw new Exception("This method is not allowed on SD ASTs");
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
        return acc[this.staticDistance];
    }

    @Override
    public String toString(){
        return "SD:" + this.staticDistance;
    }
}
