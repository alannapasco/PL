package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;


public class ASTName implements AST {
    String name;

    public ASTName(String name) {
        this.name = name;
    }

    @Override
    public Type typeCheck(Accumulator<Type> accumulator) throws Exception {
        try {
            return accumulator.get(this.name);
        } catch (Exception e) {
            throw new Exception("Type Error");
        }
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        try {
            return accumulator.get(this.name);
        } catch (Exception e) {
            throw new Exception("Invalid " + this.getClass().toString());
        }
    }

    @Override
    public AST staticDistance(Accumulator<Integer> accumulator) {
        try {
            int curDepthInTree = accumulator.data;
            int depthOfDeclaration = accumulator.get(this.name);
            int sd = curDepthInTree-depthOfDeclaration;
            return new ASTStaticDistanceNode(sd);
        } catch (Exception e) {
            return new ASTError("No SD Found for " + this.name);
        }
    }

    @Override
    public int countNumLetsInAST(int count) {
        return count;
    }

    @Override
    public IMeaning valueSD(IMeaning[] acc, int nextFreeSlot) {
      // MF : okay for here, throw an Exn in the "real world"; you want to know that something went wrong 
        return null;
    }

    @Override
    public String toString(){
        return "var:" + this.name;
    }
}
