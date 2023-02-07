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
    public AST staticDistance(String[] acc, int tailIdx) {
        for (int i=0; i<acc.length; i++){
            if (acc[i] != null && acc[i].equals(this.name)){
                try {
                    ASTStaticDistanceNode ret = new ASTStaticDistanceNode(i);
                    acc[i] = null;
                    return ret;
                } catch (Exception e){
                    //i was < 0 (will never occur)
                    return new ASTError("No SD Found for " + this.name);
                }
            }
        }
        return new ASTError("No SD Found for " + this.name);
    }

    @Override
    public int countNumLets(int count) {
        return count;
    }

    @Override
    public IMeaning valueSD(IMeaning[] acc, int tailIdx) {
        return null;
    }

    @Override
    public String toString(){
        return "name:" + this.name;
    }
}
