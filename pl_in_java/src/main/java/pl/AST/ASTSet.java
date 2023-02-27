package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;

public class ASTSet implements AST {
    String varName;
    AST newVal;

    public ASTSet(String varName, AST newVal){
        this.varName=varName;
        this.newVal=newVal;
    }

    @Override
    public Type typeCheck(Accumulator<Type> accumulator) throws Exception {
        Type existingVarType = accumulator.get(this.varName);
        Type newValType = this.newVal.typeCheck(accumulator);
        if (!existingVarType.equals(newValType)){
            throw new Exception("Type Error");
        } else {
            return existingVarType;
        }
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        return accumulator.update(this.varName, this.newVal.value(accumulator));
    }

    @Override
    public String toString(){
        return "[set " + this.varName + " to " + this.newVal + "]";
    }
}
