package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.Accumulator;
import pl.TypePrediction.Type;


public class ASTLet implements AST{
    Type varType;
    String varName;
    AST varValue;
    AST scope;

    public ASTLet(Type varType, String varName, AST varValue, AST scope){
        this.varType = varType;
        this.varName = varName;
        this.varValue = varValue;
        this.scope = scope;
    }

    @Override
    public Type typeCheck(Accumulator<Type> accumulator) throws Exception {
        Type typeVerified = varValue.typeCheck(accumulator);
        if (!typeVerified.equals(varType)) {
            throw new Exception("Type Error");
        }
        return this.scope.typeCheck(new Accumulator<>(this.varName, this.varType, accumulator));
    }

    @Override
    public IMeaning value(Accumulator<IMeaning> accumulator) throws Exception {
        IMeaning valueOfVariable = this.varValue.value(accumulator);
        return this.scope.value(new Accumulator<>(this.varName, valueOfVariable, accumulator));
    }

    @Override
    public AST staticDistance(String[] acc, int tailIdx)  {
        acc[tailIdx] = this.varName;
        return new ASTLet(this.varType, this.varName, this.varValue.staticDistance(acc, tailIdx-1), this.scope.staticDistance(acc, tailIdx-1));
    }

    @Override
    public int countNumLets(int count) {
        return this.scope.countNumLets(count) + 1;
    }

    @Override
    public IMeaning valueSD(IMeaning[] acc, int tailIdx) throws Exception {
        tailIdx--;
        acc[tailIdx] = this.varValue.valueSD(acc, tailIdx);
        return this.scope.valueSD(acc, tailIdx);
    }

    @Override
    public String toString(){
        return "[let " + this.varType + " " + this.varName + " = " + this.varValue.toString() + " in " + this.scope.toString() + "]";
    }
}
