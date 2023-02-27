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
        IMeaning valueEvaluated = this.varValue.value(accumulator);
        return this.scope.value(new Accumulator<>(this.varName, valueEvaluated, accumulator));
    }

    @Override
    public String toString(){
        return "[let " + this.varType + " " + this.varName + " = " + this.varValue.toString() + " in " + this.scope.toString() + "]";
    }
}
