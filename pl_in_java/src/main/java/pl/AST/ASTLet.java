package pl.AST;

import pl.Meaning.EnvironmentUpdateDelayed;
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

        if (valueEvaluated instanceof EnvironmentUpdateDelayed asEUD) {
            //Effect: updates the node representing the variable that was updated with the new value
            accumulator.update(asEUD.nameOfValueToUpdate, asEUD.newValue);
            valueEvaluated = asEUD.newValue;
        }

        IMeaning result = this.scope.value(new Accumulator<>(this.varName, valueEvaluated, accumulator));

        if (result instanceof EnvironmentUpdateDelayed rasEUD) {
            return rasEUD.newValue;
        } else {
            return result;
        }
    }

    @Override
    public AST staticDistance(Accumulator<Integer> accumulator)  {
        AST valueEvaluated = this.varValue.staticDistance(accumulator);

        int depthInTree = 0;
        if (accumulator.data != null){
            depthInTree=accumulator.data+1;
        }
        AST scopeEvaluated = this.scope.staticDistance(new Accumulator<>(this.varName, depthInTree, accumulator));
        return new ASTLet(this.varType, this.varName, valueEvaluated, scopeEvaluated);
    }

    @Override
    public int countNumLetsInAST(int count) {
        return this.scope.countNumLetsInAST(count) + 1;
    }

    @Override
    public IMeaning valueSD(IMeaning[] acc, int nextFreeSlot) throws Exception {
        IMeaning valueEvaluated = this.varValue.valueSD(acc, nextFreeSlot);
        acc[nextFreeSlot] = valueEvaluated;
        return this.scope.valueSD(acc, nextFreeSlot-1);
    }

    @Override
    public String toString(){
        return "[let " + this.varType + " " + this.varName + " = " + this.varValue.toString() + " in " + this.scope.toString() + "]";
    }
}
