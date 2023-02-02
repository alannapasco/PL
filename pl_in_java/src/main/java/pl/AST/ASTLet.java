package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.TypeEntry;
import pl.SymbolTable.ValueEntry;
import pl.TypePrediction.Type;

import java.util.LinkedList;

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
    public Type typeCheck(LinkedList<TypeEntry> accumulator) throws Exception {
        //update the accumulator with a new Entry for the new variable (type)
        accumulator.add(new TypeEntry(this.varName, this.varType));

        //Then, perform typeCheck() on the SCOPE
        return this.scope.typeCheck(accumulator);
        //after performing typecheck, you could pop off the new entry to ensure it isn't available
        //outside its scope (or leave it to garbage)
    }

    @Override
    public IMeaning value(LinkedList<ValueEntry> accumulator) throws Exception {
        //update the accumulator with a new Entry for the new variable (the variable value, not the value of the whole let expression)
        accumulator.add(new ValueEntry(this.varName, this.varValue.value(accumulator)));

        //then, perform value() on the SCOPE
        return this.scope.value(accumulator);
        //after performing value, you could pop off the new entry to ensure it isn't available
        //outside its scope (or leave it to garbage)
    }

    @Override
    public String toString(){
        return "[let " + this.varType + " " + this.varName + " = " + this.varValue.toString() + " in " + this.scope.toString() + "]";
    }
}
