package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.TypeEntry;
import pl.SymbolTable.ValueEntry;
import pl.TypePrediction.Type;

import java.util.LinkedList;

public class ASTError implements AST {
    final String message;

    public ASTError(String message){
        this.message = message;
    }

    @Override
    public Type typeCheck(LinkedList<TypeEntry> accumulator) throws Exception {
        throw new Exception("Type Error");
    }

    @Override
    public IMeaning value(LinkedList<ValueEntry> accumulator) throws Exception {
        throw new Exception("Error: " + this.message);
    }

    @Override
    public String toString(){
        return "***" + this.message;
    }
}
