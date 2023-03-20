package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.Type;

public class ASTFunTCall implements AST {
    AST funName;
    Type ttype;

    public ASTFunTCall(AST funName, Type ttype){
        this.funName = funName;
        this.ttype = ttype;
    }


    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        return null;
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        return null;
    }

    @Override
    public String toString(){
        return "tfunCall: " + this.funName + "<" + ttype + ">";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof ASTFunTCall x)) {
            return false;
        }
        return this.funName.equals(x.funName) && this.ttype.equals(x.ttype);
    }
}
