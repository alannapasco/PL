package pl.AST;

import pl.Meaning.IMeaning;
import pl.SymbolTable.IEnvironment;
import pl.TypePrediction.ArrowTypeGeneric;
import pl.TypePrediction.Type;

/**
 * Generic Fun Call
 */
public class ASTFunCall_T implements AST {
    AST funExpression;
    Type genericActual;

    public ASTFunCall_T(AST funExpression, Type genericActual){
        this.funExpression = funExpression;
        this.genericActual = genericActual;
    }


    @Override
    public Type typeCheck(IEnvironment<Type> env) throws Exception {
        //get the arrowTypeGeneric of the funExpression
        Type funExpressionType = this.funExpression.typeCheck(env);
        //ensure the funExpression accurately returns an arrowTypeGeneric
        if (funExpressionType instanceof ArrowTypeGeneric fnExType) {
            //returns ArrowType (converts from ArrowTypeGeneric to ArrowType)
            return fnExType.apply(this.genericActual);
        } else {
            throw new Exception("Type Error - func " + this.funExpression + " does not resolve to a generic arrow type");
        }
    }

    @Override
    public IMeaning value(IEnvironment<IMeaning> env) throws Exception {
        return this.funExpression.value(env);
    }

    @Override
    public String toString(){
        return "tfunCall: " + this.funExpression + "<" + genericActual + ">";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof ASTFunCall_T x)) {
            return false;
        }
        return this.funExpression.equals(x.funExpression) && this.genericActual.equals(x.genericActual);
    }
}
