package pl.Meaning;
import pl.AST.*;
import pl.SymbolTable.*;

//akaFunctionEvaluationDelayed
public class Closure implements IMeaning {
    public final String funName;
    private final AST funBody;
    private final String parameterName;
    private final IEnvironment<IMeaning> environment;

    //For testing purposes only
    public Closure(){
        this.funName = null;
        this.funBody = null;
        this.parameterName = null;
        this.environment = null;
    }

    // "introduction" / "create"
    public Closure(String funName, AST funBody, String parameter, IEnvironment<IMeaning> environment) {
        this.funName = funName;
        this.funBody = funBody;
        this.parameterName = parameter;
        this.environment = environment;
    }

    public IMeaning execute(IMeaning argumentEvaluated) throws Exception {
        return this.funBody.value(new Environment<>(this.parameterName, argumentEvaluated, this.environment));
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof Closure c)) {
            return false;
        }
        //for testing purposes only
        if (this.funBody==null || c.funBody ==null) {
            return true;
        }
        return this.funBody.equals(c.funBody)
                && this.parameterName.equals(c.parameterName)
                && this.environment.equals(c.environment);
    }

    @Override
    public String toString(){
        return "Closure Body: " + funBody;
    }
}
