package pl.Meaning;
import pl.AST.*;
import pl.SymbolTable.*;

public class Closure_akaFunctionEvaluationDelayed implements IMeaning {
    private final AST funBody;
    private final String parameterName;
    private final IEnvironment<IMeaning> environment;

    public Closure_akaFunctionEvaluationDelayed(){
        this.funBody = null;
        this.parameterName = null;
        this.environment = null;
    }

    // "introduction" / "create"
    public Closure_akaFunctionEvaluationDelayed(AST funBody, String parameter, IEnvironment<IMeaning> environment) {
        this.funBody = funBody;
        this.parameterName = parameter;
        this.environment = environment;
    }

    public IMeaning execute(IMeaning argumentEvaluated) throws Exception {
        return this.funBody.value(new Environment<>(this.parameterName, argumentEvaluated, this.environment));
    }

    @Override
    public String toString(){
        return "Closure Body: " + funBody;
    }
}
