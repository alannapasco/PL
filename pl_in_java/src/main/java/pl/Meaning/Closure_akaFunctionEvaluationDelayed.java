package pl.Meaning;
import pl.AST.*;
import pl.SymbolTable.*;

public class Closure_akaFunctionEvaluationDelayed implements IMeaning {
    private final AST funBody;
    private final String parameterName;
    private final Accumulator<IMeaning> environment;

    // "introduction" / "create"
    public Closure_akaFunctionEvaluationDelayed(AST funBody, String parameter, Accumulator<IMeaning> environment) {
        this.funBody = funBody;
        this.parameterName = parameter;
        this.environment = environment;
    }

    /**
     * "Elimination" / "use"
     * evaluates the function
     * @param argumentEvaluated
     * @return
     * @throws Exception
     */
    public IMeaning apply(IMeaning argumentEvaluated) throws Exception {
        return this.funBody.value(new Accumulator<>(this.parameterName, argumentEvaluated, this.environment));
    }
}
