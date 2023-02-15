package pl.Meaning;
import pl.AST.*;
import pl.SymbolTable.*;

public class Closure_akaFunctionEvaluationDelayed implements IMeaning {
    public final AST funBody;
    public final String parameterName;
    public final Accumulator<IMeaning> environment;

    public Closure_akaFunctionEvaluationDelayed(AST funBody, String parameter, Accumulator<IMeaning> environment) {
        this.funBody = funBody;
        this.parameterName = parameter;
        this.environment = environment;
    }
}
