package pl.TypePrediction;

public class AType implements Type {
    @Override
    public Type substitute(GenericPlaceholder placeholder, Type actual) {
        return this;
    }
}
