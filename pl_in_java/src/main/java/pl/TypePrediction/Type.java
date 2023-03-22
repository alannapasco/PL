package pl.TypePrediction;

public interface Type {

    /**
     * substitute this Type with the given Type
     */
    Type substitute(GenericPlaceholder placeholder, Type actual);
}
