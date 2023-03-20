package pl.TypePrediction;

public class ForAll implements Type {
    TypeName T;
    Type typeForAllT;

    public ForAll(TypeName T, Type typeForAllT) {
        this.T = T;
        this.typeForAllT = typeForAllT;
    }
}
