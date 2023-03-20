package pl.TypePrediction;

public class TypeName implements Type {
    public String t;

    public TypeName(String t){
        this.t = t;
    }

    @Override
    public String toString(){
        return "<" + t + ">";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof TypeName x)) {
            return false;
        }
        return this.t.equals(x.t);
    }
}
