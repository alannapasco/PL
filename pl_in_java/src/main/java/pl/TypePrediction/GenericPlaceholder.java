package pl.TypePrediction;

public class GenericPlaceholder implements Type {
    public String placeholder;

    public GenericPlaceholder(String placeholder){
        this.placeholder = placeholder;
    }

    @Override
    public Type substitute(GenericPlaceholder placeholder, Type actual) {
        if (placeholder.equals(this)){
            return actual;
        } else {
            return this;
        }
    }

    @Override
    public String toString(){
        return "<" + placeholder + ">";
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof GenericPlaceholder x)) {
            return false;
        }
        return this.placeholder.equals(x.placeholder);
    }
}
