package pl.Meaning;

public class EnvironmentUpdateDelayed implements IMeaning {
    public final String nameOfValueToUpdate;
    public final IMeaning newValue;

    public EnvironmentUpdateDelayed(String nameOfValueToUpdate, IMeaning newValue) {
        this.nameOfValueToUpdate = nameOfValueToUpdate;
        this.newValue = newValue;
    }

}
