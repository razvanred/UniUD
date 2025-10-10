import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class Section {
    public final List<String> inherits;
    public final Map<String, Assignment<?>> assignments;
    public final String name;

    public Section(String name, List<String> inherits, Map<String, Assignment<?>> assignments){
        this.inherits = inherits;
        this.name = name;
        this.assignments = assignments;
    }

    public Section(String name, Map<String, Assignment<?>>  assignments){
        this.inherits = Collections.emptyList();
        this.name = name;
        this.assignments = assignments;
    }

    @Override
    public final boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Section)) return false;

        Section section = (Section) o;
        return Objects.equals(name, section.name);
    }
}
