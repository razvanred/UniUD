import java.util.*;

public class AnnotatedComments extends HashMap<String, Map<Assignment<?>, List<String>>> {
    private List<String> lastComments = new LinkedList<>();
    //    private Assignment<?> lastAssignment = null;
    private String lexedSection = null;
    private String parsedSection = null;

    public AnnotatedComments() {
        put(null, new HashMap<>());
    }

    public void push(String comment) {
        lastComments.add(comment);
    }

    public void lexedSection(String newSection) {
        // flush last comments immediately after [section] lexem
        if (!lastComments.isEmpty()) {
            parsedAssignment(null);
        }
        put(newSection, new HashMap<>());
        lexedSection = newSection;
    }

    public void parsedSection(String newSection) {
        parsedSection = newSection;
    }

    public void parsedAssignment(Assignment<?> assignment) {
        if (!lastComments.isEmpty()) {
            get(parsedSection).put(assignment, lastComments);
            lastComments = new LinkedList<>();
        }
    }
}
