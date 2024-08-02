import java.util.*;

public class AnnotatedComments extends HashMap<String, Map<String, List<String>>> {
    private List<String> lastComments = new LinkedList<>();
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
    }

    public void parsedSection(String newSection) {
        parsedSection = newSection;
    }

    public void parsedAssignment(String newAssignment) {
        if (!lastComments.isEmpty()) {
            get(parsedSection).put(newAssignment, lastComments);
            lastComments = new LinkedList<>();
        }
    }
}
