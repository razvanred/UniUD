import jflex.base.Pair;

import java.util.*;
import java.util.stream.Stream;

public class Configuration extends LinkedHashMap<String, Section> {
    private final Set<Section> visitedSections = new HashSet<>();
    private final Set<Section> pathStack = new HashSet<>();
    private final Set<Pair<Section, String>> visitedAssignments = new HashSet<>();

    public enum resolveResult {
        OK,
        CYCLE,
        INVALID
    }

    public Configuration(Section section) {
        super();
        this.put(section.name, section);
    }

    private boolean dfsHasCycle(Section section) {
        visitedSections.clear();
        pathStack.clear();
        return dfsHasCycleHelper(section);
    }

    private boolean dfsHasCycleHelper(Section section) {
        if (pathStack.contains(section)) {
            return true;
        }
        if (visitedSections.contains(section)) {
            return false;
        }

        visitedSections.add(section);
        pathStack.add(section);


        final boolean anyLoop = section.inherits.stream().anyMatch(s -> dfsHasCycleHelper(get(s)));
        pathStack.remove(section);
        return anyLoop;
    }

    private void prune() {
        Stack<Pair<Section, Assignment<?>>> deleteStack = new Stack<>();
        final Set<String> box = new HashSet<>(1);
        do {
            box.remove("hasPruned");
            this.values().forEach(section -> section.assignments.values()
                    .removeIf(assignment -> {
                        if (assignment.isReference() && resolveReference(section, assignment.rValue).fst() != resolveResult.OK) {
                            box.add("hasPruned");
                            return true;
                        }
                        return false;
                    }));
        } while (box.contains("hasPruned"));
    }

    private Triple<resolveResult, Section, Either<?, Pair<Optional<String>, String>>> resolveReference(Section section, Either<?, Pair<Optional<String>, String>> rValue) {
        visitedAssignments.clear();
        return resolveReferenceHelper(section, rValue);
    }

    private Triple<resolveResult, Section, Either<?, Pair<Optional<String>, String>>> resolveReferenceHelper(Section section, Either<?, Pair<Optional<String>, String>> rValue) {
        if (rValue.isLeft()) {
            return new Triple<>(resolveResult.OK, section, rValue);
        }
        final Pair<Optional<String>, String> ref = rValue.getRight();
        if (ref.fst.isPresent()) { // qualified ref
            if (!this.containsKey(ref.fst.get())) {
                // qualifier does not exist
                return new Triple<>(resolveResult.INVALID, section, rValue);
            }
            // not true recursion
            return resolveReferenceHelper(get(ref.fst.get()), Assignment.dequalify(rValue));
        } else { // unqualified ref
            final String unqualifiedRef = ref.snd;
            if (visitedAssignments.contains(new Pair<>(section, unqualifiedRef))) {
                return new Triple<>(resolveResult.CYCLE, section, rValue);
            }
            if (section.assignments.containsKey(ref.snd)) {
                visitedAssignments.add(new Pair<>(section, unqualifiedRef));
                return resolveReferenceHelper(section, section.assignments.get(unqualifiedRef).rValue);
            }
            for (String inherit : section.inherits) {
                Triple<resolveResult, Section, Either<?, Pair<Optional<String>, String>>> r = resolveReferenceHelper(get(inherit), rValue);
                if (r.fst() == resolveResult.OK || r.fst() == resolveResult.CYCLE) {
                    visitedAssignments.add(new Pair<>(get(inherit), unqualifiedRef));
                    return r;
                }
            }
            return new Triple<>(resolveResult.INVALID, section, rValue);
        }
    }

    public Optional<?> getRValueFromName(String sectionName, String lValue) {
        Triple<resolveResult, Section, Either<?, Pair<Optional<String>, String>>> r =
                resolveReference(get(sectionName), get(sectionName).assignments.get(lValue).rValue);
        if (r.fst() == resolveResult.OK) {
            return Optional.of(r.trd().getLeft());
        }
        return Optional.empty();
    }

    public void removeSection(String sectionName) {
        remove(sectionName);
        values().forEach(section -> section.inherits.remove(sectionName));
        prune();
        values().removeIf(section -> section.assignments.isEmpty());
    }

    @Deprecated
    public void removeSectionOld(String sectionName) {
        Stack<Pair<Section, Assignment<?>>> deleteStack = new Stack<>();
        remove(sectionName);

        for (Section section : values()) {
            for (Assignment<?> assignment : section.assignments.values()) {
                if (resolveReference(section, assignment.rValue).fst() != resolveResult.OK) {
                    deleteStack.push(new Pair<>(section, assignment));
                }
            }
        }
        while (!deleteStack.isEmpty()) {
            Pair<Section, Assignment<?>> element = deleteStack.pop();
            element.fst.assignments.remove(element.snd.lValue);
        }
        for (Section section : values()) {
            section.inherits.remove(sectionName);
        }
        // remove empty sections
        values().removeIf(section -> section.assignments.isEmpty());
    }


    public void removeBinding(String sectionName, String lValue) {
        Assignment<?> removedAssignment = get(sectionName).assignments.remove(lValue);
        prune();
    }

    public boolean validate() {
        //validate inherits
        for (Section section : values()) {
            for (String inherit : section.inherits) {
                if (!containsKey(inherit)) {
                    System.err.println("Invalid inherit to " + inherit);
                    return false;
                }
            }
        }

        // acyclic inherits
        for (Section section : values()) {
            if (dfsHasCycle(section)) {
                System.err.println("Cyclic inherit");
                return false;
            }
        }

        // validate references, acyclic references
        final boolean validReferences = values().stream()
                .allMatch(section -> section.assignments.values().stream()
                        .filter(Assignment::isReference)
                        .allMatch(referenceAssignment -> {
                            resolveResult r = resolveReference(section, referenceAssignment.rValue).fst();
                            if (r == resolveResult.OK) {
                                return true;
                            } else {
                                System.err.println(section.name + "." + referenceAssignment.lValue + " resolve failed with " + r);
                                return false;
                            }
                        }));
        if (!validReferences) {
            return false;
        }

        return true;
    }

    public void prettyPrint(AnnotatedComments annotatedComments) {
        if (annotatedComments.get(null).containsKey(null)) {
            annotatedComments.get(null).get(null).forEach(System.out::print);
        }
        System.out.println();
        for (Section section : values()) {
            System.out.println("[" + section.name + "]");
            final Map<String, List<String>> sectionComments = annotatedComments.get(section.name);

            section.inherits.stream().map(sectioName -> "inherit " + sectioName).forEach(System.out::println);
            for (Assignment<?> assignment : section.assignments.values()) {
                if (sectionComments.containsKey(assignment.lValue)) {
                    sectionComments.get(assignment.lValue).forEach(System.out::print);
                }
                System.out.println(assignment);
            }
            if (sectionComments.containsKey(null)) {
                sectionComments.get(null).forEach(System.out::print);
            }
            System.out.println();
        }
    }




}//class
