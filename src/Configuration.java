import jflex.base.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Configuration extends LinkedHashMap<String, Section> {

    private final Set<Section> visitedSections = new HashSet<>();
    private final Set<Pair<Section, String>> visitedAssignments = new HashSet<>();

    public enum resolveResult {
        OK,
        CYCLE,
        INVALID
    }

    public Configuration() {
        super();
    }

    public Configuration(Section section) {
        this();
        this.put(section.name, section);
    }

    private boolean dfsHasCycle(Section section) {
        if (visitedSections.contains(section)) {
            return true;
        }
        visitedSections.add(section);
        return section.inherits.stream().anyMatch(s -> dfsHasCycle(get(s)));
    }

    public Triple<resolveResult, Section, Either<?, Pair<Optional<String>, String>>> resolveReference(Section section, Either<?, Pair<Optional<String>, String>> rValue) {
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
            return resolveReference(get(ref.fst.get()), Assignment.dequalify(rValue));
        } else { // unqualified ref
            final String unqualifiedRef = ref.snd;
            if (visitedAssignments.contains(new Pair<>(section, unqualifiedRef))) {
                return new Triple<>(resolveResult.CYCLE, section, rValue);
            }
            if (section.assignments.containsKey(ref.snd)) {
                visitedAssignments.add(new Pair<>(section, unqualifiedRef));
                return resolveReference(section, section.assignments.get(unqualifiedRef).rValue);
            }
            for (String inherit : section.inherits) {
                Triple<resolveResult, Section, Either<?, Pair<Optional<String>, String>>> r = resolveReference(get(inherit), rValue);
                if (r.fst() == resolveResult.OK || r.fst() == resolveResult.CYCLE) {
                    visitedAssignments.add(new Pair<>(get(inherit), unqualifiedRef));
                    return r;
                }
            }
            return new Triple<>(resolveResult.INVALID, section, rValue);
        }
    }


    public Optional<?> getRvalueFromName(String sectionName, String lValue) {
        visitedAssignments.clear();
        Triple<resolveResult, Section, Either<?, Pair<Optional<String>, String>>> r =
                resolveReference(get(sectionName), get(sectionName).assignments.get(lValue).rValue);
        if (r.fst() == resolveResult.OK) {
            return Optional.of(r.trd().getLeft());
        }
        return Optional.empty();
    }


    public void removeSection(String sectionName) {

        Stack<Pair<Section, Assignment<?>>> deleteStack = new Stack<>();
        remove(sectionName);

        for (Section section : values()) {
            for (Assignment<?> assignment : section.assignments.values()) {
                visitedAssignments.clear(); // TODO TENERE O MENO
                System.out.println(section.name + assignment.rValue); // TODO TOGLIERE
                System.out.println(resolveReference(section, assignment.rValue).fst()); // TODO TOGLIERE

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
    }


    public void removeBinding(String sectionName, String lValue) {
        Assignment<?> removedAssignment = get(sectionName).assignments.remove(lValue);

        boolean anyPruned = false;
        Stack<Pair<Section, Assignment<?>>> deleteStack = new Stack<>();
        do {
            this.values().stream()
                    .forEach(section ->
                            section.assignments.values().stream()
                                    .filter(Assignment::isReference)
                                    .forEach(assignment -> {
                                        visitedAssignments.clear();
                                        if (resolveReference(section, assignment.rValue).fst() != resolveResult.OK) {
                                            deleteStack.push(new Pair<>(section, assignment));
                                        }
                                    }));
            while (!deleteStack.isEmpty()) {
                anyPruned = true;
                Pair<Section, Assignment<?>> element = deleteStack.pop();
                element.fst.assignments.remove(element.snd.lValue);
            }
        } while (anyPruned);
    }


    /*public void removeBinding(String sectionName, String lValue) {
        Assignment<?> removedAssignment = get(sectionName).assignments.remove(lValue);

        boolean anyPruned;
        Stack<Pair<Section, Assignment<?>>> deleteStack = new Stack<>();
        do {
            anyPruned = this.values().stream()
                    .anyMatch(section ->
                            section.assignments.values().stream()
                                    .filter(Assignment::isReference)
                                    .anyMatch(assignment -> {
                                        visitedAssignments.clear();
                                        if (resolveReference(section, assignment.rValue).fst() != resolveResult.OK) {
                                            deleteStack.push(new Pair<>(section, assignment));
                                            return true;
                                        }
                                        return false;
                                    }));
            while (!deleteStack.isEmpty()) {
                Pair<Section, Assignment<?>> element = deleteStack.pop();
                element.fst.assignments.remove(element.snd.lValue);
            }
        } while (anyPruned);
    }*/

    public boolean analyze() {
        //validate inherits
        for (Section section : values()) {
            for (String inherit : section.inherits) {
                if (!containsKey(inherit)) {
                    System.err.println("Invalid inherit to " + inherit);
                    return false;
                }
            }
        }

        // non-recursive inherits
        for (Section section : values()) {
            visitedSections.clear();
            if (dfsHasCycle(section)) {
                System.err.println("Cyclic inherit");
                return false;
            }
        }

        //validate references
        /*boolean allMatches = configuration.values().stream()
                .allMatch(section -> section.assignments.values().stream()
                        .filter(assignment -> assignment.rValue.isRight())
                        .map(assignment -> assignment.rValue.getRight())
                        .filter(optionalStringPair -> !optionalStringPair.fst.isPresent())
                        .map(optionalStringPair -> optionalStringPair.snd)
                        .allMatch(s -> checkReference(section, s)));
        if (!allMatches) {
            return false;
        }*/

        //TODO validate references
        final boolean validReferences = values().stream()
                .allMatch(section -> section.assignments.values().stream()
                        .filter(Assignment::isReference)
                        .allMatch(referenceAssignment -> {
                            visitedAssignments.clear();
                            resolveResult r = resolveReference(section, referenceAssignment.rValue).fst();
                            if (r == resolveResult.OK) {
                                return true;
                            } else {
                                System.err.println("Variable " + referenceAssignment.lValue + " resolve failed with " + r);
                                return false;
                            }
                        }));

        if (!validReferences) {
            return false;
        }

//        if (configuration.values().stream()
//                .anyMatch(section -> section.assignments.values().stream()
//                .filter(assignment -> assignment.rValue.isRight())
//                .map(assignment -> assignment.rValue.getRight())
//                .filter(optionalStringPair -> !optionalStringPair.fst.isPresent())
//                .map(optionalStringPair -> optionalStringPair.snd)
//                .anyMatch(s -> !section.assignments.containsKey(s)))) {
//            return false;
//        }

        return true;
    }

    public void prettyPrinter(AnnotatedComments annotatedComments) {
        if (annotatedComments.get(null).containsKey(null)) {
            for (String comment : annotatedComments.get(null).get(null)) {
                System.out.println(comment);
            }
        }
        for (Section section : values()) {
            System.out.println(System.lineSeparator() + "[" + section.name + "]");
            for (Assignment<?> assignment : section.assignments.values()) {
                for (String comment : annotatedComments.get(section).get(assignment)) {
                    System.out.println(comment);
                }
                String right = (assignment.rValue.isLeft()) ? assignment.rValue.getLeft().toString() : assignment.rValue.getRight().snd;
                System.out.println(assignment.lValue + " = " + right);
            }
            if (annotatedComments.get(section).containsKey(null)) {
                for (String comment : annotatedComments.get(section).get(null)) {
                    System.out.println(comment);
                }
            }
            System.out.println();
        }
    }


}//class
