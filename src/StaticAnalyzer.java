import jflex.base.Pair;

import java.util.*;

public class StaticAnalyzer {
    private final Configuration configuration;
    private final Set<Section> visitedSections = new HashSet<>();
    private final Set<Pair<Section, String>> visitedAssignments = new HashSet<>();

    public enum resolveResult {
        OK,
        CYCLE,
        INVALID
    }

    public StaticAnalyzer(Configuration configuration) {
        this.configuration = configuration;
    }

    // incompatible with loops
    /*private boolean checkReference(Section section, String ref) {
        final boolean localMatches = section.assignments.values().stream()
                .anyMatch(assignment -> assignment.lValue.equals(ref));
        if (localMatches) {
            return true;
        }
        if (section.inherits.isEmpty()) {
            return false;
        }
        return section.inherits.stream().anyMatch(s -> checkReference(configuration.get(s), ref));
    }*/

    private boolean dfsHasCycle(Section section) {
        if (visitedSections.contains(section)) {
            return true;
        }
        visitedSections.add(section);
        return section.inherits.stream().anyMatch(s -> dfsHasCycle(configuration.get(s)));
    }

    public Triple<resolveResult, Section, Either<?, Pair<Optional<String>, String>>> resolveReference(Section section, Either<?, Pair<Optional<String>, String>> rValue) {
        if (rValue.isLeft()) {
            //visitedAssignments.add(new Pair<>(section,rValue.getRight()));
            return new Triple<>(resolveResult.OK, section, rValue);
        }
        final Pair<Optional<String>, String> ref = rValue.getRight();
        if (ref.fst.isPresent()) { // qualified ref
            if (!configuration.containsKey(ref.fst.get())) {
                // qualifier does not exist
                return new Triple<>(resolveResult.INVALID, section, rValue);
            }
            // not true recursion
            return resolveReference(configuration.get(ref.fst.get()), Assignment.dequalify(rValue));
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
                Triple<resolveResult, Section, Either<?, Pair<Optional<String>, String>>> r = resolveReference(configuration.get(inherit), rValue);
                if (r.fst() == resolveResult.OK || r.fst() == resolveResult.CYCLE) {
                    visitedAssignments.add(new Pair<>(configuration.get(inherit), unqualifiedRef));
                    return r;
                }
            }
            return new Triple<>(resolveResult.INVALID, section, rValue);
        }
    }

    public boolean analyze(Configuration configuration) {
        //validate inherits
        for (Section section : configuration.values()) {
            for (String inherit : section.inherits) {
                if (!configuration.containsKey(inherit)) {
                    System.err.println("Invalid inherit to " + inherit);
                    return false;
                }
            }
        }

        // non-recursive inherits
        for (Section section : configuration.values()) {
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
        final boolean validReferences = configuration.values().stream()
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
}
