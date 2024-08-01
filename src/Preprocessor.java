import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

public class Preprocessor {
    private final Set<Path> seenFiles = new HashSet<>();

    public Optional<FileReader> process(String stringPath) {
        try {
            final Path path = Paths.get(stringPath).toRealPath();
            if (seenFiles.contains(path)) {
                System.err.println("File \"" + path + "\" was already imported");
                return Optional.empty();
            }
            seenFiles.add(path);
            try {
                FileReader file = new FileReader(path.toFile());
                return Optional.of(file);
            } catch (IOException ex) {
                System.err.println("File \"" + path + "\" could not be open");
                throw new RuntimeException(ex);
            }
        } catch (IOException ex) {
            System.err.println("Path \"" + stringPath + "\" is not valid");
            throw new RuntimeException(ex);
        }
    }
}
