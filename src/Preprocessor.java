import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

public class Preprocessor {
    private final Path basePath;
    private final Set<Path> seenFiles = new HashSet<>();

    public Preprocessor(Path basePath) {
        this.basePath = basePath;
    }

    public Optional<FileReader> process(String stringPath) {
        Path path = basePath.resolve(stringPath);

        if (seenFiles.stream().anyMatch(path1 -> {
            try {
                return Files.isSameFile(path1, path);
            } catch (IOException ex) {
                System.err.println("Path \"" + stringPath + "\" is not valid");
                throw new RuntimeException(ex);
            }
        })) {
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

    }
}
