package ro.razvan.learning.dependencyInjectionDagger;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public interface Command {

    @NotNull String key();

    @NotNull Result handleInput(final @NotNull String[] input);

    final class Result {
        private final @NotNull Status status;
        private final @Nullable CommandRouter nestedCommandRouter;

        @Contract(pure = true)
        private Result(final @NotNull Status status, final @Nullable CommandRouter nestedCommandRouter) {
            this.nestedCommandRouter = nestedCommandRouter;
            this.status = status;
        }

        @Contract(value = "_ -> new", pure = true)
        public static @NotNull Result enterNestedCommandSet(final @NotNull CommandRouter nestedCommandRouter) {
            return new Result(Status.HANDLED, nestedCommandRouter);
        }

        @Contract(value = " -> new", pure = true)
        public static @NotNull Result handled() {
            return new Result(Status.HANDLED, null);
        }

        @Contract(value = " -> new", pure = true)
        public static @NotNull Result inputCompleted() {
            return new Result(Status.INPUT_COMPLETED, null);
        }

        @Contract(value = " -> new", pure = true)
        public static @NotNull Result invalid() {
            return new Result(Status.INVALID, null);
        }

        @Contract(pure = true)
        public @NotNull Status status() {
            return status;
        }

        @Contract(pure = true)
        public @Nullable CommandRouter nestedCommandRouter() {
            return nestedCommandRouter;
        }
    }

    enum Status {
        INVALID,
        HANDLED,
        INPUT_COMPLETED
    }
}
