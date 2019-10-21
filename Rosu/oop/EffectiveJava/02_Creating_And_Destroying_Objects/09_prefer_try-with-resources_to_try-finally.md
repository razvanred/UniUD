# Prefer ```try```-with-resources to ```try-finally```

Historically, a ```try-finally``` statement was the best way to guarantee that a resource would be closed properly, even in the face of an exception or return:

```java
static String firstLineOfFile(final String path) throws IOException {
    final var br = new BufferedReader(new FileReader(path));
    try {
        return br.readLine();
    } finally {
        br.close();
    }
}
```

This may not look bad, but it gets worse when you add a second resource:

```java
static void copy(final String src, final String dst) throws IOException {
    final InputStream in = new FileInputStream(src);
    try {
        final OutputStream out = new FileOutputStream(dst);
        try {
            final byte[] buff = new byte[BUFFER_SIZE];
            int n;
            while ((n = in.read(buff)) >= 0) {
                out.write(buff, 0, n);
            }
        } finally {
            out.close();
        }
    } finally {
        in.close();
    }
}
```

The code in both the ```try``` block and the ```finally``` block is capable of throwing exceptions: the second exception (thrown by the first method) completely obliterates the first one. There is no record of the first exception in the exception of the stack trace.

All these problems were solved in one fell swoop when Java 7 introducted the [```try```-with-resources](https://docs.oracle.com/javase/specs/jls/se7/html/jls-14.html#jls-14.20.3) statement. To be usable with this construct, a resource must implement the ```AutoCloseable``` interface.

```java
static String firstLineOfFile(final String path) throws IOException {
    try (final var br = new BufferedReader(new FileReader(path))) {
        return br.readLine();
    }
}

static void copy(final String src, final String dst) {
    try (
        final InputStream in = new FileInputStream(src);
        final OutputStream out = new FileOutputStream(dst);
    ) {
        final byte[] buff = new byte[BUFFER_SIZE];
        int n;
        while ((n = in.read(buff)) >= 0){
            out.write(buff, 0, n);
        }
    }
}
```

If exceptions are thrown by both ```readLine``` call and the (invisible) ```close```, the latter exception is _suppressed_ in favor of the former. Multiple exception may be suppressed in order to preserve the exception that you actually want to see. These suppressed exceptions are not merely discarded; they are printed in the stack trace with a notation saying that they were suppressed. You can also access them programmatically with the ```getSuppressed``` method, which was added to ```Throwable``` in Java 7.

```Try```-with-resource with a ```catch``` clause:

```java
static String firstLineOfFile(final String path, final String defaultVal) {
    try (final var br = new BufferedReader(new FileReader(path))) {
        return br.readLine();
    } catch (final IOException exc) {
        return defaultVal;
    }
}
```
