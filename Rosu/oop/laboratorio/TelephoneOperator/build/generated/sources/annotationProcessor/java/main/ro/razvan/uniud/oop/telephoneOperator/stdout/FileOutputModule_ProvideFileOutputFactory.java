package ro.razvan.uniud.oop.telephoneOperator.stdout;

import dagger.internal.Factory;
import dagger.internal.Preconditions;
import javax.annotation.processing.Generated;
import javax.inject.Provider;

@Generated(
    value = "dagger.internal.codegen.ComponentProcessor",
    comments = "https://dagger.dev"
)
@SuppressWarnings({
    "unchecked",
    "rawtypes"
})
public final class FileOutputModule_ProvideFileOutputFactory implements Factory<Outputter> {
  private final Provider<String> fileNameProvider;

  private final Provider<Outputter> errorStreamProvider;

  public FileOutputModule_ProvideFileOutputFactory(Provider<String> fileNameProvider,
      Provider<Outputter> errorStreamProvider) {
    this.fileNameProvider = fileNameProvider;
    this.errorStreamProvider = errorStreamProvider;
  }

  @Override
  public Outputter get() {
    return provideFileOutput(fileNameProvider.get(), errorStreamProvider.get());
  }

  public static FileOutputModule_ProvideFileOutputFactory create(Provider<String> fileNameProvider,
      Provider<Outputter> errorStreamProvider) {
    return new FileOutputModule_ProvideFileOutputFactory(fileNameProvider, errorStreamProvider);
  }

  public static Outputter provideFileOutput(String fileName, Outputter errorStream) {
    return Preconditions.checkNotNull(FileOutputModule.provideFileOutput(fileName, errorStream), "Cannot return null from a non-@Nullable @Provides method");
  }
}
