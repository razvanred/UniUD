package ro.razvan.uniud.oop.telephoneOperator.stdout;

import dagger.internal.Factory;
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
public final class FileOutput_Factory implements Factory<FileOutput> {
  private final Provider<String> fileNameProvider;

  private final Provider<Outputter> errorOutputterProvider;

  public FileOutput_Factory(Provider<String> fileNameProvider,
      Provider<Outputter> errorOutputterProvider) {
    this.fileNameProvider = fileNameProvider;
    this.errorOutputterProvider = errorOutputterProvider;
  }

  @Override
  public FileOutput get() {
    return newInstance(fileNameProvider.get(), errorOutputterProvider.get());
  }

  public static FileOutput_Factory create(Provider<String> fileNameProvider,
      Provider<Outputter> errorOutputterProvider) {
    return new FileOutput_Factory(fileNameProvider, errorOutputterProvider);
  }

  public static FileOutput newInstance(String fileName, Outputter errorOutputter) {
    return new FileOutput(fileName, errorOutputter);
  }
}
