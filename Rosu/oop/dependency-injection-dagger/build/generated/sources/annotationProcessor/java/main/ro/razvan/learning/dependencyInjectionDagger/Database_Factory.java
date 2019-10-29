package ro.razvan.learning.dependencyInjectionDagger;

import dagger.internal.Factory;
import javax.annotation.processing.Generated;

@Generated(
    value = "dagger.internal.codegen.ComponentProcessor",
    comments = "https://dagger.dev"
)
@SuppressWarnings({
    "unchecked",
    "rawtypes"
})
public final class Database_Factory implements Factory<Database> {
  @Override
  public Database get() {
    return newInstance();
  }

  public static Database_Factory create() {
    return InstanceHolder.INSTANCE;
  }

  public static Database newInstance() {
    return new Database();
  }

  private static final class InstanceHolder {
    private static final Database_Factory INSTANCE = new Database_Factory();
  }
}
