package ro.razvan.uniud.oop.telephoneOperator.model;

import dagger.MembersInjector;
import javax.annotation.processing.Generated;

@Generated(
    value = "dagger.internal.codegen.ComponentProcessor",
    comments = "https://dagger.dev"
)
@SuppressWarnings({
    "unchecked",
    "rawtypes"
})
public final class App_MembersInjector implements MembersInjector<App> {
  public App_MembersInjector() {
  }

  public static MembersInjector<App> create() {
    return new App_MembersInjector();}

  @Override
  public void injectMembers(App instance) {
    injectExecute(instance);
  }

  public static void injectExecute(App instance) {
    instance.execute();
  }
}
