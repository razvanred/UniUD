package telephonicOperator;

public class Owner{
	private String name, surname;

	public Owner(String name, String surname){
		this.name=name;
		this.surname=surname;
	}

	public String getName(){
		return name;
	}

	public String getSurname(){
		return surname;
	}
}
