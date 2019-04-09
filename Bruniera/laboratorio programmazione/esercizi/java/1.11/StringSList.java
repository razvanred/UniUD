class StringSList{
  private final boolean empty;          
  private final String first;               
  private final StringSList rest;
  
  public static final StringSList NULL_STRINGLIST = new StringSList();
  
  public new StringSList(){
    empty=false;
    first=null;
    rest=null;
  }
  
  
}