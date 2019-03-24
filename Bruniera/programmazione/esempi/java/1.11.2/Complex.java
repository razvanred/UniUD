public class Complex{
  
  private final double real;
  private final double imaginary;
  public static final Complex ZERO_COMPLEX=new Complex();
  
  public Complex(){
    real=0;
    imaginary=0;
  }
  
  public Complex(double real, double imaginary){
    this.real=real;
    this.imaginary=imaginary;
  }
  
  public double real(){
    return real;
  }
  
  public double imaginary(){
    return imaginary;
  }
  
  public boolean zeroComplex(){
    return (real*imaginary)==0;
  }
  
  public Complex sum(Complex b){
    return new Complex(real+b.real(), imaginary+b.imaginary());
  }
  
  public Complex dif(Complex b){
    return new Complex(real-b.real(), imaginary-b.imaginary());
  }
  
  public Complex mul(Complex b){
    return new Complex(real*b.real()+imaginary*b.imaginary(), real*b.imaginary()+imaginary*b.real());
  }
  
  public Complex div(Complex b){
    return new Complex(real*b.real()+imaginary*b.imaginary(), real*b.imaginary()-imaginary*b.real());
  }
  
  public String toString(){
    return "("+real+" i"+imaginary+")";
  }
}