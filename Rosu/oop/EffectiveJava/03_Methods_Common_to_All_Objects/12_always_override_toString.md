# Always override ```toString```

## General Contract

The ```Object```'s ```toString``` implementation consists of the class name followed by an ```@``` and the unsigned hexadecimal representation of the hash code: ```Person@123b91```.

The ```toString``` general contract says that the method should return a string that represent a concise and representive information about the object.

> It is recommended that all subclasses override this method.

While it isn't as critical as obeying the ```equals``` and ```hashCode``` contracts, **providing a good ```toString``` implementation makes your class much more pleasant to use and makes systems using class easier to debug**.

**When practical, the ```toString``` method should return all of the interesting information contained in the object**, for example: ```Person{phoneNumber=333-123-2198}``` is better than ```Person{phoneNumber=PhoneNumber@123b92}```.

Ideally, the string should be self-explanatory.

## Format for _value classes_

If you want to specify the format the _value class_ will return, it's usually a good idea to provide a matching static factory or constructor so programmers can easily translate back and forth between the object and its string representation. This approach is taken for example by ```BigDecimal``` and ```BigInteger```, and by most of the boxed primitive classes.

The disadvantage of specifying the format of the ```toString``` return value is that once you've specified it, you're stuck with it for life, assuming your class is widely used. By choosing not to specify a format, you preserve the flexibility to add information or improve the format in a subsequent release.

**Whether or not you decide to specify the format, you should clearly document your intentions.**

Whether or not you specify the format, **provide programmatic access to the information contained in the value returned by ```toString```.**

## Tips

* It makes no sense to write a ```toString``` method in a [static utility class](../02_Creating_And_Destroying_Objects/04_enforce_noninstantiability_with_a_private_constructor.md).
* Nor should you write a ```toString``` method in most [enum types](../06_Enums_and_Annotations/34_use_enums_instead_of_int_constants.md) because Java provides a perfectly good one for you.
* You shoudl write a ```toString``` method in any abstract class whose subclasses share a common string representation.
* Google's [AutoValue framework](https://github.com/google/auto/tree/master/value) will generate a ```toString``` method for you.
