package ro.razvan.uniud.asd.lab.es4;

import java.util.Arrays;
import java.util.Scanner;

class Principale {

    public static void main(String[] args) {
        Arrays.stream(StringUtils.prefixes(new Scanner(System.in).nextLine()))
                .forEach(System.out::println);
    }
}
