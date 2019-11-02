package ro.razvan.uniud.asd.lab.es5;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

class Principale {

    public static void main(String[] args) throws IOException {

        try (
                final InputStreamReader isr = new InputStreamReader(System.in);
                final BufferedReader reader = new BufferedReader(isr)
        ) {
            System.out.println(
                    IntArrayUtils.binarySearch(
                            IntArrayUtils.valueOf(reader.readLine()), Integer.parseInt(reader.readLine())
                    )
            );
        }

    }
}
