package ro.razvan.uniud.fromSchemeToJava

/*
(define fibonacci  ; valore: naturale
  (lambda (n)      ; n: naturale
    (if (< n 2)
        1
        (+ (fibonacci (- n 2)) (fibonacci (- n 1)))
        )
    ))

*/

fun fibonacci(n: Int): Int {
    return if (n < 2) {
        1
    } else {
        fibonacci(n - 2) + fibonacci(n - 1)
    }
}


/*
(define s        ; valore: numero reale (misura lato)
  (lambda (k)    ; k: numero naturale (indice formato)
    (if (< k 2)
        (if (= k 0) s0 s1)  ; misure conosciute
        (/ (s (- k 2)) 2)   ; piegando due volte la lunghezza dei lati si dimezza
        )
    ))

(define s0 (* 100 (expt 2  1/4)))  ; lato maggiore formato A0 in cm

(define s1 (* 100 (expt 2 -1/4)))  ; lato minore   formato A0 in cm
*/

val s0: Double = 100 * Math.pow(2.toDouble(), 1.toDouble() / 4)
val s1: Double = 100 * Math.pow(2.toDouble(), (-1).toDouble() / 4)

fun s(k: Int): Double {
    return if (k < 2) {
        if (k == 0) {
            s0
        } else {
            s1
        }
    } else {
        s(k - 2) / 2
    }
}

/*
(define ones-complement  ; val: stringa di 0/1
  (lambda (bin)          ; bin: stringa di 0/1
    (if (string=? bin "")
        ""
        (string-append
         (ones-complement (substring bin 0 (- (string-length bin) 1)))
         (bit-complement (substring bin (- (string-length bin) 1)))
         ))
    ))

(define bit-complement   ; val: stringa
  (lambda (bit)          ; bit: stringa
    (if (string=? bit "0")
        "1"
        "0"
        )))

*/
const val ONE_CHAR = '1'
const val ZERO_CHAR = '0'

fun bitComplement(bin: Char): Char = when (bin) {
    ZERO_CHAR -> ONE_CHAR
    ONE_CHAR -> ZERO_CHAR
    else -> throw IllegalArgumentException("the string is not binary")
}

fun onesComplement(bin: String): String {
    return if (bin.isEmpty()) {
        bin
    } else {
        onesComplement(bin.substring(0, bin.length - 1)) + bitComplement(bin[bin.length - 1])
    }
}

/*
(define bin-succ                  ; valore: stringa di 0/1
  (lambda (b)                     ; b: stringa di 0/1
    (let ((n (string-length b)))
      (let ((lsb (string-ref b (- n 1))))  ; bit meno significativo
        (if (= n 1)
            (if (char=? lsb #\0)
                "1"
                "10")
            (if (char=? lsb #\0)
                (string-append (substring b 0 (- n 1)) "1")
                (string-append (bin-succ (substring b 0 (- n 1))) "0"))
            )
        ))))

*/
fun binSucc(b: String): String {

    val n = b.length
    val lsb = b[n - 1]

    return if (n == 1) {
        if (lsb == ZERO_CHAR) {
            ONE_CHAR.toString()
        } else {
            ONE_CHAR.toString() + ZERO_CHAR
        }
    } else {
        if (lsb == ZERO_CHAR) {
            b.substring(0, n - 1) + ONE_CHAR
        } else {
            binSucc(b.substring(0, n - 1)) + ZERO_CHAR
        }
    }
}

/*
(define btr-val                                               ; valore: intero
  (lambda (btr)                                               ; btr: stringa non vuota di -/./+
    (let ((k (- (string-length btr) 1))
          )
      (let ((pre (substring btr 0 k))                         ; pre = prefix
            (lsd (string-ref btr k))                          ; lsd = least significant digit
            )                                                 ; per facilitare la leggibilita'
        (if (= k 0)
            (btd-val lsd)
            (+ (* 3 (btr-val pre)) (btd-val lsd))
            )))
    ))

(define btd-val                                               ; valore: [-1, 0, 1]
  (lambda (btd)                                               ; d: carattere -/./+
    (cond ((char=? btd #\-) -1)
          ((char=? btd #\.)  0)
          ((char=? btd #\+) +1)
          )
    ))

*/

const val MINUS_CHAR = '-'
const val PLUS_CHAR = '+'
const val DOT_CHAR = '.'

fun btdVal(btd: Char): Int = when (btd) {
    MINUS_CHAR -> -1
    PLUS_CHAR -> 1
    DOT_CHAR -> 0
    else -> throw IllegalArgumentException("btd string not valid")
}

fun btrVal(btr: String): Int {
    val k = btr.length - 1
    val pre = btr.substring(0, k)
    val lsd = btr[k]

    return if (k == 0) {
        btdVal(lsd)
    } else {
        btrVal(pre) * 3 + btdVal(lsd)
    }
}

fun btrValIter(btr: String): Int {
    val n = btr.length

    val digitVals = IntArray(n) { i -> btdVal(btr[i]) }

    return digitVals.fold(0) { acc, act ->
        acc * 3 + act
    }
}