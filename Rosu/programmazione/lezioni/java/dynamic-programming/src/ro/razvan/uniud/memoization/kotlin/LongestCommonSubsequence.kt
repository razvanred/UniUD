package ro.razvan.uniud.memoization.kotlin

import kotlin.math.max

object LongestCommonSubsequence {

    private const val UNKNOWN = -1

    fun llcsRec(u: String, v: String): Int = when {
        u.isEmpty() || v.isEmpty() -> 0
        u[0] == v[0] -> 1 + llcsRec(u.substring(1), v.substring(1))
        else -> max(
            llcsRec(u.substring(1), v),
            llcsRec(u, v.substring(1))
        )
    }

    fun llcsMemRec(
        u: String,
        v: String
    ): Int = llcsMemRec(u, v, Array(u.length + 1) { IntArray(v.length + 1) { UNKNOWN } })

    private fun llcsMemRec(
        u: String,
        v: String,
        matrix: Array<IntArray>
    ): Int {

        if (matrix[u.length][v.length] == UNKNOWN) {

            matrix[u.length][v.length] = when {
                u.isEmpty() || v.isEmpty() -> 0
                u[0] == v[0] -> 1 + llcsMemRec(u.substring(1), v.substring(1), matrix)
                else -> max(
                    llcsMemRec(u.substring(1), v),
                    llcsMemRec(u, v.substring(1))
                )
            }

        }

        return matrix[u.length][v.length]
    }

    fun llcsIter(u: String, v: String): Int {

        val m = u.length
        val n = v.length
        val matrix = Array(m + 1) { i ->
            IntArray(n + 1) { j ->
                if (j == 0 || i == 0) {
                    0
                } else {
                    UNKNOWN
                }
            }
        }

        for (i in 1 until matrix.size) {

            for (j in 1 until matrix[i].size) {

                matrix[i][j] = when {

                    u.isEmpty() || v.isEmpty() -> 0

                    u[m - i] == v[n - j] -> 1 + matrix[i - 1][j - 1]

                    else -> max(
                        matrix[i - 1][j],
                        matrix[i][j - 1]
                    )

                }

            }

        }

        return matrix[m][n]
    }

    fun lcs(u: String, v: String): String {
        val m = u.length
        val n = v.length
        val matrix = Array(m + 1) { i ->
            IntArray(n + 1) { j ->
                if (j == 0 || i == 0) {
                    0
                } else {
                    UNKNOWN
                }
            }
        }

        for (i in 1 until matrix.size) {

            for (j in 1 until matrix[i].size) {
                matrix[i][j] = when {

                    u.isEmpty() || v.isEmpty() -> 0

                    u[m - i] == v[n - j] -> 1 + matrix[i - 1][j - 1]

                    else -> max(
                        matrix[i - 1][j],
                        matrix[i][j - 1]
                    )

                }
            }

        }

        // fine llcs

        val builder = StringBuilder()
        var i = m
        var j = n

        while (matrix[i][j] != 0) {

            when {

                u[m - i] == v[n - j] -> {
                    builder.append(u[m - i])
                    j--
                    i--
                }

                matrix[i - 1][j] < matrix[i][j - 1] -> j--
                else -> i--

            }

        }

        return builder.toString()
    }


}