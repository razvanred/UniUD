/**
 Razvan Rosu <razvanred.work@gmail.com>
 MergeSort di un array di interi.
 */

func mergeSort(_ a: inout [Int]) {
    for j in 1..<a.count {
        let k = a[j]
        var i = j - 1
        while i >= 0 && a[i] > k {
            a[i + 1] = a[i]
            i -= 1
        }
        a[i + 1] = k
    }
}

var array: Array<Int> = [3, 2, 1]
mergeSort(&array)
print(array)
