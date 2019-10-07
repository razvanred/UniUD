func selectionSort(_ a: inout [Int]) {
    for i in 0..<a.count {
        var smallerKey = i
        for j in i + 1..<a.count {
            if(a[smallerKey] > a[j]){
                smallerKey = j
            }
        }
        let smaller = a[smallerKey]
        a[smallerKey] = a[i]
        a[i] = smaller 
    }
}

var array: Array<Int> = [3, 2, 1]
selectionSort(&array)
print(array)