#include <iostream>
#include <vector>

int main() {
    // template - classi che dipendono da altri tipi
    std::vector<int> array(100); // vettori allocati in memoria dinamica
    array.push_back(4);
    std::cout << array[100] << std::endl;

    return 0;
}