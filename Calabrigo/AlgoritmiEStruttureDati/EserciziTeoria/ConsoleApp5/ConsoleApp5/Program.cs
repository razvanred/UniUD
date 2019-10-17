using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DataStructure;

namespace ConsoleApp5 {
    public class Program {
        Outputter outputter;


        public Program(Outputter outputter) {
            this.outputter = outputter;
        }

        static void Main(string[] args) {
            //Program p = new Program(new ConsolePrinter());
            HeapNode H = HeapNode.createHeap();
            data key = new data(1, "a");
            H.tailInsert(H, key);
            key = new data(2, "a");
            H.tailInsert(H, key);
            key = new data(3, "a");
            H.tailInsert(H, key);
            H.tailInsert(H, key);
            H.tailInsert(H, key);
            H.tailInsert(H, key);
            key = new data(2, "a");
            H.insertNode(H, key, 5);
            H.deleteNode(H,2);
            H.printHeap(H);
            Console.ReadKey();
        }

        public bool searchInt(int[] array, int k) {
            return searchIntRic(array, k, 0, array.Length - 1);
        }

        private bool searchIntRic(int[] array, int k, int p, int q) {
            int idx = (q + p) / 2;
            bool found = array[idx] == k;
            if (found || (q - p) <= 0) {
                return found;
            }
            else if (array[idx] < k) {
                return searchIntRic(array, k, idx + 1, q);
            }
            else {
                return searchIntRic(array, k, p, idx - 1);
            }
        }

        public void selectionSort(ref int[] array) {
            int minIdx;
            for (int j = 0; j < array.Length; j++) {
                minIdx = j;
                for (int i = j + 1; i < array.Length; i++) {
                    if (array[i] < array[minIdx]) {
                        minIdx = i;
                    }
                }
                int temp;
                temp = array[j];
                array[j] = array[minIdx];
                array[minIdx] = temp;
            }
        }

        public void intersection(ref int[] A, ref int[] B) {
            int i = 0, j = 0;
            while (i < A.Length && j < B.Length) {
                if (A[i] < B[j]) {
                    i++;
                }
                else if (A[i] == B[j]) {
                    //Console.Write("\n" + A[i]);
                    i++;
                    j++;
                }
                else {
                    j++;
                }
            }
        }

        public void recursiveIntersection(int[] A, int[] B, int i, int j) {
            if (i < A.Length && j < B.Length) {
                if (A[i] < B[j]) {
                    recursiveIntersection(A, B, i + 1, j);
                }
                else if (A[i] == B[j]) {
                    outputter.print(A[i]);
                    recursiveIntersection(A, B, i + 1, j + 1);
                }
                else {
                    recursiveIntersection(A, B, i, j + 1);
                }
            }
        }



    }
}
