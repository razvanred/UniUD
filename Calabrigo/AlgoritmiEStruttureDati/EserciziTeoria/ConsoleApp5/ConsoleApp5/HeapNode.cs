using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DataStructure {
    //l'heap è una lista

    struct data {
        public int pKey;
        public String name;

        public data(int pKey, String name) {
            this.pKey = pKey;
            this.name = name;
        }
    }

    class HeapNode {
        private data key;
        private HeapNode next;

        public HeapNode(data key, HeapNode next) {
            this.key = key;
            this.next = next;
        }

        public void insertNode(HeapNode H, data key, int pos) {
            int j = 0;
            HeapNode L = H;
            while(j < pos - 1) {
                L = L.next;
                j++;
            }
            HeapNode C = new HeapNode(key, L.next);
            L.next = C;
        }

        public void tailInsert(HeapNode H, data key) {
            int j = 0;
            HeapNode L = H;
            while(L.next != null) {
                L = L.next;
            }
            L.next = new HeapNode(key, null);
        }

        public void deleteNode(HeapNode N, int pos) {
            int j = 0;
            HeapNode L = N;
            while(j < pos - 1) {
                L = L.next;
                j++;
            }
            HeapNode K = L.next;
            L.next = L.next.next;
            K = null;
        }

        public static HeapNode createHeap() {
            data key = new data(0, "");
            HeapNode next = null;
            HeapNode H = new HeapNode(key, next);
            return H;
        }

        public void printHeap(HeapNode N) {
            HeapNode L = N;
            while (L != null) {
                Console.Write("\n" + L.key.pKey);
                L = L.next;
            }
        }
    }
}
