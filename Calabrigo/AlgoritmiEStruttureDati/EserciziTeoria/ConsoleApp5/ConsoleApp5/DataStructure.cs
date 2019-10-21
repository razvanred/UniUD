using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

//current data structures: list, concatenate list
namespace DataStructure {
    //struttura di dati dei nodi di DataStructure
    struct data {
        public int pKey;
        public String name;

        public data(int pKey, String name) {
            this.pKey = pKey;
            this.name = name;
        }
    }

    //lista concatenata fifo
    class ConcatenateList {

        private ConcatenateListNode first;
        private ConcatenateListNode last;

        public ConcatenateList(ConcatenateListNode first,
            ConcatenateListNode last) {
            this.first = first;
            this.last = last;
        }

        public void Insert(data key) {
            ConcatenateListNode node = new ConcatenateListNode(key, null);
            if(first == null) {
                first = node;
                last = node;
            }
            else {
                last.next = node;
                last = node;
            }
        }

        public data getElement() {
            data element = first.key;
            ConcatenateListNode temp = first;
            first = first.next;
            temp.next = null;
            return element;
        }

        public static ConcatenateList createVoidQueue() {
            return new ConcatenateList(null, null);
        }

        public void printList() {
            ConcatenateListNode L = first;
            while (L != null) {
                Console.Write("\n" + L.key.pKey);
                L = L.next;
            }
        }
    }

    //classe di supporto per la lista concatenata
    class ConcatenateListNode {

        public data key;
        public ConcatenateListNode next;

        public ConcatenateListNode(data key, ConcatenateListNode next) {
            this.key = key;
            this.next = next;
        }

    }

    //lista normale fifo
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
