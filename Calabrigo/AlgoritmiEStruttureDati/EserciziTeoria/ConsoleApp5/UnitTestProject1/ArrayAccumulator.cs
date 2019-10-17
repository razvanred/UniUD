using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ConsoleApp5;

namespace UnitTestProject1
{
    class ArrayAccumulator : Outputter
    {
        private List<int> list = new List<int>();

        public void print(int x)
        {
            list.Add(x);
        }

        public int[] getList()
        {
            return list.ToArray();
        }
    }
}
