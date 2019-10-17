using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ConsoleApp5
{
    class ConsolePrinter : Outputter
    {
        public void print(int x)
        {
            Console.WriteLine(x);
        }
    }
}
