using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using ConsoleApp5;
using System.Diagnostics;

namespace UnitTestProject1
{
    [TestClass]
    public class ProgramTester
    {

        ArrayAccumulator outputter;
        Program program;

        int[] A = new int[] { 1, 2, 3, 4, 5 };
        int[] B = new int[] { 3, 4, 5, 6, 7 };

        public ProgramTester()
        {
            outputter = new ArrayAccumulator();
            program = new Program(outputter);
        }

        [TestMethod]
        public void Test()
        {
            
            Assert.IsTrue(program.searchInt(A, -10));
        }

        [TestMethod]
        public void Test1()
        {
          
            Assert.IsTrue(program.searchInt(B, 76));
        }

        [TestMethod]
        public void Test2()
        {
          
            Assert.IsTrue(program.searchInt(B, 3232));
        }
    }
}
