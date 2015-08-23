using System;
using System.Collections.Generic;

namespace StressRegionSpace
{

  public class TestClass
  {
    public string m_test;

    public TestClass(string test)
    {
      m_test = test;
    }

  }

  public class StressRegionSpace
  {

    public static void AllocateInner(int num_iterations, string general)
    {
      for (int i = 0; i < 100; ++i)
      {
        for (int j = 0; j < num_iterations; ++j)
        {
          TestClass testObj = new TestClass(general);
        }
      }
    }

    public static void AllocateInnerOuter(int num_iterations, string general)
    {
      int count = 0;
      for (int i = 0; i < 100; ++i)
      {
        List<TestClass> objList = new List<TestClass>();
        for (int j = 0; j < 100000; ++j)
        {
          objList.Add(new TestClass(general));
        }
        for (int j = 0; j < num_iterations; ++j)
        {
          TestClass testObj = new TestClass(general);
        }
        count += objList.Count;
      }
    }

    public static void Main(string[] args)
    {
      Random random = new Random();
      string general = "teststringisgettingbiggerandbiggerit'salreadybigdata";
      int generalLen = general.Length;
      if (args.Length != 2)
      {
        Console.WriteLine("Please supply arguments: gen0|gen1 num_inner_iteations");
      }
      int num_iterations = Convert.ToInt32(args[1]);
      if (args[0] == "gen0")
      {
        AllocateInner(num_iterations, general);
      } else if (args[0] == "gen1")
      {
        AllocateInnerOuter(num_iterations, general);
      } else
      {
        Console.WriteLine("Unexpected argument");
      }
    }

  }

}
