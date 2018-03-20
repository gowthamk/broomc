using Midori.Runtime;
using System;
using System.Collections.Generic;

namespace StressRegionSpace
{

  public class TestClass
  {
    public string m_test;

    public TestClass(Region region, string test)
    {
      m_test = test;
    }

    public TestClass(string test)
    {
      m_test = test;
    }

  }

  public class StressRegionSpace
  {

    public static void AllocateInnerLoop(int num_iterations, string general)
    {
      for (int i = 0; i < 100; ++i)
      {
        for (int j = 0; j < num_iterations; ++j)
        {
          TestClass testObj = new TestClass(general);
        }
      }
    }

    public static void AllocateInnerLoopRegion(int num_iterations,
                                               string general)
    {
      for (int i = 0; i < 100; ++i)
      {
        var region = RegionAllocator.AllocateRegion(40000000);
        for (int j = 0; j < num_iterations; ++j)
        {
          TestClass testObj = new TestClass(region, general);
        }
        RegionAllocator.FreeRegion(region);
      }
    }

    public static void AllocateInnerLoopRegionContext(int num_iterations,
                                                      string general)
    {
      for (int i = 0; i < 100; ++i)
      {
        var region = RegionAllocator.AllocateRegion(40000000);
        using (RegionContext regContext = RegionContext.Create(region))
        {
          for (int j = 0; j < num_iterations; ++j)
          {
            TestClass testObj = new TestClass(region, general);
          }
        }
      }
    }

    public static void AllocateInnerOuterLoop(int num_iterations,
                                              string general)
    {
      int count = 0;
      for (int i = 0; i < 100; ++i)
      {
        TestClass[] objList = new TestClass[100000];
        for (int j = 0; j < 100000; ++j)
        {
          objList[j] = new TestClass(general);
        }
        for (int j = 0; j < num_iterations; ++j)
        {
          TestClass testObj = new TestClass(general);
        }
        count += objList.Length;
      }
    }

    public static void AllocateInnerOuterLoopRegion(int num_iterations,
                                                    string general)
    {
      // NOTE: This does quite implement the same behaviour as
      // AllocateInnerOuterLoop
      for (int i = 0; i < 100; ++i)
      {
        var longRegion = RegionAllocator.AllocateRegion(40000000);
        using (RegionContext regContext = RegionContext.Create(longRegion))
        {
          TestClass[] objList = new TestClass[100000];
          for (int j = 0; j < 100000; ++j)
          {
            objList[j] = new TestClass(longRegion, general);
          }
        }
        var region = RegionAllocator.AllocateRegion(40000000);
        for (int j = 0; j < num_iterations; ++j)
        {
          TestClass testObj = new TestClass(region, general);
        }
        RegionAllocator.FreeRegion(region);
      }
    }

    public static void AllocateInnerOuterLoopRegionContext(int num_iterations,
                                                           string general)
    {
      for (int i = 0; i < 100; ++i)
      {
        var longRegion = RegionAllocator.AllocateRegion(40000000);
        using (RegionContext outerRegContext = RegionContext.Create(longRegion))
        {
          TestClass[] objList = new TestClass[100000];
          for (int j = 0; j < 100000; ++j)
          {
            objList[j] = new TestClass(general);
          }
          var region = RegionAllocator.AllocateRegion(40000000);
          using (RegionContext regContext = RegionContext.Create(region))
          {
            for (int j = 0; j < num_iterations; ++j)
            {
              TestClass testObj = new TestClass(general);
            }
          }
        }
      }
    }

    public static void Main(string[] args)
    {
      Random random = new Random();
      string general = "teststringisgettingbiggerandbiggerit'salreadybigdata";
      int generalLen = general.Length;
      if (args.Length != 3)
      {
        Console.WriteLine("Please supply arguments: gen0|gen1 region|nonregion|regioncontext number_inner_iterations");
        return;
      }
      RegionAllocator.Initialize();
      int num_iterations = Convert.ToInt32(args[2]);
      if (args[0] == "gen0")
      {
        if (args[1] == "region")
        {
          AllocateInnerLoopRegion(num_iterations, general);
        } else if (args[1] == "regioncontext")
        {
          AllocateInnerLoopRegionContext(num_iterations, general);
        } else if (args[1] == "nonregion")
        {
          AllocateInnerLoop(num_iterations, general);
        } else
        {
          Console.WriteLine("Unexpected argument");
        }
      } else if (args[0] == "gen1")
      {
        if (args[1] == "region")
        {
          AllocateInnerOuterLoopRegion(num_iterations, general);
        } else if (args[1] == "regioncontext")
        {
          AllocateInnerOuterLoopRegionContext(num_iterations, general);
        } else if (args[1] == "nonregion")
        {
          AllocateInnerOuterLoop(num_iterations, general);
        } else
        {
          Console.WriteLine("Unexpected argument");
        }
      } else
      {
        Console.WriteLine("Unexpected argument");
      }
    }

  }
}