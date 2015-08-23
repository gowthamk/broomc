using Midori.Runtime;
using System;
using System.Collections.Generic;
using System.Threading;

namespace GrowableRegions {

  public class TestClass
  {
    public int m_Count;

    public TestClass(int count)
    {
      this.m_Count = count;
    }

    public TestClass(Region region, int count)
    {
      this.m_Count = count;
    }

    public TestClass(Region region)
    {
      this.m_Count = 1;
    }

  }

  public class TestGrowableRegions
  {

    public static void Main(string[] args)
    {
      RegionAllocator.Initialize();
      var tinyRegion = RegionAllocator.AllocateRegion(500);
      for (int i = 0; i < 100; i++)
      {
        TestClass obj = new TestClass(tinyRegion, 10);
      }

      using (var context = RegionContext.Create(tinyRegion))
      {
        char[] alphabet = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'};
        for (int i = 0; i < 100; i++)
        {
          string alpha = new string(alphabet);
        }
      }

      RegionAllocator.FreeRegion(tinyRegion);

      Console.WriteLine("Finish");
    }

  }

}