using Midori.Runtime;
using System;
using System.Collections.Generic;
using System.Threading;

namespace BlocksReuse {

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

  public class TestBlocksReuse
  {

    public static void Main(string[] args)
    {
      RegionAllocator.Initialize();
      var smallRegion = RegionAllocator.AllocateRegion(5000);
      TestClass obj1 = new TestClass(smallRegion, 1);
      RegionAllocator.FreeRegion(smallRegion);
      var largeRegion = RegionAllocator.AllocateRegion(50000);
      TestClass obj2 = new TestClass(largeRegion, 2);
      RegionAllocator.FreeRegion(largeRegion);
      var tinyRegion = RegionAllocator.AllocateRegion(500);
      Console.WriteLine("Finish");
    }

  }

}