using Midori.Runtime;
using System;

namespace GCTraceSpace {

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

  public class GCTrace
  {
    public static void Main(string[] args) {
      var region = RegionAllocator.AllocateRegion(10000);
      var region2 = RegionAllocator.AllocateRegion(10000);
      using (RegionContext regContext = RegionContext.Create(region))
      {
        var testObjReg2 = new TestClass(region2, 12);
        var testobjContext = new TestClass(11);
      }
      RegionAllocator.FreeRegion(region2);
      var testObjReg = new TestClass(region, 10);
      var testObjHeap = new TestClass(10);
      GC.Collect();
      var region3 = RegionAllocator.AllocateRegion(9000);
      var testObjReg3 = new TestClass(region3, 15);
      GC.Collect();
    }

  }

}
