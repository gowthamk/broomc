using System;
using Midori.Runtime;

namespace TestContextInContext
{

  class TestContextInContext
  {

    static void Main(string[] args)
    {
      RegionAllocator.Initialize();
      Region outerRegion = RegionAllocator.AllocateRegion(10000);
      Region otherRegion = RegionAllocator.AllocateRegion(10000);
      using (RegionContext outerContext = RegionContext.Create(outerRegion))
      {
        Region innerRegion = RegionAllocator.AllocateRegion(10000);
        using (RegionContext innerContext = RegionContext.Create(innerRegion))
        {
        }
        using (RegionContext otherContext = RegionContext.Create(otherRegion))
        {
        }
        using (RegionContext innerContext = RegionContext.Create(innerRegion))
        {
        }
      }
    }

  }

}