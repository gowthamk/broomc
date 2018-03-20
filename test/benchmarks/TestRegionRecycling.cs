using Midori.Runtime;
using System;
using System.Collections.Generic;
using System.Threading;

namespace TestRegionRecycling
{

  public class TestRegionRecycling
  {

    public static void Main(string[] args)
    {
      RegionAllocator.Initialize();
      if (args.Length != 1)
      {
        Console.WriteLine("Unexpected number of arguments. Please specify: num_regions");
        return;
      }
      int num_regions = Convert.ToInt32(args[0]);
      Region[] regions = new Region[num_regions];
      for (int i = 0; i < num_regions; ++i)
      {
        regions[i] = RegionAllocator.AllocateRegion(10000);
      }
      Region region = RegionAllocator.AllocateRegion(10000);
      using (RegionContext regContext = RegionContext.Create(region))
      {
        for (int i = 0; i < num_regions; ++i)
        {
          RegionAllocator.FreeRegion(regions[i]);
        }
      }
      Region failedRegion = RegionAllocator.AllocateRegion(10000);
      Console.WriteLine("Finish");
    }

  }

}