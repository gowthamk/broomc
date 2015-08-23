using Midori.Runtime;
using System;
using System.Collections.Generic;
using System.Threading;


namespace WarmRegion
{

  public class RegionWarmer
  {

    public static void WarmAllocRegions(uint num_regions, uint region_size)
    {
      for (int i = 0; i < num_regions; i++)
      {
        RegionAllocator.AllocateRegion(region_size);
      }
    }

    public static void WarmFreeList(uint num_regions, uint region_size)
    {
      for (int i = 0; i < num_regions; i++)
      {
        Region region = RegionAllocator.AllocateRegion(region_size);
        RegionAllocator.FreeRegion(region);
      }
    }

  }

}