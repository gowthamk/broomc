using System;
using Midori.Runtime;

namespace TestStressRegionContext
{

  class TestStressRegionContext
  {

    static void StressRegionContext(int num_iterations)
    {
      Region region = RegionAllocator.AllocateRegion(1000);
      for (int i = 0; i < num_iterations; i++)
      {
        using (RegionContext regContext = RegionContext.Create(region))
        {
        }
        // TODO(ionel): Make sure that the region doesn't get freed at the end
        // of the using.
      }
    }

    static void Main(string[] args)
    {
      RegionAllocator.Initialize();
      if (args.Length != 2)
      {
        Console.WriteLine("Please specify: empty_stack|non_empty_stack num_iterations");
      }
      int num_iterations = Convert.ToInt32(args[1]);
      if (args[0] == "non_empty_stack")
      {
        Region outerRegion = RegionAllocator.AllocateRegion(1000);
        using (RegionContext outerRegCxt = RegionContext.Create(outerRegion))
        {
          StressRegionContext(num_iterations);
        }
      }
      else
      {
        StressRegionContext(num_iterations);
      }
    }

  }

}