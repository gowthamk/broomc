using System;
using System.Collections.Generic;
using Midori.Runtime;

namespace TestRegionContext
{
  class Program
  {
    public static void Main(string[] args)
    {
      RegionAllocator.Initialize();
      Console.WriteLine("Allocating region");
      System.Diagnostics.Debugger.Break();
      var region = RegionAllocator.AllocateRegion(10000);
      Console.WriteLine("Allocated region");

      using (var c = RegionContext.Create(region))
        {
          Console.WriteLine("Created region context");
          var dictionary = new Dictionary<int, int>();
          dictionary.Add(10, 10);
          Console.WriteLine("{0}", dictionary[10]);
        }

      RegionAllocator.FreeRegion(region);
    }
  }
}