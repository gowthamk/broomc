using Midori.Runtime;
using System;
using System.Collections.Generic;
using System.Threading;

namespace BenchmarkRegionCreation
{

  public class BenchmarkRegionCreation
  {

    public static void Print(Int64 cycles)
    {
      Int64 frequency = RegionAllocator.NativeGetPerformanceFrequency();
      Console.WriteLine("Cycles: {0}\n", cycles);
      cycles *= 1000000000;
      double duration = (double)cycles / (double)frequency;
      Console.WriteLine("Time(ns): {0}\n", duration);
    }

    public static void Print(double cycles)
    {
      Int64 frequency = RegionAllocator.NativeGetPerformanceFrequency();
      Console.WriteLine("Cycles: {0}\n", cycles);
      cycles *= 1000000000.0;
      double duration = cycles / (double)frequency;
      Console.WriteLine("Time(ns): {0}\n", duration);
    }

    public static void Main(string[] args)
    {
      if (args.Length != 2)
      {
        Console.WriteLine("Unexpected number of arguments. Please specify: size_of_region, num_repetitions");
        return;
      }
      RegionAllocator.Initialize();
      uint size_of_region = (uint)Convert.ToInt32(args[0]);
      int num_repetitions = Convert.ToInt32(args[1]);
      Int64 start;
      Int64 end;
      Int64 cycles = 0;
      for (int i = 0; i < num_repetitions; ++i)
      {
        start = RegionAllocator.NativeGetPerformanceCounter();
        RegionAllocator.AllocateRegion(size_of_region);
        end = RegionAllocator.NativeGetPerformanceCounter();
        cycles += end - start;
      }
      Print(cycles / num_repetitions);
    }

  }

}