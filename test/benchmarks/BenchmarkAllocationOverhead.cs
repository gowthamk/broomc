using Midori.Runtime;
using System;
using System.Collections.Generic;
using System.Threading;

namespace BenchmarkAllocationOverhead
{

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

  public class BenchmarkAllocationOverhead
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
      if (args.Length != 3)
      {
        Console.WriteLine("Unexpected number of arguments. Please specify: size_of_region num_repetitions region|nonregion|regioncontext");
        return;
      }
      RegionAllocator.Initialize();
      uint size_of_region = (uint)Convert.ToInt32(args[0]);
      int num_repetitions = Convert.ToInt32(args[1]);
      Region region = RegionAllocator.AllocateRegion(size_of_region);
      Int64 start;
      Int64 end;
      Int64 cycles = 0;
      TestClass obj;
      if (args[2] == "nonregion")
      {
        for (int i = 0; i < num_repetitions; ++i)
        {
          start = RegionAllocator.NativeGetPerformanceCounter();
          obj = new TestClass(1);
          end = RegionAllocator.NativeGetPerformanceCounter();
          cycles += end - start;
        }
        Console.WriteLine("--- Allocate on the heap ---");
        Print((double)cycles / (double)num_repetitions);
      } else if (args[2] == "region")
      {
        for (int i = 0; i < num_repetitions; ++i)
        {
          start = RegionAllocator.NativeGetPerformanceCounter();
          obj = new TestClass(region, 1);
          end = RegionAllocator.NativeGetPerformanceCounter();
          cycles += end - start;
        }

        Console.WriteLine("--- Allocate in the region ---");
        Print((double)cycles / (double)num_repetitions);
      } else if (args[2] == "regioncontext")
      {
        for (int i = 0; i < num_repetitions; ++i)
        {
          using (RegionContext regContext = RegionContext.Create(region))
          {
            start = RegionAllocator.NativeGetPerformanceCounter();
            obj = new TestClass(1);
            end = RegionAllocator.NativeGetPerformanceCounter();
          }
          cycles += end - start;
        }

        Console.WriteLine("--- Allocate in the region using a context ---");
        Print((double)cycles / (double)num_repetitions);
      } else
      {
        Console.WriteLine("Unexpected option");
      }

    }

  }

}