using Midori.Runtime;
using System;
using System.Collections.Generic;
using System.Threading;
using WarmHeap;

namespace BenchmarkSingleObject
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

  public class BenchmarkSingleObject
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

    public static void AllocateInRegion(Region region, int num_objects)
    {
      Int64 start;
      Int64 end;
      TestClass obj;
      start = RegionAllocator.NativeGetPerformanceCounter();
      for (int i = 0; i < num_objects; ++i)
      {
        obj = new TestClass(region, 1);
      }
      end = RegionAllocator.NativeGetPerformanceCounter();
      Console.WriteLine("--- Allocate in the region ---");
      Print(end - start);
    }

    public static void AllocateInRegionContext(Region region, int num_objects)
    {
      Int64 start;
      Int64 end;
      TestClass obj;
      start = RegionAllocator.NativeGetPerformanceCounter();
      using (RegionContext regContext = RegionContext.Create(region))
      {
        for (int i = 0; i < num_objects; ++i)
        {
          obj = new TestClass(1);
        }
      }
      end = RegionAllocator.NativeGetPerformanceCounter();
      Console.WriteLine("--- Allocate using the regioncontext ---");
      Print(end - start);
    }

    public static void Allocate(int num_objects)
    {
      Int64 start;
      Int64 end;
      TestClass obj;
      start = RegionAllocator.NativeGetPerformanceCounter();
      for (int i = 0; i < num_objects; ++i)
      {
        obj = new TestClass(1);
      }
      end = RegionAllocator.NativeGetPerformanceCounter();
      Console.WriteLine("--- Allocate on the heap ---");
      Print(end - start);
    }

    public static void Main(string[] args)
    {
      HeapWarmer heapWarmer = new HeapWarmer();
      if (args.Length != 3 && args.Length != 4)
      {
        Console.WriteLine("Unexpected number of arguments. Please specify: region|nonregion|regioncontext size_of_region, num_objects");
        return;
      }
      RegionAllocator.Initialize();
      uint size_of_region = (uint)Convert.ToInt32(args[1]);
      int num_objects = Convert.ToInt32(args[2]);
      if (args.Length == 4 && args[3] == "warm")
      {
        heapWarmer.WarmHeap(1000);
      }
      if (args[0] == "region")
      {
        Region region = RegionAllocator.AllocateRegion(size_of_region);
        AllocateInRegion(region, num_objects);
      }
      else if (args[0] == "regioncontext")
      {
        Region region = RegionAllocator.AllocateRegion(size_of_region);
        AllocateInRegionContext(region, num_objects);
      }
      else if (args[0] == "nonregion")
      {
        Allocate(num_objects);
      }
      else
      {
        Console.WriteLine("Unexpected region type");
      }
    }

  }

}