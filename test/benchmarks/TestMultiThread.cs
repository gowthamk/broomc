using Midori.Runtime;
using System;
using System.Collections.Generic;
using System.Threading;

namespace TestMultiThread
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

  public class WorkerObject
  {

    private int num_iterations;
    private int num_objects;

    public WorkerObject(int c_num_iterations, int c_num_objects)
    {
      num_iterations = c_num_iterations;
      num_objects = c_num_objects;
    }

    public void Allocate()
    {
      for (int i = 0; i < num_iterations; ++i)
      {
        var region = RegionAllocator.AllocateRegion(100000);
        using (RegionContext regContext = RegionContext.Create(region))
        {
          List<TestClass> objList = new List<TestClass>();
          for (int j = 0; j < num_objects; ++j)
          {
            objList.Add(new TestClass(region, 10));
          }
          RegionAllocator.FreeRegion(region);
        }
      }
    }

  }

  public class TestMultiThread
  {

    public static void Main(string[] args)
    {
      if (args.Length != 3)
      {
        Console.WriteLine("Unexpected number of arguments. Please specify: num_threads num_iterations num_objects");
        return;
      }
      RegionAllocator.Initialize();
      int num_threads = Convert.ToInt32(args[0]);
      int num_iterations = Convert.ToInt32(args[1]);
      int num_objects = Convert.ToInt32(args[2]);
      Thread[] workerThreads = new Thread[num_threads];
      for (int i = 0; i < num_threads; ++i)
      {
        WorkerObject workerObject =
          new WorkerObject(num_iterations, num_objects);
        workerThreads[i] = new Thread(workerObject.Allocate);
        workerThreads[i].Start();
      }
      for (int i = 0; i < num_threads; ++i)
      {
        workerThreads[i].Join();
      }
    }
  }

}
