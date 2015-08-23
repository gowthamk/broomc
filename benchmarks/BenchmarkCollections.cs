using System;
using System.Collections.Generic;
using Midori.Runtime;
using WarmHeap;

namespace TestGCDeepObject
{

  class TestGCDeepObject
  {

    class RegionLinkedList<T>
    {

      class Node
      {
        public Node Next;
        public Node Prev;
        public T Data;

        public Node(T Val)
        {
          Data = Val;
        }

        public Node(Region region, T Val)
        {
          Data = Val;
        }

      }

      Node Head;

      public RegionLinkedList()
      {
      }

      public RegionLinkedList(Region region)
      {
      }

      public void InsertFirst(T newVal)
      {
        if (Head == null)
          {
            Head = new Node(newVal);
          }
        else
          {
            Node node = new Node(newVal);
            node.Next = Head;
            Head.Prev = node;
            Head = node;
          }
      }

      public void InsertFirst(Region region, T newVal)
      {
        if (Head == null)
          {
            Head = new Node(region, newVal);
          }
        else
          {
            Node node = new Node(region, newVal);
            node.Next = Head;
            Head.Prev = node;
            Head = node;
          }
      }

    }

    class TestClass1
    {
      int m_count;

      public TestClass1(int count)
      {
        m_count = count;
      }

      public TestClass1(Region region, int count)
      {
        m_count = count;
      }

    }

    class DeepListList
    {
      RegionLinkedList<RegionLinkedList<TestClass1>> list;

      public DeepListList(int count)
      {
        list = new RegionLinkedList<RegionLinkedList<TestClass1>>();
        for (int i = 0; i < count; ++i)
          {
            RegionLinkedList<TestClass1> tmpList =
              new RegionLinkedList<TestClass1>();
            for (int j = 0; j < count; ++j)
              {
                tmpList.InsertFirst(new TestClass1(10));
              }
            list.InsertFirst(tmpList);
          }
      }

      public DeepListList(Region region, int count)
      {
        list = new RegionLinkedList<RegionLinkedList<TestClass1>>(region);
        for (int i = 0; i < count; ++i)
          {
            RegionLinkedList<TestClass1> tmpList =
              new RegionLinkedList<TestClass1>(region);
            for (int j = 0; j < count; ++j)
              {
                tmpList.InsertFirst(region, new TestClass1(region, 10));
              }
            list.InsertFirst(region, tmpList);
          }
      }

    }

    class DeepDictionaryList
    {

      RegionLinkedList<Dictionary<int, TestClass1>> list;

      public DeepDictionaryList(int count)
      {
        list = new RegionLinkedList<Dictionary<int, TestClass1>>();
        for (int i = 0; i < count; ++i)
          {
            Dictionary<int, TestClass1> tmpDict =
              new Dictionary<int, TestClass1>();
            for (int j = 0; j < count; ++j)
              {
                tmpDict.Add(j, new TestClass1(10));
              }
            list.InsertFirst(tmpDict);
          }
      }

    }

    class ClassList
    {
      RegionLinkedList<TestClass1> m_test1;

      public ClassList(int count)
      {
        m_test1 = new RegionLinkedList<TestClass1>();
        for (int i = 0; i < count; ++i)
          {
            m_test1.InsertFirst(new TestClass1(10));
          }
      }

      public ClassList(Region region, int count)
      {
        m_test1 = new RegionLinkedList<TestClass1>(region);
        for (int i = 0; i < count; ++i)
          {
            m_test1.InsertFirst(region, new TestClass1(region, 10));
          }
      }

    }

    class ClassDictionary
    {
      Dictionary<int, TestClass1> m_test1 = new Dictionary<int, TestClass1>();

      public ClassDictionary(int count)
      {
        for (int i = 0; i < count; ++i)
          {
            m_test1.Add(i, new TestClass1(10));
          }
      }

    }

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

    static void allocateInRegion(int num_iterations, int num_objects,
                                 string classType)
    {
      if (classType == "dict")
        {
          Console.WriteLine("Can not allocate dict in region.");
          return;
        }
      else if (classType == "list")
        {
          Int64 start = RegionAllocator.NativeGetPerformanceCounter();
          Region region = RegionAllocator.AllocateRegion(10000000);
          DeepListList d_list;
          for (int i = 0; i < num_iterations; i++)
            {
              d_list = new DeepListList(region, num_objects);
            }
          Int64 end = RegionAllocator.NativeGetPerformanceCounter();
          Print(end - start);
        }
      else
        {
          Console.WriteLine("Unexpected class type");
        }
    }

    static void allocateInRegionContext(int num_iterations, int num_objects,
                                        string classType)
    {
      Region region = RegionAllocator.AllocateRegion(10000000);
      if (classType == "dict")
        {
          Int64 start = RegionAllocator.NativeGetPerformanceCounter();
          using (RegionContext regContext = RegionContext.Create(region))
          {
            DeepDictionaryList d_dict;
            for (int i = 0; i < num_iterations; i++)
            {
              d_dict = new DeepDictionaryList(num_objects);
            }
          }
          Int64 end = RegionAllocator.NativeGetPerformanceCounter();
          Print(end - start);
        }
      else if (classType == "list")
        {
          Int64 start = RegionAllocator.NativeGetPerformanceCounter();
          using (RegionContext regContext = RegionContext.Create(region))
          {
            DeepListList d_list;
            for (int i = 0; i < num_iterations; i++)
            {
              d_list = new DeepListList(num_objects);
            }
          }
          Int64 end = RegionAllocator.NativeGetPerformanceCounter();
          Print(end - start);
        }
      else
        {
          Console.WriteLine("Unexpected class type");
        }
    }

    static void allocate(int num_iterations, int num_objects, string classType)
    {
      if (classType == "dict")
        {
          Int64 start = RegionAllocator.NativeGetPerformanceCounter();
          DeepDictionaryList d_dict;
          for (int i = 0; i < num_iterations; i++)
            {
              d_dict = new DeepDictionaryList(num_objects);
            }
          Int64 end = RegionAllocator.NativeGetPerformanceCounter();
          Print(end - start);
        }
      else if (classType == "list")
        {
          Int64 start = RegionAllocator.NativeGetPerformanceCounter();
          DeepListList d_list;
          for (int i = 0; i < num_iterations; i++)
            {
              d_list = new DeepListList(num_objects);
            }
          Int64 end = RegionAllocator.NativeGetPerformanceCounter();
          Print(end - start);
        }
      else
        {
          Console.WriteLine("Unexpected class type");
        }
    }

    static void Main(string[] args)
    {
      RegionAllocator.Initialize();
      HeapWarmer heapWarmer = new HeapWarmer();
      if (args.Length != 4 && args.Length != 5)
        {
          Console.WriteLine("Unexpected number of arguments. Please specify: region|regioncontext|nonregion num_iterations num_objs dict|list");
          return;
        }
      if (args.Length == 5 && args[4] == "warm")
      {
        heapWarmer.WarmHeap(1000);
      }
	System.GC.Collect();
      int num_iterations = Convert.ToInt32(args[1]);
      int num_objects = Convert.ToInt32(args[2]);
      if (args[0] == "region")
        {
          allocateInRegion(num_iterations, num_objects, args[3]);
        }
      else if (args[0] == "regioncontext")
        {
          allocateInRegionContext(num_iterations, num_objects, args[3]);
        }
      else if (args[0] == "nonregion")
        {
          allocate(num_iterations, num_objects, args[3]);
        }
      else
        {
          Console.WriteLine("Unexpected region type");
        }
	
	RegionAllocator.PrintStatistics();

    }

  }

}