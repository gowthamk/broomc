using System;
using System.Collections.Generic;

namespace TestGCDeepObject
{

  class TestGCDeepObjectCSharp
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

      }

      Node Head;

      public RegionLinkedList()
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

    }

    class TestClass1
    {
      int m_count;

      public TestClass1(int count)
      {
        m_count = count;
      }
    }

    class DeepListList
    {
      RegionLinkedList<RegionLinkedList<TestClass1>> list =
        new RegionLinkedList<RegionLinkedList<TestClass1>>();

      public DeepListList(int count)
      {
        for (int i = 0; i < count; ++i)
          {
            RegionLinkedList<TestClass1> tmpList = new RegionLinkedList<TestClass1>();
            for (int j = 0; j < count; ++j)
              {
                tmpList.InsertFirst(new TestClass1(10));
              }
            list.InsertFirst(tmpList);
          }
      }

    }

    class DeepDictionaryList
    {

      RegionLinkedList<Dictionary<int, TestClass1>> list =
        new RegionLinkedList<Dictionary<int, TestClass1>>();

      public DeepDictionaryList(int count)
      {
        for (int i = 0; i < count; ++i)
          {
            Dictionary<int, TestClass1> tmpDict = new Dictionary<int, TestClass1>();
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
      RegionLinkedList<TestClass1> m_test1 = new RegionLinkedList<TestClass1>();

      public ClassList(int count)
      {
        for (int i = 0; i < count; ++i)
          {
            m_test1.InsertFirst(new TestClass1(10));
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

    static void Main(string[] args)
    {
      if (args.Length != 3)
        {
          Console.WriteLine("Please specify: num_iterations num_objs dict|list");
          return;
        }
      int num_iterations = Convert.ToInt32(args[0]);
      int num_objects = Convert.ToInt32(args[1]);
      if (args[2] == "dict")
        {
          DeepDictionaryList d_dict;
          for (int i = 0; i < num_iterations; i++)
            {
              d_dict = new DeepDictionaryList(num_objects);
            }
        }
      else if (args[2] == "list")
        {
          DeepListList d_list;
          for (int i = 0; i < num_iterations; i++)
            {
              d_list = new DeepListList(num_objects);
            }
        }
      else
        {
          Console.WriteLine("Unexpected class type");
        }
    }

  }

}