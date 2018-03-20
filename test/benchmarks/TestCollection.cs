using Midori.Runtime;
using System;
//using Platform.Collections;
using System.Collections.Generic;
using System.Threading;

namespace GCTraceSpace {

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

  public class RegionLinkedList<T>
  {

    public class Node
    {
      public T Data;
      public Node Next;
      public Node Prev;

      public Node(Region region, T nodeData)
      {
        Data = nodeData;
        Next = null;
        Prev = null;
      }

      public Node(T nodeData)
      {
        Data = nodeData;
        Next = null;
        Prev = null;
      }

    }

    public Node Head;

    public RegionLinkedList()
    {
      Head = null;
    }

    public RegionLinkedList(Region region)
    {
      Head = null;
    }

    public RegionLinkedList(T newHead)
    {
      Head = new Node(newHead);
    }

    public RegionLinkedList(Region region, T newHead)
    {
      Head = new Node(region, newHead);
    }

    public Node GetFirst()
    {
      return Head;
    }

    public void InsertFirst(Region region, T newHead)
    {
      if (Head == null)
      {
        Head = new Node(region, newHead);
      } else
      {
        Node newNode = new Node(region, newHead);
        newNode.Next = Head;
        Head.Prev = newNode;
        Head = newNode;
      }
    }

    public void InsertFirst(T newHead)
    {
      if (Head == null)
      {
        Head = new Node(newHead);
      }
      else
      {
        Node newNode = new Node(newHead);
        newNode.Next = Head;
        Head.Prev = newNode;
        Head = newNode;
      }
    }

    public void Remove(Node node)
    {
      if (Head == null)
      {
        Console.WriteLine("List is empty!");
      }
      if (node == Head)
      {
        Head = Head.Next;
        Head.Prev = null;
      }
      else
      {
        var nextNode = node.Next;
        nextNode.Prev = node.Prev;
        node.Prev.Next = nextNode;
      }
    }

  }


  public class GCTrace
  {
    public static void Main(string[] args) {
      RegionAllocator.Initialize();
      for (int i = 0; i < 50; ++i)
      {
        var region = RegionAllocator.AllocateRegion(100000);
        using (RegionContext regContext = RegionContext.Create(region))
        {
//          List<TestClass> objList = new List<TestClass>();
          RegionLinkedList<TestClass> objList = new RegionLinkedList<TestClass>();
          //Dictionary<int, TestClass> objDict = new Dictionary<int, TestClass>();
          for (int j = 0; j < 2; ++j)
          {
//            objList.Add(new TestClass(region, 10));
            objList.InsertFirst(new TestClass(10));
//            objList.InsertFirst(region, new TestClass(region, 10));
//            objDict.Add(j, new TestClass(10));
          }
        }
        RegionAllocator.FreeRegion(region);
        if (i % 10 == 0)
        {
          GC.Collect();
        }
      }
    }
  }
