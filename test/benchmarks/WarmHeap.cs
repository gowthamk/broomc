using Midori.Runtime;
using System;
using System.Collections.Generic;
using System.Threading;


namespace WarmHeap
{

  public class RegionLinkedList<T>
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

  public class ElementClass
  {
    string m_message;

    public ElementClass(string message)
    {
      m_message = message;
    }

    public ElementClass(Region region, string message)
    {
      m_message = message;
    }

  }


  public class HeapWarmer
  {

    public RegionLinkedList<ElementClass> WarmHeap(uint num_objects)
    {
      RegionLinkedList<ElementClass> list =
        new RegionLinkedList<ElementClass>();
      for (int i = 0; i < num_objects; i++)
      {
        list.InsertFirst(new ElementClass("thisisalargestringit'sbigdata"));
      }
      return list;
    }

    public RegionLinkedList<RegionLinkedList<ElementClass>> WarmHeap(uint num_lists, uint num_objects)
    {
      RegionLinkedList<RegionLinkedList<ElementClass>> list =
        new RegionLinkedList<RegionLinkedList<ElementClass>>();
      for (int i = 0; i < num_lists; ++i)
      {
        RegionLinkedList<ElementClass> inner_list =
          new RegionLinkedList<ElementClass>();
        for (int j = 0; j < num_lists; ++j)
        {
          inner_list.InsertFirst(new ElementClass("thisisalargestringit'sbigdata"));
        }
        list.InsertFirst(inner_list);
      }
      return list;
    }
  }

}