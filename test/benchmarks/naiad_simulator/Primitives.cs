//#define DEBUG

using Midori.Runtime;
using System;
using System.Collections.Generic;

namespace NaiadSimulator
{

  public class Pair<TFirst, TSecond> : IEquatable<Pair<TFirst, TSecond>>, ICloneable
    where TFirst : ICloneable
    where TSecond : ICloneable
  {
    public TFirst First;
    public TSecond Second;

    public Pair(TFirst first, TSecond second)
    {
      this.First = first;
      this.Second = second;
    }

    public Pair(Region region, TFirst first, TSecond second)
    {
      this.First = first;
      this.Second = second;
    }

    public override string ToString()
    {
      return "[" + First.ToString() + " " + Second.ToString() + ")";
    }

    public bool Equals(Pair<TFirst, TSecond> other)
    {
      return First.Equals(other.First) && Second.Equals(other.Second);
    }

    public object Clone()
    {
      return new Pair<TFirst, TSecond>((TFirst)First.Clone(), (TSecond)Second.Clone());
    }

    public override int GetHashCode()
    {
      return First.GetHashCode() * 17 + Second.GetHashCode();
    }

  }

  public class PairNonCloneable<TFirst, TSecond> : IEquatable<PairNonCloneable<TFirst, TSecond>>
  {
    public TFirst First;
    public TSecond Second;

    public PairNonCloneable(TFirst first, TSecond second)
    {
      this.First = first;
      this.Second = second;
    }

    public PairNonCloneable(Region region, TFirst first, TSecond second)
    {
      this.First = first;
      this.Second = second;
    }

    public override string ToString()
    {
      return "[" + First.ToString() + " " + Second.ToString() + ")";
    }

    public bool Equals(PairNonCloneable<TFirst, TSecond> other)
    {
      return First.Equals(other.First) && Second.Equals(other.Second);
    }

    public override int GetHashCode()
    {
      return First.GetHashCode() * 17 + Second.GetHashCode();
    }

  }

  class Node<T>
  {
    public Node<T> Next;
    public Node<T> Prev;
    public T Data;

    public Node(T Val)
    {
      Data = Val;
    }

    public Node(Region region, T Val)
    {
      Data = Val;
    }

    public override string ToString()
    {
      return Data.ToString();
    }

  }

  class RegionLinkedList<T>
  {

    public Node<T> Head;

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
        Head = new Node<T>(newVal);
      }
      else
      {
        Node<T> node = new Node<T>(newVal);
        node.Next = Head;
        Head.Prev = node;
        Head = node;
      }
    }

    public void InsertFirst(Region region, T newVal)
    {
      if (Head == null)
      {
        Head = new Node<T>(region, newVal);
      }
      else
      {
        Node<T> node = new Node<T>(region, newVal);
        node.Next = Head;
        Head.Prev = node;
        Head = node;
      }
    }

    public override string ToString()
    {
      string res = "List ";
      for (Node<T> cur = Head; cur != default(Node<T>); cur = cur.Next)
      {
        res += cur + "\n";
      }
      return res;
    }

  }

  public class Buffer<TTime, TRecord>
  {

    private const int DEFAULT_BUFFER_SIZE = 1500;

    internal TRecord[] memory_block;
    internal int size;
    internal TTime id;

    public Buffer(TTime id, int size)
    {
      this.memory_block = new TRecord[size];
      this.size = size;
      this.id = id;
    }

    public Buffer(Region region, TTime id, int size)
    {
      this.memory_block = new TRecord[size];
      this.size = size;
      this.id = id;
    }

    public Buffer(TTime id)
    {
      this.memory_block = new TRecord[DEFAULT_BUFFER_SIZE];
      this.size = DEFAULT_BUFFER_SIZE;
      this.id = id;
    }

    public Buffer(Region region, TTime id)
    {
      this.memory_block = new TRecord[DEFAULT_BUFFER_SIZE];
      this.size = DEFAULT_BUFFER_SIZE;
      this.id = id;
    }

    public void set(int offset, TRecord value)
    {
      this.memory_block[offset % size] = value;
    }

    public TRecord get(int offset)
    {
      return this.memory_block[offset % size];
    }

    public void print()
    {
      for (int i = 0; i < size; i++)
      {
        if (memory_block[i] != null)
        {
          Console.WriteLine("-- " + memory_block[i]);
        }
      }
    }

    public void send()
    {
      // Message<TTime, TRecord> msg = new Message<TTime, TRecord>(id, size);
      // for (int i = 0; i < size; i++)
      // {
      //   msg.put(memory_block[i]);
      // }
#if DEBUG
      print();
      Console.WriteLine("DONE");
#endif
    }

    // The region will be freed.
    public void send(Region region)
    {
      // Message<TTime, TRecord> msg =
      //   new Message<TTime, TRecord>(region, id, size);
      // for (int i = 0; i < size; i++)
      // {
      //   msg.put(memory_block[i]);
      // }
#if DEBUG
      print();
      Console.WriteLine("DONE");
#endif
     RegionAllocator.FreeRegion(region);
    }
  }

  public class Message<TTime, TRecord> : ICloneable
  where TRecord : ICloneable
  {
    public const int DEFAULT_MESSAGE_LENGTH = 25;

    public TTime time;
    public TRecord[] payload;
    public int length;
    internal int offset;

    public Message(TTime time)
    {
      this.time = time;
      this.payload = new TRecord[DEFAULT_MESSAGE_LENGTH];
      this.length = DEFAULT_MESSAGE_LENGTH;
      this.offset = 0;
    }

    public Message(Region region, TTime time)
    {
      this.time = time;
      this.payload = new TRecord[DEFAULT_MESSAGE_LENGTH];
      this.length = DEFAULT_MESSAGE_LENGTH;
      this.offset = 0;
    }

    public Message(TTime time, int length)
    {
      this.time = time;
      this.payload = new TRecord[length];
      this.length = length;
      this.offset = 0;
    }

    public Message(Region region, TTime time, int length)
    {
      this.time = time;
      this.payload = new TRecord[length];
      this.length = length;
      this.offset = 0;
    }

    public void put(TRecord record)
    {
      if (offset < length)
      {
        payload[offset++] = record;
      }
      else
      {
        Console.Error.WriteLine("The message buffer is overloaded!");
      }
    }

    public object Clone()
    {
      Message<TTime, TRecord> msg = new Message<TTime, TRecord>(time, length);
      for (int i = 0; i < offset; ++i)
      {
        msg.put((TRecord)payload[i].Clone());
      }
      return msg;
    }

  }

  public class IntegerEqualityComparer : IEqualityComparer<Integer>
  {

    public bool Equals(Integer t1, Integer t2)
    {
      return t1.Equals(t2);
    }

    public int GetHashCode(Integer t)
    {
      return t.GetHashCode();
    }

  }

  public class EqualityComparer<TState> : IEqualityComparer<TState>
  {

    public bool Equals(TState t1, TState t2)
    {
      return t1.Equals(t2);
    }

    public int GetHashCode(TState t)
    {
      return t.GetHashCode();
    }

  }

  public class Integer : IEquatable<Integer>, ICloneable
  {
    public int value = 0;

    public Integer(int value)
    {
      this.value = value;
    }

    public static implicit operator Integer(int value)
    {
      return new Integer(value);
    }

    public static implicit operator int(Integer integer)
    {
      return integer.value;
    }

    public static Integer operator +(Integer one, Integer two)
    {
      return new Integer(one.value + two.value);
    }

    public static Integer operator +(int one, Integer two)
    {
      return new Integer(one + two);
    }

    public static int operator -(Integer one, Integer two)
    {
      return one.value - two.value;
    }

    public static Integer operator -(int one, Integer two)
    {
      return new Integer(one - two);
    }

    public object Clone()
    {
      return new Integer(value);
    }

    public bool Equals(Integer other)
    {
      return value == other.value;
    }

    public override int GetHashCode()
    {
      return value;
    }

    public override string ToString()
    {
      return value.ToString();
    }

  }

  public class Initializer
  {

    public static void InitializeStatics()
    {
      Convert.ToInt32("42");
      Console.Write("");
      new Dictionary<int, int>();
    }

  }

  public class Utils
  {

    public static void Print(Int64 cycles)
    {
      Int64 frequency = RegionAllocator.NativeGetPerformanceFrequency();
      Console.WriteLine("Cycles: {0}", cycles);
      cycles *= 1000000000;
      double duration = (double)cycles / (double)frequency;
      Console.WriteLine("Time(ns): {0}", duration);
    }

    public static void Print(double cycles)
    {
      Int64 frequency = RegionAllocator.NativeGetPerformanceFrequency();
      Console.WriteLine("Cycles: {0}", cycles);
      cycles *= 1000000000.0;
      double duration = cycles / (double)frequency;
      Console.WriteLine("Time(ns): {0}", duration);
    }

  }

}