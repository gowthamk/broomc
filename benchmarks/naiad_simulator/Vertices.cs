using Midori.Runtime;
using System;
using System.Collections.Generic;

namespace NaiadSimulator
{

  public class SelectVertex<TInput, TTime>
  where TInput : ICloneable
  {
    private readonly Func<TInput, bool> function;

    public SelectVertex(Func<TInput, bool> fun)
    {
      this.function = fun;
    }

    public SelectVertex(Region region, Func<TInput, bool> fun)
    {
      this.function = fun;
    }

    public void onReceive(Message<TTime, TInput> message)
    {
      var output = new Buffer<TTime, TInput>(message.time);
      int outputOffset = 0;

      for (int i = 0; i < message.length; i++)
      {
        if (function(message.payload[i]))
        {
          output.set(outputOffset, message.payload[i]);
          outputOffset++;
        }
      }
      output.send();
    }

    public void onNotify(TTime time)
    {
    }

  }

  public class SelectManyVertex<TInput, TOutput, TTime>
  where TInput : ICloneable
  {
    private readonly Func<TInput, IEnumerable<TOutput>> function;

    public SelectManyVertex(Func<TInput, IEnumerable<TOutput>> function)
    {
      this.function = function;
    }

    public void onReceive(Message<TTime, TInput> message)
    {
      var output = new Buffer<TTime, TOutput>(message.time);
      int outputOffset = 0;

      for (int i = 0; i < message.length; i++)
      {
        foreach (var res in this.function(message.payload[i]))
        {
          output.set(outputOffset, res);
          outputOffset++;
        }
      }
      output.send();
    }

    public void onNotify(TTime time)
    {
    }

  }


  public class GroupByVertex<TInput, TKey, TTime>
  where TInput : ICloneable
  {
    private readonly Func<TInput, TKey> keySelector;
    private readonly Dictionary<TTime, Dictionary<TKey, RegionLinkedList<TInput>>> vals;

    public GroupByVertex(Func<TInput, TKey> keySel)
    {
      keySelector = keySel;
      vals =
        new Dictionary<TTime, Dictionary<TKey, RegionLinkedList<TInput>>>();
    }

    public void onReceive(Message<TTime, TInput> message)
    {
      if (!vals.ContainsKey(message.time))
      {
        vals.Add(message.time,
                 new Dictionary<TKey, RegionLinkedList<TInput>>());
      }
      var dictGroup = vals[message.time];

      for (int i = 0; i < message.length; i++)
      {
        var key = keySelector(message.payload[i]);
        if (!dictGroup.ContainsKey(key))
        {
          dictGroup.Add(key, new RegionLinkedList<TInput>());
        }
        dictGroup[key].InsertFirst(message.payload[i]);
      }
    }

    public void onNotify(TTime time)
    {
      if (vals.ContainsKey(time))
      {
        var output =
          new Buffer<TTime, PairNonCloneable<TKey, RegionLinkedList<TInput>>>(time);
        int outputOffset = 0;
        foreach (var group in vals[time])
        {
          output.set(outputOffset,
                     new PairNonCloneable<TKey, RegionLinkedList<TInput>>(group.Key, group.Value));
          outputOffset++;
        }
        output.send();
        vals.Remove(time);
      }
    }

  }

  public class JoinVertex<TInput1, TInput2, TKey, TOutput, TTime>
  where TInput1 : ICloneable
  where TInput2 : ICloneable
  {
    private readonly Func<TInput1, TKey> keySelector1;
    private readonly Func<TInput2, TKey> keySelector2;
    private readonly Func<TInput1, TInput2, TOutput> resSelector;

    private readonly Dictionary<TTime, Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>> vals;

    public JoinVertex(Func<TInput1, TKey> keySel1,
                      Func<TInput2, TKey> keySel2,
                      Func<TInput1, TInput2, TOutput> resSel)
    {
      keySelector1 = keySel1;
      keySelector2 = keySel2;
      resSelector = resSel;
      vals = new Dictionary<TTime, Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>>(0, new EqualityComparer<TTime>());
    }

    public void onReceive(Message<TTime, TInput1> message)
    {
      var output = new Buffer<TTime, TOutput>(message.time);
      int outputOffset = 0;

      if (!vals.ContainsKey(message.time))
      {
        vals.Add(message.time,
                 new Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>(0, new EqualityComparer<TKey>()));
      }
      Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>> dictKeys =
        vals[message.time];

      for (int i = 0; i < message.length; i++)
      {
        var key = keySelector1(message.payload[i]);
        PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>> currentEntry;
        if (!dictKeys.TryGetValue(key, out currentEntry))
        {
          currentEntry = new PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>(
              new RegionLinkedList<TInput1>(), new RegionLinkedList<TInput2>());
          dictKeys.Add(key, currentEntry);
        }
        currentEntry.First.InsertFirst(message.payload[i]);

        for (Node<TInput2> cur = currentEntry.Second.Head;
             cur != default(Node<TInput2>); cur = cur.Next)
        {
          output.set(outputOffset, resSelector(message.payload[i], cur.Data));
          outputOffset++;
        }
      }
      output.send();
    }

    public void onReceive(Message<TTime, TInput2> message)
    {
      var output = new Buffer<TTime, TOutput>(message.time);
      int outputOffset = 0;

      if (!vals.ContainsKey(message.time))
      {
        vals.Add(message.time,
                 new Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>());
      }
      Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>> dictKeys =
        vals[message.time];

      for (int i = 0; i < message.length; i++)
      {
        var key = keySelector2(message.payload[i]);
        PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>> currentEntry;
        if (!dictKeys.TryGetValue(key, out currentEntry))
        {
          currentEntry = new PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>(
              new RegionLinkedList<TInput1>(), new RegionLinkedList<TInput2>());
          dictKeys.Add(key, currentEntry);
        }
        currentEntry.Second.InsertFirst(message.payload[i]);
        for (Node<TInput1> cur = currentEntry.First.Head;
             cur != default(Node<TInput1>); cur = cur.Next)
        {
          output.set(outputOffset, resSelector(cur.Data, message.payload[i]));
          outputOffset++;
        }
      }
      output.send();
    }

    public void onNotify(TTime time)
    {
      vals.Remove(time);
    }

  }

  public class WindowJoinVertex<TInput1, TInput2, TKey, TOutput>
  where TInput1 : ICloneable
  where TInput2 : ICloneable
  {
    private readonly int windowSize;
    private readonly Func<TInput1, TKey> keySelector1;
    private readonly Func<TInput2, TKey> keySelector2;
    private readonly Func<TInput1, TInput2, TOutput> resSelector;
    private readonly Dictionary<int, Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>> vals;

    public WindowJoinVertex(int windowSize,
                            Func<TInput1, TKey> keySel1,
                            Func<TInput2, TKey> keySel2,
                            Func<TInput1, TInput2, TOutput> resSel)
    {
      this.windowSize = windowSize;
      keySelector1 = keySel1;
      keySelector2 = keySel2;
      resSelector = resSel;
      vals = new Dictionary<int, Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>>();
    }

    public void onReceive(Message<int, TInput1> message)
    {
      var output = new Buffer<int, TOutput>(message.time);
      int outputOffset = 0;

      if (!vals.ContainsKey(message.time))
      {
        vals.Add(message.time,
                 new Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>());
      }
      Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>> dictKeys =
        vals[message.time];

      for (int i = 0; i < message.length; i++)
      {
        var key = keySelector1(message.payload[i]);
        PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>> currentEntry;
        if (!dictKeys.TryGetValue(key, out currentEntry))
        {
          currentEntry = new PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>(
              new RegionLinkedList<TInput1>(), new RegionLinkedList<TInput2>());
          dictKeys[key] = currentEntry;
        }
        currentEntry.First.InsertFirst(message.payload[i]);
        for (int curTime = message.time - windowSize + 1;
             curTime <= message.time; curTime++)
        {
          if (vals.ContainsKey(curTime))
          {
            var matchedDictKeys = vals[curTime];
            if (matchedDictKeys.ContainsKey(key))
            {
              for (Node<TInput2> cur = matchedDictKeys[key].Second.Head;
                   cur != default(Node<TInput2>); cur = cur.Next)
              {
                output.set(outputOffset,
                           resSelector(message.payload[i], cur.Data));
                outputOffset++;
              }
            }
          }
        }
      }
      output.send();
    }

    public void onReceive(Message<int, TInput2> message)
    {
      var output = new Buffer<int, TOutput>(message.time);
      int outputOffset = 0;

      if (!vals.ContainsKey(message.time))
      {
        vals.Add(message.time,
                 new Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>());
      }
      Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>> dictKeys =
        vals[message.time];

      for (int i = 0; i < message.length; i++)
      {
        var key = keySelector2(message.payload[i]);
        PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>> currentEntry;
        if (!dictKeys.TryGetValue(key, out currentEntry))
        {
          currentEntry = new PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>(
              new RegionLinkedList<TInput1>(), new RegionLinkedList<TInput2>());
          dictKeys[key] = currentEntry;
        }
        currentEntry.Second.InsertFirst(message.payload[i]);
        for (int curTime = message.time - windowSize + 1;
             curTime <= message.time; curTime++)
        {
          if (vals.ContainsKey(curTime))
          {
            var matchedDictKeys = vals[curTime];
            if (matchedDictKeys.ContainsKey(key))
            {
              for (Node<TInput1> cur = matchedDictKeys[key].First.Head;
                   cur != default(Node<TInput1>); cur = cur.Next)
              {
                output.set(outputOffset,
                           resSelector(cur.Data, message.payload[i]));
                outputOffset++;
              }
            }
          }
        }
      }
      output.send();
    }

    public void onNotify(int time)
    {
      vals.Remove(time - windowSize);
    }

  }

  public class AggregateVertex<TState, TTime>
  where TState : ICloneable
  {
    private readonly Func<TState, TState, TState> combiner;
    private readonly Dictionary<TTime, Dictionary<Integer, TState>> stateByTime;

    public AggregateVertex(Func<TState, TState, TState> combiner)
    {
      this.combiner = combiner;
      stateByTime = new Dictionary<TTime, Dictionary<Integer, TState>>();
    }

    public void onReceive(Message<TTime, Pair<Integer, TState>> message)
    {
      if (!stateByTime.ContainsKey(message.time))
      {
        stateByTime.Add(message.time,
                        new Dictionary<Integer, TState>(0, new IntegerEqualityComparer()));
      }
      var states = stateByTime[message.time];

      for (int i = 0; i < message.length; i++)
      {
        Integer key = message.payload[i].First;
        if (!states.ContainsKey(key))
        {
          states.Add(key, message.payload[i].Second);
        }
        else
        {
          states[key] = combiner(states[key], message.payload[i].Second);
        }
      }
    }

    public void onNotify(TTime time)
    {
      if (stateByTime.ContainsKey(time))
      {
        var output = new Buffer<TTime, Pair<Integer, TState>>(time);
        int outputOffset = 0;
        foreach (var pair in stateByTime[time])
        {
          output.set(outputOffset,
                     new Pair<Integer, TState>(pair.Key, pair.Value));
          outputOffset++;
        }
        output.send();
        stateByTime.Remove(time);
      }
    }
  }

  // Commented out because of missing HashSet. It's located under Platform and
  // not Collections.Generic
  // public class DistinctVertex<TRecord, TTime>
  // {
  //   private readonly Dictionary<TTime, HashSet<TRecord>> vals;

  //   public DistinctVertex()
  //   {
  //     vals = new Dictionary<TTime, HashSet<TRecord>>();
  //   }

  //   public void onReceive(Message<TTime, TRecord> message)
  //   {
  //     var output = new Buffer<TTime, TRecord>(message.time);
  //     int outputOffset = 0;
  //     if (!vals.ContainsKey(message.time))
  //     {
  //       vals.Add(message.time, new HashSet<TRecord>());
  //     }
  //     var currentSet = vals[message.time];
  //     for (int i = 0; i < message.length; i++)
  //     {
  //       if (currentSet.Add(message.payload[i]))
  //       {
  //         output.set(outputOffset, message.payload[i]);
  //       }
  //     }
  //     output.send();
  //   }

  //   public void onNotify(TTime time)
  //   {
  //     vals.Remove(time);
  //   }

  // }

}
