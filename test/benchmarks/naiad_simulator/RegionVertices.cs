using Midori.Runtime;
using System;
using System.Collections.Generic;

namespace NaiadSimulator
{

  public class RegionJoinVertex<TInput1, TInput2, TKey, TOutput, TTime>
  where TInput1 : ICloneable
  where TInput2 : ICloneable
  {
    private readonly Func<TInput1, TKey> keySelector1;
    private readonly Func<TInput2, TKey> keySelector2;
    private readonly Func<TInput1, TInput2, TOutput> resSelector;

    private readonly Dictionary<TTime, Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>> vals;

    private readonly Dictionary<TTime, Region> dictRegions;

    public RegionJoinVertex(Func<TInput1, TKey> keySel1,
                            Func<TInput2, TKey> keySel2,
                            Func<TInput1, TInput2, TOutput> resSel)
    {
      keySelector1 = keySel1;
      keySelector2 = keySel2;
      resSelector = resSel;
      vals = new Dictionary<TTime, Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>>(0, new EqualityComparer<TTime>());
      dictRegions =
        new Dictionary<TTime, Region>(0, new EqualityComparer<TTime>());
    }

    public void onReceive(Message<TTime, TInput1> message, Region actorRegion)
    {
      Region outputRegion =
        RegionAllocator.AllocateRegion(NaiadSimulator.OUTPUT_REGION_SIZE);
      var output = new Buffer<TTime, TOutput>(outputRegion, message.time);
      int outputOffset = 0;

      Region dictRegion;
      if (!vals.ContainsKey(message.time))
      {
        Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>> dictKeyPair;
        dictRegion =
          RegionAllocator.AllocateRegion(NaiadSimulator.TMP_REGION_SIZE);
        using (RegionContext regContext = RegionContext.Create(dictRegion))
        {
          dictKeyPair =
            new Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>(0, new EqualityComparer<TKey>());
        }
        using (RegionContext regContext = RegionContext.Create(actorRegion))
        {
          vals.Add(message.time, dictKeyPair);
          dictRegions.Add(message.time, dictRegion);
        }
      }
      Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>> dictKeys =
        vals[message.time];
      dictRegion = dictRegions[message.time];

      PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>> currentEntry;
      TInput1 curInput;
      using (RegionContext regContext = RegionContext.Create(dictRegion))
      {
        for (int i = 0; i < message.length; i++)
        {
          // Note: Automatically annotated code may require this Clone, but
          // in the manually written one we don't need it.
          // curInput = (TInput1)message.payload[i].Clone();
          curInput = (TInput1)message.payload[i];
          var key = keySelector1(curInput);
          if (!dictKeys.TryGetValue(key, out currentEntry))
          {
            currentEntry = new PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>(
                new RegionLinkedList<TInput1>(), new RegionLinkedList<TInput2>());
            dictKeys.Add(key, currentEntry);
          }
          currentEntry.First.InsertFirst(curInput);

//          using (RegionContext context = RegionContext.Create(outputRegion))
          {
            for (Node<TInput2> cur = currentEntry.Second.Head;
                 cur != default(Node<TInput2>); cur = cur.Next)
            {
              output.set(outputOffset, resSelector(curInput, cur.Data));
              outputOffset++;
            }
          }
        }
      }
      // The region will be freed.
      output.send(outputRegion);
    }

    public void onReceive(Message<TTime, TInput2> message, Region actorRegion)
    {
      Region outputRegion =
        RegionAllocator.AllocateRegion(NaiadSimulator.OUTPUT_REGION_SIZE);
      var output = new Buffer<TTime, TOutput>(outputRegion, message.time);
      int outputOffset = 0;

      Region dictRegion;
      if (!vals.ContainsKey(message.time))
      {
        Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>> dictKeyPair;
        dictRegion =
          RegionAllocator.AllocateRegion(NaiadSimulator.TMP_REGION_SIZE);
        using (RegionContext regContext = RegionContext.Create(dictRegion))
        {
          dictKeyPair =
            new Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>(0, new EqualityComparer<TKey>());
        }
        using (RegionContext regContext = RegionContext.Create(actorRegion))
        {
          vals.Add(message.time, dictKeyPair);
          dictRegions.Add(message.time, dictRegion);
        }
      }
      Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>> dictKeys =
        vals[message.time];
      dictRegion = dictRegions[message.time];

      PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>> currentEntry;
      TInput2 curInput;
      using (RegionContext regContext = RegionContext.Create(dictRegion))
      {

        for (int i = 0; i < message.length; i++)
        {
          // Note: Automatically annotated code may require this Clone, but
          // in the manually written one we don't need it.
          // curInput = (TInput2)message.payload[i].Clone();
          curInput = (TInput2)message.payload[i];
          var key = keySelector2(curInput);
          if (!dictKeys.TryGetValue(key, out currentEntry))
          {
            currentEntry = new PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>(
                new RegionLinkedList<TInput1>(), new RegionLinkedList<TInput2>());
            dictKeys.Add(key, currentEntry);
          }
          currentEntry.Second.InsertFirst(curInput);

//          using (RegionContext context = RegionContext.Create(outputRegion))
          {
            for (Node<TInput1> cur = currentEntry.First.Head;
                 cur != default(Node<TInput1>); cur = cur.Next)
            {
              output.set(outputOffset, resSelector(cur.Data, curInput));
              outputOffset++;
            }
          }
        }
      }
      // The region will be freed.
      output.send(outputRegion);
    }

    public void onNotify(TTime time)
    {
      vals.Remove(time);
      Region region = dictRegions[time];
      dictRegions.Remove(time);
      RegionAllocator.FreeRegion(region);
    }

  }

  public class RegionWindowJoinVertex<TInput1, TInput2, TKey, TOutput>
  where TInput1 : ICloneable
  where TInput2 : ICloneable
  {
    private readonly int windowSize;
    private readonly Func<TInput1, TKey> keySelector1;
    private readonly Func<TInput2, TKey> keySelector2;
    private readonly Func<TInput1, TInput2, TOutput> resSelector;
    private readonly Dictionary<int, Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>> vals;

    private readonly Dictionary<int, Region> dictRegions;

    public RegionWindowJoinVertex(int windowSize,
                                  Func<TInput1, TKey> keySel1,
                                  Func<TInput2, TKey> keySel2,
                                  Func<TInput1, TInput2, TOutput> resSel)
    {
      this.windowSize = windowSize;
      keySelector1 = keySel1;
      keySelector2 = keySel2;
      resSelector = resSel;
      vals = new Dictionary<int, Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>>();
      dictRegions = new Dictionary<int, Region>();
    }

    public void onReceive(Message<int, TInput1> message, Region actorRegion)
    {
      Region outputRegion =
        RegionAllocator.AllocateRegion(NaiadSimulator.OUTPUT_REGION_SIZE);
      var output = new Buffer<int, TOutput>(outputRegion, message.time);
      int outputOffset = 0;

      Region dictRegion;
      if (!vals.ContainsKey(message.time))
      {
        Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>> dictKeyPair;
        dictRegion = RegionAllocator.AllocateRegion(NaiadSimulator.TMP_REGION_SIZE);
        using (RegionContext regContext = RegionContext.Create(dictRegion))
        {
          dictKeyPair = new Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>();
        }
        using (RegionContext regContext = RegionContext.Create(actorRegion))
        {
          vals.Add(message.time, dictKeyPair);
          dictRegions.Add(message.time, dictRegion);
        }
      }
      Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>> dictKeys =
        vals[message.time];
      dictRegion = dictRegions[message.time];

      PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>> currentEntry;
      TInput1 curInput;
      using (RegionContext regContext = RegionContext.Create(dictRegion))
      {
        for (int i = 0; i < message.length; i++)
        {
          var key = keySelector1(message.payload[i]);
          if (!dictKeys.TryGetValue(key, out currentEntry))
          {
            currentEntry = new PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>(
                new RegionLinkedList<TInput1>(), new RegionLinkedList<TInput2>());
            dictKeys[key] = currentEntry;
          }
          curInput = (TInput1)message.payload[i];
          // Note: Automatically annotated code may require this Clone, but
          // in the manually written one we don't need it.
          // curInput = (TInput1)message.payload[i].Clone();
          currentEntry.First.InsertFirst(curInput);

//          using (RegionContext context = RegionContext.Create(outputRegion))
          {
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
                    output.set(outputOffset, resSelector(curInput, cur.Data));
                    outputOffset++;
                  }
                }
              }
            }
          }
        }
      }
      // The region will be freed.
      output.send(outputRegion);
    }

    public void onReceive(Message<int, TInput2> message, Region actorRegion)
    {
      Region outputRegion =
        RegionAllocator.AllocateRegion(NaiadSimulator.OUTPUT_REGION_SIZE);
      var output = new Buffer<int, TOutput>(outputRegion, message.time);
      int outputOffset = 0;

      Region dictRegion;
      if (!vals.ContainsKey(message.time))
      {
        Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>> dictKeyPair;
        dictRegion = RegionAllocator.AllocateRegion(NaiadSimulator.TMP_REGION_SIZE);
        using (RegionContext regContext = RegionContext.Create(dictRegion))
        {
          dictKeyPair = new Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>>();
        }
        using (RegionContext regContext = RegionContext.Create(actorRegion))
        {
          vals.Add(message.time, dictKeyPair);
          dictRegions.Add(message.time, dictRegion);
        }
      }
      Dictionary<TKey, PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>> dictKeys =
        vals[message.time];
      dictRegion = dictRegions[message.time];

      PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>> currentEntry;
      TInput2 curInput;
      using (RegionContext regContext = RegionContext.Create(dictRegion))
      {
        for (int i = 0; i < message.length; i++)
        {
          var key = keySelector2(message.payload[i]);
          if (!dictKeys.TryGetValue(key, out currentEntry))
          {
            currentEntry = new PairNonCloneable<RegionLinkedList<TInput1>, RegionLinkedList<TInput2>>(
                new RegionLinkedList<TInput1>(), new RegionLinkedList<TInput2>());
            dictKeys[key] = currentEntry;
          }
          curInput = (TInput2)message.payload[i];
          // Note: Automatically annotated code may require this Clone, but
          // in the manually written one we don't need it.
          // curInput = (TInput2)message.payload[i].Clone();
          currentEntry.Second.InsertFirst(curInput);

//          using (RegionContext context = RegionContext.Create(outputRegion))
          {
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
                    output.set(outputOffset, resSelector(cur.Data, curInput));
                    outputOffset++;
                  }
                }
              }
            }
          }
        }
      }
      // The region will be freed.
      output.send(outputRegion);
    }

    public void onNotify(int time)
    {
      // TODO(ionel): This doesn't free the last windowSize regions.
      if (time - windowSize >= 0)
      {
        vals.Remove(time - windowSize);
        Region region = dictRegions[time - windowSize];
        dictRegions.Remove(time - windowSize);
        RegionAllocator.FreeRegion(region);
      }
    }

  }

  public class RegionAggregateVertex<TState, TTime>
  where TState : ICloneable
  {
    private readonly Func<TState, TState, TState> combiner;
    private readonly Dictionary<TTime, Dictionary<Integer, TState>> stateByTime;

    private readonly Dictionary<TTime, Region> dictRegions;

    public RegionAggregateVertex(Func<TState, TState, TState> combiner)
    {
      this.combiner = combiner;
      stateByTime = new Dictionary<TTime, Dictionary<Integer, TState>>();
      dictRegions = new Dictionary<TTime, Region>();
    }

    public void onReceive(Message<TTime, Pair<Integer, TState>> message,
                          Region actorRegion)
    {
      Region dictRegion;
      if (!stateByTime.ContainsKey(message.time))
      {
        dictRegion =
          RegionAllocator.AllocateRegion(NaiadSimulator.TMP_REGION_SIZE);
        Dictionary<Integer, TState> dictKeyState;
        using (RegionContext regContext = RegionContext.Create(dictRegion))
        {
          dictKeyState =
            new Dictionary<Integer, TState>(0, new IntegerEqualityComparer());
        }
        using (RegionContext regContext = RegionContext.Create(actorRegion))
        {
          stateByTime.Add(message.time, dictKeyState);
          dictRegions.Add(message.time, dictRegion);
        }
      }
      var states = stateByTime[message.time];
      dictRegion = dictRegions[message.time];

      using (RegionContext regContext = RegionContext.Create(dictRegion))
      {
        Pair<Integer, TState> inputPair;
        for (int i = 0; i < message.length; i++)
        {
          inputPair = (Pair<Integer, TState>)message.payload[i].Clone();
          var key = inputPair.First;

          if (!states.ContainsKey(key))
          {
            states.Add(key, inputPair.Second);
          }
          else
          {
            states[key] = combiner(states[key], inputPair.Second);
          }
        }
      }
    }

    public void onNotify(TTime time)
    {
      if (stateByTime.ContainsKey(time))
      {
        Region outputRegion =
          RegionAllocator.AllocateRegion(NaiadSimulator.OUTPUT_REGION_SIZE);
        var output =
          new Buffer<TTime, Pair<Integer, TState>>(outputRegion, time);
        int outputOffset = 0;
        using (RegionContext regContext = RegionContext.Create(outputRegion))
        {
          foreach (var pair in stateByTime[time])
          {
            // Note: Automatically annotated code may require this Clone, but
            // in the manually written one we don't need it.
            // output.set(outputOffset,
            //            new Pair<Integer, TState>((Integer)pair.Key.Clone(),
            //                                      (TState)pair.Value.Clone()));
            output.set(outputOffset,
                       new Pair<Integer, TState>(pair.Key, pair.Value));
            outputOffset++;
          }
        }
        // The region will be freed.
        output.send(outputRegion);
        stateByTime.Remove(time);
        Region dictRegion = dictRegions[time];
        dictRegions.Remove(time);
        RegionAllocator.FreeRegion(dictRegion);
      }
    }
  }

  public class RegionSelectVertex<TInput, TTime>
  where TInput : ICloneable
  {
    private readonly Func<TInput, bool> function;

    public RegionSelectVertex(Func<TInput, bool> fun)
    {
      this.function = fun;
    }

    public RegionSelectVertex(Region region, Func<TInput, bool> fun)
    {
      this.function = fun;
    }

    public void onReceive(Message<TTime, TInput> message)
    {
      Region outputRegion =
        RegionAllocator.AllocateRegion(NaiadSimulator.OUTPUT_REGION_SIZE);
      var output = new Buffer<TTime, TInput>(outputRegion, message.time);
      int outputOffset = 0;

      using (RegionContext regContext = RegionContext.Create(outputRegion))
      {
        for (int i = 0; i < message.length; i++)
        {
          if (function(message.payload[i]))
          {
            // Note: Automatically annotated code may require this Clone, but
            // in the manually written one we don't need it.
            // output.set(outputOffset, (TInput)message.payload[i].Clone());
            output.set(outputOffset, message.payload[i]);
            outputOffset++;
          }
        }
      }
      // The region will be freed.
      output.send(outputRegion);
    }

    public void onNotify(TTime time)
    {
    }

  }

  public class RegionSelectManyVertex<TInput, TOutput, TTime>
  where TInput : ICloneable
  {
    private readonly Func<TInput, IEnumerable<TOutput>> function;

    public RegionSelectManyVertex(Func<TInput, IEnumerable<TOutput>> function)
    {
      this.function = function;
    }

    public void onReceive(Message<TTime, TInput> message)
    {
      Region outputRegion =
        RegionAllocator.AllocateRegion(NaiadSimulator.OUTPUT_REGION_SIZE);
      var output = new Buffer<TTime, TOutput>(outputRegion, message.time);
      int outputOffset = 0;

      TInput input;
      using (RegionContext regContext = RegionContext.Create(outputRegion))
      {
        for (int i = 0; i < message.length; i++)
        {
          // Note: Automatically annotated code may require this Clone, but
          // in the manually written one we don't need it.
          // input = (TInput)message.payload[i].Clone();
          input = message.payload[i];
          foreach (var res in this.function(input))
          {
            output.set(outputOffset, res);
            outputOffset++;
          }
        }
      }
      // The region will be freed.
      output.send(outputRegion);
    }

    public void onNotify(TTime time)
    {
    }

  }

  public class RegionGroupByVertex<TInput, TKey, TTime>
  where TInput : ICloneable
  {
    private readonly Func<TInput, TKey> keySelector;
    private readonly Dictionary<TTime, Dictionary<TKey, RegionLinkedList<TInput>>> vals;

    private readonly Dictionary<TTime, Region> dictRegions;

    public RegionGroupByVertex(Func<TInput, TKey> keySel)
    {
      keySelector = keySel;
      vals = new Dictionary<TTime, Dictionary<TKey, RegionLinkedList<TInput>>>();
      dictRegions = new Dictionary<TTime, Region>();
    }

    public void onReceive(Message<TTime, TInput> message, Region actorRegion)
    {
      Region dictRegion;
      if (!vals.ContainsKey(message.time))
      {
        Dictionary<TKey, RegionLinkedList<TInput>> dictKeyList;
        dictRegion =
          RegionAllocator.AllocateRegion(NaiadSimulator.TMP_REGION_SIZE);
        using (RegionContext regContext = RegionContext.Create(dictRegion))
        {
          dictKeyList = new Dictionary<TKey, RegionLinkedList<TInput>>();
        }
        using (RegionContext regContext = RegionContext.Create(actorRegion))
        {
          vals.Add(message.time, dictKeyList);
          dictRegions.Add(message.time, dictRegion);
        }
      }
      var dictGroup = vals[message.time];
      dictRegion = dictRegions[message.time];

      using (RegionContext regContext = RegionContext.Create(dictRegion))
      {
        TInput input;
        for (int i = 0; i < message.length; i++)
        {
          input = (TInput)message.payload[i].Clone();
          var key = keySelector(input);
          if (!dictGroup.ContainsKey(key))
          {
            dictGroup.Add(key, new RegionLinkedList<TInput>());
          }
          dictGroup[key].InsertFirst(input);
        }
      }
    }

    public void onNotify(TTime time)
    {
      if (vals.ContainsKey(time))
      {
        Region outputRegion =
          RegionAllocator.AllocateRegion(NaiadSimulator.OUTPUT_REGION_SIZE);
        var output =
          new Buffer<TTime, PairNonCloneable<TKey, RegionLinkedList<TInput>>>(outputRegion, time);
        int outputOffset = 0;
        using (RegionContext regContext = RegionContext.Create(outputRegion))
        {
          foreach (var group in vals[time])
          {
            // NOTE: In real Naiad we might have to copy the pair.
            output.set(outputOffset,
                       new PairNonCloneable<TKey, RegionLinkedList<TInput>>(group.Key, group.Value));
            outputOffset++;
          }
        }
        output.send(outputRegion);
        vals.Remove(time);
        Region dictRegion = dictRegions[time];
        dictRegions.Remove(time);
        RegionAllocator.FreeRegion(dictRegion);
      }
    }

  }

}
