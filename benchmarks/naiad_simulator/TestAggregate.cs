using Midori.Runtime;
using System;
using System.IO;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace NaiadSimulator
{

  public class TestAggregate : TestInterface
  {

    public void Execute(string[] args)
    {
      AggregateVertex<AggDocument, int> aggregateVertex =
        new AggregateVertex<AggDocument, int>(
            (accDoc, newDoc) => new AggDocument(accDoc.docId,
                                                accDoc.length + newDoc.length,
                                                accDoc.authorId, accDoc.title));
      PlayData(aggregateVertex, args[0]);
      // PlayDataFromMemory(aggregateVertex, args[0]);
    }

    public void PlayData(AggregateVertex<AggDocument, int> aggVertex,
                         string filename)
    {
      if (File.Exists(filename))
      {
        using (StreamReader file = File.OpenText(filename))
        {
          while (true)
          {
            var line = file.ReadLine();
            if (line == null)
            {
              break;
            }
            var elements = line.Split(' ');
            if (elements[0] == "BEGIN")
            {
              int time = Convert.ToInt32(elements[1]);
              int batch_size = Convert.ToInt32(elements[2]);
              Message<int, Pair<Integer, AggDocument>> msg =
                new Message<int, Pair<Integer, AggDocument>>(time, batch_size);
              for (int i = 0; i < batch_size; i++)
              {
                elements = file.ReadLine().Split(' ');
                msg.put(new Pair<Integer, AggDocument>(
                    new Integer(Convert.ToInt32(elements[3])),
                    new AggDocument(new Integer(Convert.ToInt32(elements[1])),
                                    new Integer(Convert.ToInt32(elements[2])),
                                    new Integer(Convert.ToInt32(elements[3])),
                                    elements[4])));
              }
              aggVertex.onReceive(msg);
            }
            else if (elements[0] == "END")
            {
              aggVertex.onNotify(Convert.ToInt32(elements[1]));
            }
          }
        }
      }
      else
      {
        Console.WriteLine("Input file does not exist");
      }
    }

    public void PlayDataFromMemory(AggregateVertex<AggDocument, int> aggVertex,
                                   string filename)
    {
      int time_epoch = 0;
      RegionLinkedList<Message<int, Pair<Integer, AggDocument>>> messages;
      using (StreamReader file = File.OpenText(filename))
      {
        Region inputRegion =
          RegionAllocator.AllocateRegion(NaiadSimulator.TMP_REGION_SIZE);
        using (RegionContext regContext = RegionContext.Create(inputRegion))
        {
          messages =
            new RegionLinkedList<Message<int, Pair<Integer, AggDocument>>>();
          while (true)
          {
            var line = file.ReadLine();
            if (line == null)
            {
              break;
            }
            var elements = line.Split(' ');
            if (elements[0] == "BEGIN")
            {
              int time = Convert.ToInt32(elements[1]);
              int batch_size = Convert.ToInt32(elements[2]);
              Message<int, Pair<Integer, AggDocument>> msg =
                new Message<int, Pair<Integer, AggDocument>>(time, batch_size);
              for (int i = 0; i < batch_size; i++)
              {
                elements = file.ReadLine().Split(' ');
                msg.put(new Pair<Integer, AggDocument>(
                    new Integer(Convert.ToInt32(elements[3])),
                    new AggDocument(new Integer(Convert.ToInt32(elements[1])),
                                    new Integer(Convert.ToInt32(elements[2])),
                                    new Integer(Convert.ToInt32(elements[3])),
                                    elements[4])));
              }
              messages.InsertFirst(msg);
              time_epoch++;
            }
          }
        }
      }
      Int64 start = RegionAllocator.NativeGetPerformanceCounter();
      for (Node<Message<int, Pair<Integer, AggDocument>>> cur = messages.Head;
               cur != default(Node<Message<int, Pair<Integer, AggDocument>>>);
               cur = cur.Next)
      {
        aggVertex.onReceive((Message<int, Pair<Integer, AggDocument>>)cur.Data.Clone());
        aggVertex.onNotify(--time_epoch);
      }
      // NOTE: We do not free the input region because we don't want to include
      // it in measuring the runtime. The input region is just used as a
      // mechanism to load the data into memory.
      Int64 end = RegionAllocator.NativeGetPerformanceCounter();
      Utils.Print(end - start);
    }

  }

}