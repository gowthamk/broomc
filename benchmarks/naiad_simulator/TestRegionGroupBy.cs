using Midori.Runtime;
using System;
using System.IO;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace NaiadSimulator
{

  public class TestRegionGroupBy : TestInterface
  {

    public void Execute(string[] args)
    {
      Region actorRegion =
        RegionAllocator.AllocateRegion(NaiadSimulator.ACTOR_REGION_SIZE);
      RegionGroupByVertex<Document, int, int> groupByVertex;
      using (RegionContext regContext = RegionContext.Create(actorRegion))
      {
        groupByVertex =
          new RegionGroupByVertex<Document, int, int>(
              document => document.authorId);
      }
      PlayData(groupByVertex, args[0], actorRegion);
      RegionAllocator.FreeRegion(actorRegion);
    }

    public void PlayData(RegionGroupByVertex<Document, int, int> vertex,
                         string filename, Region actorRegion) {
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
            Region inputRegion =
              RegionAllocator.AllocateRegion(NaiadSimulator.TMP_REGION_SIZE);
            using (RegionContext regContext = RegionContext.Create(inputRegion))
            {
              var elements = line.Split(' ');
              if (elements[0] == "BEGIN")
              {
                int time = Convert.ToInt32(elements[1]);
                int batch_size = Convert.ToInt32(elements[2]);
                Message<int, Document> msg =
                  new Message<int, Document>(time, batch_size);
                for (int i = 0; i < batch_size; i++)
                {
                  elements = file.ReadLine().Split(' ');
                  msg.put(new Document(Convert.ToInt32(elements[1]),
                                       Convert.ToInt32(elements[2]),
                                       Convert.ToInt32(elements[3]),
                                       elements[4]));
                }
                vertex.onReceive(msg, actorRegion);
              }
              else if (elements[0] == "END")
              {
                vertex.onNotify(Convert.ToInt32(elements[1]));
              }
            }
            RegionAllocator.FreeRegion(inputRegion);
          }
        }
      }
      else
      {
        Console.WriteLine("Input file does not exist");
      }
    }

  }

}