using Midori.Runtime;
using System;
using System.IO;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace NaiadSimulator
{

  public class TestRegionSelect : TestInterface
  {
    public void Execute(string[] args)
    {
      RegionSelectVertex<Document, int> selectVertex =
        new RegionSelectVertex<Document, int>(
            document => (document.id < 1000) ? true : false);
      PlayData(selectVertex, args[0]);
    }

    public void PlayData(RegionSelectVertex<Document, int> selectVertex,
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
                selectVertex.onReceive(msg);
              }
              else if (elements[0] == "END")
              {
                selectVertex.onNotify(Convert.ToInt32(elements[1]));
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