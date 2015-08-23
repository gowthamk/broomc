using Midori.Runtime;
using System;
using System.IO;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace NaiadSimulator
{

  public class TestRegionWindowJoin : TestInterface
  {

    public void Execute(string[] args)
    {
      int windowSize = 4;
      Region actorRegion =
        RegionAllocator.AllocateRegion(NaiadSimulator.ACTOR_REGION_SIZE);
      RegionWindowJoinVertex<Document, Author, int, JoinOutput> windowJoinVertex;
      using (RegionContext regContext = RegionContext.Create(actorRegion))
      {
        windowJoinVertex =
          new RegionWindowJoinVertex<Document, Author, int, JoinOutput>(
            windowSize,
            document => document.authorId,
            author => author.id,
            (document, author) => new JoinOutput(document.title,
                                                 author.name,
                                                 author.age));
        // Note: Depending on how we use batching we may or may not have to
        // clone the fields.
        // (document, author) => new JoinOutput(String.Copy(document.title),
        //                                      String.Copy(author.name),
        //                                      author.age));
      }
      PlayData(windowJoinVertex, args[0], args[1], actorRegion);
      RegionAllocator.FreeRegion(actorRegion);
    }

    public void PlayData(RegionWindowJoinVertex<Document, Author, int, JoinOutput> vertex,
                         string docFileName, string authorFileName,
                         Region actorRegion) {
      if (File.Exists(docFileName) && File.Exists(authorFileName))
      {
        using (StreamReader docFile = File.OpenText(docFileName))
        {
          using (StreamReader authorFile = File.OpenText(authorFileName))
          {
            bool docEmpty = false;
            bool authorEmpty = false;
            Region inputRegion =
              RegionAllocator.AllocateRegion(NaiadSimulator.TMP_REGION_SIZE);
            using (RegionContext regContext = RegionContext.Create(inputRegion))
            {
              while (!docEmpty || !authorEmpty)
              {
                var line = docFile.ReadLine();
                if (line != null)
                {
                  ReadDocBatch(docFile, line, vertex, actorRegion);
                }
                else
                {
                  docEmpty = true;
                }

                line = authorFile.ReadLine();
                if (line != null)
                {
                  ReadAuthorBatch(authorFile, line, vertex, actorRegion);
                }
                else
                {
                  authorEmpty = true;
                }
              }
            }
            RegionAllocator.FreeRegion(inputRegion);
          }
        }
      }
    }

    public void ReadAuthorBatch(StreamReader file,
                                String line,
                                RegionWindowJoinVertex<Document, Author, int, JoinOutput> vertex,
                                Region actorRegion)
    {
      var elements = line.Split(' ');
      if (elements[0] == "BEGIN")
      {
        int time = Convert.ToInt32(elements[1]);
        int batch_size = Convert.ToInt32(elements[2]);
        Message<int, Author> msg =
          new Message<int, Author>(time, batch_size);
        for (int i = 0; i < batch_size; i++)
        {
          elements = file.ReadLine().Split(' ');
          msg.put(new Author(Convert.ToInt32(elements[1]),
                             elements[2],
                             Convert.ToInt32(elements[3])));
        }
        vertex.onReceive(msg, actorRegion);
      }
      else if (elements[0] == "END")
      {
        // NOTE: This is commented because we don't want to call onNotify
        // twice for the same time epoch.
        // vertex.onNotify(Convert.ToInt32(elements[1]));
      }
    }

    public void ReadDocBatch(StreamReader file,
                             String line,
                             RegionWindowJoinVertex<Document, Author, int, JoinOutput> vertex,
                             Region actorRegion)
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
  }

}