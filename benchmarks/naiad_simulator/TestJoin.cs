using Midori.Runtime;
using System;
using System.IO;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace NaiadSimulator
{

  public class TestJoin : TestInterface
  {

    public void Execute(string[] args)
    {
      JoinVertex<Document, Author, int, JoinOutput, int> joinVertex =
        new JoinVertex<Document, Author, int, JoinOutput, int>(
            document => document.authorId,
            author => author.id,
            (document, author) => new JoinOutput(document.title, author.name,
                                                 author.age));
      //PlayData(joinVertex, args[0], args[1]);
      PlayDataFromMemory(joinVertex, args[0], args[1]);
    }

    public void PlayData(JoinVertex<Document, Author, int, JoinOutput, int> joinVertex,
                         string docFileName, string authorFileName) {
      if (File.Exists(docFileName) && File.Exists(authorFileName))
      {
        using (StreamReader docFile = File.OpenText(docFileName))
        {
          using (StreamReader authorFile = File.OpenText(authorFileName))
          {
            bool docEmpty = false;
            bool authorEmpty = false;
            while (!docEmpty || !authorEmpty)
            {
              var line = docFile.ReadLine();
              if (line != null)
              {
                ReadDocBatch(docFile, line, joinVertex);
              }
              else
              {
                docEmpty = true;
              }

              line = authorFile.ReadLine();
              if (line != null)
              {
                ReadAuthorBatch(authorFile, line, joinVertex);
              }
              else
              {
                authorEmpty = true;
              }
            }
          }
        }
      }
    }

    public void ReadAuthorBatch(StreamReader file,
                                String line,
                                JoinVertex<Document, Author, int, JoinOutput, int> vertex)
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
        vertex.onReceive(msg);
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
                             JoinVertex<Document, Author, int, JoinOutput, int> vertex)
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
        vertex.onReceive(msg);
      }
      else if (elements[0] == "END")
      {
        vertex.onNotify(Convert.ToInt32(elements[1]));
      }
    }

    public void PlayDataFromMemory(JoinVertex<Document, Author, int, JoinOutput, int> joinVertex,
                                   string docFileName, string authorFileName) {
      int timeEpoch = 0;
      RegionLinkedList<Message<int, Document>> msgsDocs;
      RegionLinkedList<Message<int, Author>> msgsAuthors;

      using (StreamReader docFile = File.OpenText(docFileName))
      {
        using (StreamReader authorFile = File.OpenText(authorFileName))
        {
          Region inputRegion =
            RegionAllocator.AllocateRegion(NaiadSimulator.TMP_REGION_SIZE);
          using (RegionContext regContext = RegionContext.Create(inputRegion))
          {
            bool docEmpty = false;
            bool authorEmpty = false;
            msgsDocs = new RegionLinkedList<Message<int, Document>>();
            msgsAuthors = new RegionLinkedList<Message<int, Author>>();
            while (!docEmpty || !authorEmpty)
            {
              var line = docFile.ReadLine();
              if (line != null)
              {
                var elements = line.Split(' ');
                if (elements[0] == "BEGIN")
                {
                  timeEpoch++;
                  msgsDocs.InsertFirst(ReadUtils.ReadDocBatchMemory(docFile, line));
                }
              }
              else
              {
                docEmpty = true;
              }

              line = authorFile.ReadLine();
              if (line != null)
              {
                var elements = line.Split(' ');
                if (elements[0] == "BEGIN")
                  msgsAuthors.InsertFirst(ReadUtils.ReadAuthorBatchMemory(authorFile, line));
              }
              else
              {
                authorEmpty = true;
              }
            }
          }
        }
      }
      Int64 start = RegionAllocator.NativeGetPerformanceCounter();
      Node<Message<int, Document>> curDocs = msgsDocs.Head;
      Node<Message<int, Author>> curAuthors = msgsAuthors.Head;
      while (curDocs != default(Node<Message<int, Document>>) &&
             curAuthors != default(Node<Message<int, Author>>))
      {
        joinVertex.onReceive((Message<int, Document>)curDocs.Data.Clone());
        joinVertex.onReceive((Message<int, Author>)curAuthors.Data.Clone());
        joinVertex.onNotify(--timeEpoch);
        curDocs = curDocs.Next;
        curAuthors = curAuthors.Next;
      }

      // NOTE: We do not free the input region because we don't want to include
      // it in measuring the runtime. The input region is just used as a
      // mechanism to load the data into memory.
      Int64 end = RegionAllocator.NativeGetPerformanceCounter();
      Utils.Print(end - start);
    }

  }

}