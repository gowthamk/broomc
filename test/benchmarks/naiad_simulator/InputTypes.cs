using Midori.Runtime;
using System;
using System.IO;
using System.Collections.Generic;
using System.Text;

namespace NaiadSimulator
{

  public class Document : IEquatable<Document>, ICloneable
  {
    public int id;
    public int length;
    public int authorId;
    public string title;

    public override string ToString()
    {
      return id + " " + length + " " + authorId + " " + title;
    }

    public Document(int docId, int docLength, int docAuthorID, string docTitle)
    {
      this.id = docId;
      this.length = docLength;
      this.authorId = docAuthorID;
      this.title = docTitle;
    }

    public Document(Region region, int docId, int docLength, int docAuthorID,
                    string docTitle)
    {
      this.id = docId;
      this.length = docLength;
      this.authorId = docAuthorID;
      this.title = docTitle;
    }

    public object Clone()
    {
      return new Document(id, length, authorId, String.Copy(title));
    }

    public bool Equals(Document other)
    {
      return id == other.id && length == other.length &&
        authorId == other.authorId && title == other.title;
    }

    public override int GetHashCode()
    {
      return id * 17 + length * 43 + authorId * 7 + title.GetHashCode();
    }

  }

  public class Author : IEquatable<Author>, ICloneable
  {
    public int id;
    public string name;
    public int age;

    public override string ToString()
    {
      return id + " " + name + " " + age;
    }

    public Author(int authorId, string authorName, int authorAge)
    {
      this.id = authorId;
      this.name = authorName;
      this.age = authorAge;
    }

    public Author(Region region, int authorId, string authorName, int authorAge)
    {
      this.id = authorId;
      this.name = authorName;
      this.age = authorAge;
    }

    public object Clone()
    {
      return new Author(id, String.Copy(name), age);
    }

    public bool Equals(Author other)
    {
      return id == other.id && name == other.name && age == other.age;
    }

    public override int GetHashCode()
    {
      return id * 17 + name.GetHashCode() * 43 + age;
    }

  }

  public class JoinOutput : IEquatable<JoinOutput>, ICloneable
  {
    public string docTitle;
    public string authorName;
    public int authorAge;

    public override string ToString()
    {
      return docTitle + " " + authorName + " " + authorAge;
    }

    public JoinOutput(string title, string name, int age)
    {
      this.docTitle = title;
      this.authorName = name;
      this.authorAge = age;
    }

    public JoinOutput(Region region, string title, string name, int age)
    {
      this.docTitle = title;
      this.authorName = name;
      this.authorAge = age;
    }

    public object Clone()
    {
      return new JoinOutput(String.Copy(docTitle), String.Copy(authorName),
                            authorAge);
    }

    public bool Equals(JoinOutput other)
    {
      return docTitle == other.docTitle && authorName == other.authorName &&
        authorAge == other.authorAge;
    }

    public override int GetHashCode()
    {
      return docTitle.GetHashCode() * 17 + authorName.GetHashCode() * 43 +
        authorAge;
    }

  }

  public class AggDocument : IEquatable<AggDocument>, ICloneable
  {
    public Integer docId;
    public Integer length;
    public Integer authorId;
    public string title;

    public override string ToString()
    {
      return docId + " " + length + " " + authorId + " " + title;
    }

    public AggDocument(Integer docId, Integer length, Integer authorId,
                       string title)
    {
      this.docId = docId;
      this.length = length;
      this.authorId = authorId;
      this.title = title;
    }

    public AggDocument(Region region, Integer docId, Integer length,
                       Integer authorId, string title)
    {
      this.docId = docId;
      this.length = length;
      this.authorId = authorId;
      this.title = title;
    }

    public object Clone()
    {
      return new AggDocument((Integer)docId.Clone(), (Integer)length.Clone(),
                             (Integer)authorId.Clone(), String.Copy(title));
    }

    public bool Equals(AggDocument other)
    {
      return docId.Equals(other.docId) && length.Equals(other.length) &&
        authorId.Equals(other.authorId) && title == other.title;
    }

    public override int GetHashCode()
    {
      return docId.GetHashCode() * 17 + length.GetHashCode() * 43 +
        authorId.GetHashCode() * 7 + title.GetHashCode();
    }

  }

  public class ReadUtils
  {

    public static Message<int, Document> ReadDocBatchMemory(StreamReader file,
                                                            String line)
    {
      var elements = line.Split(' ');
      int time = Convert.ToInt32(elements[1]);
      int batch_size = Convert.ToInt32(elements[2]);
      Message<int, Document> msg = new Message<int, Document>(time, batch_size);
      for (int i = 0; i < batch_size; i++)
      {
        elements = file.ReadLine().Split(' ');
        msg.put(new Document(Convert.ToInt32(elements[1]),
                             Convert.ToInt32(elements[2]),
                             Convert.ToInt32(elements[3]),
                             elements[4]));
      }
      return msg;
    }

    public static Message<int, Author> ReadAuthorBatchMemory(StreamReader file,
                                                             String line)
    {
      var elements = line.Split(' ');
      int time = Convert.ToInt32(elements[1]);
      int batch_size = Convert.ToInt32(elements[2]);
      Message<int, Author> msg = new Message<int, Author>(time, batch_size);
      for (int i = 0; i < batch_size; i++)
      {
        elements = file.ReadLine().Split(' ');
        msg.put(new Author(Convert.ToInt32(elements[1]),
                           elements[2],
                           Convert.ToInt32(elements[3])));
      }
      return msg;
    }

  }

}