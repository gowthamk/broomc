using Midori.Runtime;
using System;
using System.IO;

namespace NaiadSimulator
{

  public class InputFileGenerator
  {

    static int NUM_EPOCHS = 50;            // number of epochs
    static int MAX_EPOCH_CARDINALITY = 1; // number of batches in an epoch
    static int MIN_DOC_BATCH_CARDINALITY = 100000;
    static int MAX_DOC_BATCH_CARDINALITY = 500000;  // number of items in a batch
    static int MIN_AUTHOR_BATCH_CARDINALITY = 1;
    static int MAX_AUTHOR_BATCH_CARDINALITY = 5;

    static int MAX_NUMBER_WORDS_IN_TITLE = 5;
    static int MAX_AUTHOR_ID = 10;

    static string[] authorNames = { "Jackson", "Aiden", "Liam", "Lucas", "Noah",
                                    "Mason", "Jayden", "Ethan", "Jacob", "Jack",
                                    "Caden", "Logan", "Benjamin", "Michael",
                                    "Caleb", "Ryan", "Alexander", "Elijah",
                                    "James", "William", "Oliver", "Connor",
                                    "Matthew", "Daniel", "Luke", "Brayden", "Jayce",
                                    "Henry", "Carter", "Dylan", "Gabriel", "Joshua",
                                    "Nicholas", "Isaac", "Owen", "Nathan", "Grayson",
                                    "Eli", "Landon", "Andrew", "Max", "Samuel",
                                    "Gavin", "Wyatt", "Christian", "Hunter",
                                    "Cameron", "Evan", "Charlie", "David",
                                    "Sebastian", "Joseph", "Dominic", "Anthony",
                                    "Colton", "John", "Tyler", "Zachary", "Thomas",
                                    "Julian", "Levi", "Adam", "Isaiah", "Alex",
                                    "Aaron", "Parker", "Cooper", "Miles", "Chase",
                                    "Muhammad", "Christopher", "Blake", "Austin",
                                    "Jordan", "Leo", "Jonathan", "Adrian", "Colin",
                                    "Hudson", "Ian", "Xavier", "Camden", "Tristan",
                                    "Carson", "Jason", "Nolan", "Riley", "Lincoln",
                                    "Brody", "Bentley", "Nathaniel", "Josiah",
                                    "Declan", "Jake", "Asher", "Jeremiah", "Cole",
                                    "Mateo", "Micah", "Elliot"};

    static string[] titleWords = { "Book", "Day", "27", "35", "May", "apprehensive",
                                   "children", "Moon", "the", "one", "question", "random",
                                   "surprise", "answer", "thousand", "possibilities", "and",
                                   "incredible", "many", "101", "dogs", "trees", "authors",
                                   "difficulty", "words", "sampling", "experiments", "very",
                                   "simple", "generator", "technically", "impossible", "or",
                                   "however", "without", "work", "manually"};

    public static string RandomTitle(Random seed)
    {
      int titleLength = seed.Next(MAX_NUMBER_WORDS_IN_TITLE);
      string title = "";
      for (int i = 0; i < titleLength - 1; i++)
      {
        title += titleWords[seed.Next(titleWords.Length)] + "-";
      }
      title += titleWords[seed.Next(titleWords.Length)];
      return title;
    }

    public static void GenerateDocFile(StreamWriter sw)
    {
      Random seed = new Random(42);
      int tmpEpochCardinality = 0;
      Document tmpDoc;
      for (int i = 0; i < NUM_EPOCHS; i++)
      {
        tmpEpochCardinality = seed.Next(1, MAX_EPOCH_CARDINALITY);
        for (int j = 0; j < tmpEpochCardinality; j++)
        {
          int tmpBatchCardinality =
            seed.Next(MIN_DOC_BATCH_CARDINALITY, MAX_DOC_BATCH_CARDINALITY);
          sw.WriteLine("BEGIN {0} {1}", i, tmpBatchCardinality);
          for (int k = 0; k < tmpBatchCardinality; k++)
          {
            tmpDoc = new Document(seed.Next(0, 1000000), seed.Next(1, 500),
                                  seed.Next(1, MAX_AUTHOR_ID),
                                  RandomTitle(seed));
            sw.WriteLine(i + " " + tmpDoc.ToString());
          }
        }
        sw.WriteLine("END " + i);
      }
    }

    public static void GenerateAuthorFile(StreamWriter sw)
    {
      Random seed = new Random(42);
      int tmpEpochCardinality = 0;
      Author tmpAuthor;
      for (int i = 0; i < NUM_EPOCHS; i++)
      {
        tmpEpochCardinality =
          seed.Next(MIN_AUTHOR_BATCH_CARDINALITY, MAX_AUTHOR_BATCH_CARDINALITY);
        sw.WriteLine("BEGIN {0} {1}", i, tmpEpochCardinality);
        for (int j = 0; j < tmpEpochCardinality; j++)
        {
          tmpAuthor = new Author(seed.Next(1, MAX_AUTHOR_ID),
                                 authorNames[seed.Next(1, authorNames.Length - 1)],
                                 seed.Next(15, 90));
          sw.WriteLine(i + " " + tmpAuthor.ToString());
        }
        sw.WriteLine("END " + i);
      }
    }

    public static void Main(string[] args)
    {
      if (args.Length != 7)
      {
        Console.WriteLine("Please specify: num_epochs min_doc_batch_size max_doc_batch_size min_author_batch_size max_author_batch_size doc_file_out author_file_out");
        return;
      }
      InputFileGenerator.NUM_EPOCHS = Convert.ToInt32(args[0]);
      InputFileGenerator.MIN_DOC_BATCH_CARDINALITY = Convert.ToInt32(args[1]);
      InputFileGenerator.MAX_DOC_BATCH_CARDINALITY = Convert.ToInt32(args[2]);
      InputFileGenerator.MIN_AUTHOR_BATCH_CARDINALITY =
        Convert.ToInt32(args[3]);
      InputFileGenerator.MAX_AUTHOR_BATCH_CARDINALITY =
        Convert.ToInt32(args[4]);
      string filePathDoc = @args[5];
      string filePathAuthor = @args[6];
      Console.WriteLine("Creating input files: {0} and {1}",
                        filePathDoc, filePathAuthor);

      if (!File.Exists(filePathDoc) && !File.Exists(filePathAuthor))
      {
        using (StreamWriter sw = File.CreateText(filePathDoc))
        {
          GenerateDocFile(sw);
        }

        using (StreamWriter sw = File.CreateText(filePathAuthor))
        {
          GenerateAuthorFile(sw);
        }
      }

    }

  }

}
