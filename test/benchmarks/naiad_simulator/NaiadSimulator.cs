using Midori.Runtime;
using System;
using System.IO;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace NaiadSimulator
{
  public class NaiadSimulator
  {

    public static uint ACTOR_REGION_SIZE = 10000;
    public static uint OUTPUT_REGION_SIZE = 10000;
    public static uint TMP_REGION_SIZE = 10000;

    static void Main(string[] args)
    {
      Initializer.InitializeStatics();
      if (args.Length < 3)
      {
        Console.WriteLine("Unexpected number of arguments. Please specify: region|nonregion experiment input_files");
      }
      string[] input_files = new string[args.Length - 2];
      for (int i = 2; i < args.Length; ++i)
      {
        input_files[i - 2] = args[i];
      }
      TestInterface test;
      if (args[0] == "nonregion")
      {
        if (args[1] == "select")
        {
          test = new Select();
        } else if (args[1] == "aggregate")
        {
          test = new TestAggregate();
        } else if (args[1] == "groupby")
        {
          test = new TestGroupBy();
        } else if (args[1] == "join")
        {
          test = new TestJoin();
        } else if (args[1] == "windowjoin")
        {
          test = new TestWindowJoin();
        } else
        {
          Console.WriteLine("Unexpected test type: {0}", args[1]);
          return;
        }
      } else if (args[0] == "region")
      {
        RegionAllocator.Initialize();
        if (args[1] == "select")
        {
          test = new TestRegionSelect();
        } else if (args[1] == "aggregate")
        {
          test = new TestRegionAggregate();
        } else if (args[1] == "groupby")
        {
          test = new TestRegionGroupBy();
        } else if (args[1] == "join")
        {
          test = new TestRegionJoin();
        } else if (args[1] == "windowjoin")
        {
          test = new TestRegionWindowJoin();
        } else
        {
          Console.WriteLine("Unexpected test type: {0}", args[1]);
          return;
        }
      } else
      {
        Console.WriteLine("Unexpected argument: {0}", args[0]);
        return;
      }
      Int64 start = RegionAllocator.NativeGetPerformanceCounter();
      test.Execute(input_files);
      Int64 end = RegionAllocator.NativeGetPerformanceCounter();
      Utils.Print(end - start);
      RegionAllocator.PrintStatistics();
    }

  }

}
