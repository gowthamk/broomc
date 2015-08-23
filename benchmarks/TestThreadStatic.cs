using System;
using System.Threading;
using Midori.Runtime;

namespace ThreadStatic 
{

	class Program 
	{
		static Region ParentRegion = RegionAllocator.AllocateRegion(10000);

         public static void WriteError()
         {
            Console.WriteLine("Error number = " + Thread.GetData(Thread.GetNamedDataSlot("ErrNo")));
            Console.WriteLine("Error source = " + Thread.GetData(Thread.GetNamedDataSlot("ErrSource")));
         } 
		

       public static void SetError()
        {
            Random r = new Random();
            Thread.SetData(Thread.GetNamedDataSlot("ErrNo"), r.Next(100));
            Thread.SetData(Thread.GetNamedDataSlot("ErrSource"), Thread.CurrentThread.Name);
            WriteError();
        }
        public static void Main()
        {
            Thread.AllocateNamedDataSlot("ErrNo");
            Thread.AllocateNamedDataSlot("ErrSource");
            Thread th2 = new Thread(new ThreadStart(SetError));
            th2.Name = "t2";
            th2.Start();
            Thread th3 = new Thread(new ThreadStart(SetError));
            th3.Name = "t3";
            th3.Start();
            Thread.FreeNamedDataSlot("ErrNo");
            Thread.FreeNamedDataSlot("ErrSource");
            Console.Read();
        }
    }
}