using System;
using Midori.Runtime;
using System.Diagnostics;

public interface ITest
{
	int flarp();
}

public class RegionClass : ITest
{
	public Item x;

	public RegionClass()
	{		
	}

	public RegionClass(Region region)
	{
	}

	public int Increment(Item obj)
	{		
		x = obj;
		return 1;
	}

	public int flarp() { return 42;}

	public override string ToString()
	{
		throw new NotImplementedException();
	}
}

public class Item
{
	public int x;

	public Item(Region region)
	{		
	}
}

public class Program
{

	public static void Main(string[] args)
	{
		var region = RegionAllocator.AllocateRegion(1000);
		var regionObj = new RegionClass(region);
		var heapObj = new RegionClass();
		heapObj.flarp();
		regionObj.flarp();		

		Console.WriteLine("Before allocation of second region object");
		var foo = new Item(region);	
		Console.WriteLine("After allocation of second region object");

		heapObj.flarp();
		regionObj.flarp();		
		// Console.WriteLine("{0}", obj.Increment(foo));
	}
}