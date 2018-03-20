using System;
using Midori.Runtime;

public class RegionClass
{
	int x;

	public RegionClass(Region region)
	{
		x = 0;
	}

	public int Increment()
	{		
		x++;
		return x;		
	}
}

public class Program
{

	public static void Main(string[] args)
	{
		var region = RegionAllocator.AllocateRegion(100);
		var obj = new RegionClass(region);
		Console.WriteLine("{0}", obj.Increment());
	}
}