using System;
using Midori.Runtime;

public class Item
{
	int x;
}

public class RegionClass<T>
{
	T x;

	public RegionClass(Region region)
	{
	}

	public int Increment(T obj)
	{		
		x = obj;
		return 1;
	}
}

public class Program
{

	public static void Main(string[] args)
	{
		var region = RegionAllocator.AllocateRegion(100);
		var obj = new RegionClass<Item>(region);
		Console.WriteLine("{0}", obj.Increment(new Item()));
	}
}