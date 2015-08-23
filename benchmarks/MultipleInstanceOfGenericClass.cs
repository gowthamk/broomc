using System;
using System.Collections.Generic;

public class Item1
{
	int x;
}

public class Item2
{
	int y;
}

public class Program
{
	public static void Main(string[] args)
	{
		var list1 = new List<Item1>();
		var list2 = new List<Item2>();

		list1.Add(new Item1());
		list2.Add(new Item2());
	}

}