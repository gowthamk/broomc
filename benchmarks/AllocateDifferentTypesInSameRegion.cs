using System;
using Midori.Runtime;

namespace Broom
{
	public interface ITest
	{
		void TestCall();
	}

	public class FirstType : ITest
	{
		public FirstType(Region region)
		{			
		}

		public void TestCall()
		{
			return;
		}
	}

	public class SecondType 
	{
		public SecondType(Region region)
		{			
		}

		public void TestCall()
		{
			return;
		}
	}

	public class Program 
	{
		public static void Main(string[] args)
		{
			var region = RegionAllocator.AllocateRegion(10000);
			var firstObject = new FirstType(region);
			firstObject.TestCall();			
			var secondObject = new SecondType(region);
			firstObject.TestCall();
			secondObject.TestCall();
		}		
	}
}