//
// Copyright (c) 2001 Microsoft Corporation.   All rights reserved.
//
using Midori.Runtime;
using System;

namespace HiRegionSpace {
	public class TestClass
	{
		private int m_Count;

		public TestClass(int count)
		{
			   this.m_Count = count;
		}

		public TestClass(Region region, int count)
		{
			   this.m_Count = count;  			
		}
	}

	public class HiRegion {
		public static void Main(string[] args) {
			var region = RegionAllocator.AllocateRegion(100);  
			var testObj = new TestClass(region, 10);
			System.Console.WriteLine("Hi Mom!");
		}
	}
}
