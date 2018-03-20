using System;
using System.Threading.Tasks;
using Midori.Runtime;

namespace Broom
{
	public class Program 
	{
		public static void Main(string[] args)
		{
		}

		public async int TestAsync()
		{
			var test = await ReadAsync();
			return test;
		}

		public Task<int> ReadAsync()
		{
			return Task.Factory.StartNew(() => 1);
		}
	}
}
