using System;

namespace ReferentialTransparency
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("ingrese numeros...");

            var a = Convert.ToInt32(Console.ReadLine());
            var b = Convert.ToInt32(Console.ReadLine());

            Console.WriteLine(Convert.ToInt32(Console.ReadLine()) + a + b);
        }
    }
}
