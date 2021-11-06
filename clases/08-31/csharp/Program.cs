using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace csharp
{
    class Program
    {

        static F<U> MiSelect<F, T, U>(
            Func<T, U> f,
            F<T> valores
        )
        where F : IEnumerable<T> {

            // var resultado = ImmutableList<U>.Empty;
            var resultado = new F();

            foreach(var v in valores) {

                resultado = resultado.Add(f(v));
            }

            return resultado;
        }

        static int Incrementar(int i) {
            return i  + 1;
        }

        static void Main(string[] args)
        {

            var nums = new int[]{ 1,2,3 };
            var lNums =
                ImmutableList<int>.Empty
                    .Add(1)
                    .Add(2)
                    .Add(3);
            var numsInc = MiSelect(Incrementar, nums);
            var lNumsInc = lNums.Select(Incrementar);
            var primero = numsInc.GetType();
            // var lPrimero = lNumsInc[0];

            Console.WriteLine($"La respuesta es: {primero.FullName}");
        }
    }
}
