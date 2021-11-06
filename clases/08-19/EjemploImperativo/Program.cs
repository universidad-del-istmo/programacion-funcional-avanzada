using System;

namespace EjemploImperativo {
    
    class Program {

        static (int, int) SumaParejas((int, int)[] parejas) {

            var r1 = 0;
            var r2 = 0;

            foreach(var x in parejas) {
                r1 += x.Item1;
                r2 += x.Item2;
            }

            return (r1, r2);
        }

        static int Suma(int[] valores) {

            var resultado = 0;

            foreach(var i in valores) {
                resultado += i;
            }

            return resultado;
        }

        static void Main2(string[] args) {

            // Programa 2
            Console.WriteLine(
                "Hola: " + Console.ReadLine() + ", " + Console.ReadLine()
            );
        }

        static void Main(string[] args) {

            var mensaje = Console.ReadLine();

            // Programa 1
            Console.WriteLine(
                "Hola: " + mensaje + ", " + mensaje
            );
        }
    }
}
