using System;

namespace ejemplo_csharp
{
    interface INum<T> {

        T Sumar(T otro);
    }

    public class Fraction : INum<Fraction> {

        public int Num { get; }

        public int Den { get; }

        public Fraction(int num, int den) {

            Num = num;
            Den = den;
        }

        public Fraction Sumar(Fraction frac) {

            var comun = Den * frac.Den;
            return new Fraction(
                (int) Num * (comun / Den) + frac.Num * (comun / frac.Den),
                comun
            );
        }
    }

    class Program
    {

        public static T sumar(T[] valores)
        where T : INum<T> {

            T respuesta = valores[0];

            for(int i = 1; i < valores.Length; i++) {
                respuesta = respuesta.Sumar(valores[i]);
            }
        }

        static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");
        }
    }
}
