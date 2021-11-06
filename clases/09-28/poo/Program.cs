using System;

namespace poo
{

    /*
    No es C# valido por que T<A> no es valido
    ya que un generico no puede tener parametros
    */
    interface IFunctor<T> {

        T<B> FMap<A, B>(Func<A, B> f, T<A> coleccion);
    }

    interface IEnumerable<T> {

        IEnumerable<B> FMap<B>(Func<T, B> f);
    }


    class MiLista<X>
    : IFunctor<MiLista<X>>
    , IEnumerable<X> {

        private readonly T[] _valores;

        public MiLista(T[] valores) {
            _valores = valores;
        }

        public MiLista<T> FMap<A, B>(Func<A, B> f, T coleccion) {

            B[] resultados = new B[_valores.Length];

            for(int i = 0; i < _valores.Length; i++) {
                resultados[i] = f.Invoke(_valores[i]);
            }

            return new MiLista(resultados);
        }

        public IEnumerable<B> FMap<B>(Func<A, B> f) {

            B[] resultados = new B[_valores.Length];

            for(int i = 0; i < _valores.Length; i++) {
                resultados[i] = f.Invoke(_valores[i]);
            }

            // Es valido retornar MiLista
            return new MiLista(resultados);

            // Es valido tambien
            return resultados;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");
        }
    }
}
