using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using CmdQ.FingerTree.Interop;

namespace CmdQ.FingerTree.TryOut
{
    class Program
    {
        static void Main(string[] args)
        {
            var tree = new RandomAccess<int>(new[] { 1, 2, 9, 8, 4, 6 });
            tree.RemoveAt(3);
            tree[3] = 33;
            Console.WriteLine(tree[3]);
            var shadow = tree.Set(3, 44);
            Console.WriteLine(shadow[3]);
            Console.WriteLine(tree[3]);

            var five = new int[5];
            tree.CopyTo(five, 0);

            var copy = new RandomAccess<int>(tree);
            copy.RemoveAt(0);

            var e = RandomAccess.Emtpy<char>();
            var d = e.Append('a');

            d.Concat(d);

            var l = d.ToList();
        }
    }
}
