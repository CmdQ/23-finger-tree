using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using CmdQ.FingerTree.Interop;
using CmdQ.FingerTree.Interop.Extensions;

namespace CmdQ.FingerTree.TryOut
{
    class Program
    {
        static void Main(string[] args)
        {
            var tree = new ImmutableList<int>(new[] { 1, 2, 9, 8, 4, 6 });
            Console.WriteLine(tree[3]);
            var shadow = tree.Set(3, 44);
            Console.WriteLine(shadow[3]);
            Console.WriteLine(tree[3]);

            var mut = new MutableList<int>(shadow);
            var back = new ImmutableList<int>(mut);
            var x = mut[1];
            mut[1] = x + 1;

            var five = new int[5];
            var less = tree.RemoveIndex(0);
            less.CopyTo(five, 0);

            var copy = new ImmutableList<int>(tree);

            var e = new ImmutableList<char>();
            var d = e.Append('a');

            d.Concat(d);

            var l = d.ToList();
        }
    }
}
