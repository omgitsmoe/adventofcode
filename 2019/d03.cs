using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AdventOfCode2019
{
    static class Day03
    {
        readonly static string[][] wires;

        // static constructor for static class -> executed only once when the type is first used
        static Day03()
        {
            // split into wire 1 and 2 by splitting at newline
            // split into steps by splitting at ','
            wires = File.ReadAllLines("d03.in")
                .Select(i => i.Split(',').ToArray())
                .ToArray();
        }

        public static void Part1()
        {
            Dictionary<Tuple<int, int>, bool[]> grid = new Dictionary<Tuple<int, int>, bool[]>();
            for (int wire = 0; wire < 2; wire++)
            {
                Console.WriteLine("============= WIRE " + wire + " ===============");
                int x = 0, y = 0;
                foreach (string dir in wires[wire])
                {
                    Console.WriteLine("Step: " + dir);
                    int dist = Int32.Parse(dir.Substring(1));
                    int dx = 0, dy = 0;
                    switch(dir[0])
                    {
                        case 'U':
                            dy = 1;
                            break;
                        case 'D':
                            dy = -1;
                            break;
                        case 'L':
                            dx = -1;
                            break;
                        case 'R':
                            dx = 1;
                            break;
                    }
                    for (int cdist = 0; cdist < dist; cdist++)
                    {
                        x += dx;
                        y += dy;
                        // w1_w2 will be null if we haven't visited that point yet
                        // returns true if dict contains key
                        if (grid.TryGetValue(new Tuple<int, int>(x, y), out bool[] w1_w2))
                        {
                            w1_w2[wire] = true;
                        }
                        else
                        {
                            grid.Add(new Tuple<int, int>(x, y), wire == 0 ? new bool[] { true, false } : new bool[] { false, true });
                        }
                    }
                }
            }

            int min_dist = Int32.MaxValue;
            // List<string> min_path;
            foreach (var kv in grid)
            {
                if (kv.Value.Any(b => !b))
                    continue;
                int dist = Math.Abs(kv.Key.Item1) + Math.Abs(kv.Key.Item2);
                if (dist >= min_dist)
                    continue;
                min_dist = dist;
            }

            Console.WriteLine("PART1: " + min_dist);

        }

        public static void Part2()
        {
            Dictionary<Tuple<int, int>, int[]> grid = new Dictionary<Tuple<int, int>, int[]>();
            for (int wire = 0; wire < 2; wire++)
            {
                Console.WriteLine("============= WIRE " + wire + " ===============");
                int x = 0, y = 0;
                int step = 0;
                foreach (string dir in wires[wire])
                {
                    Console.WriteLine("Dir: " + dir + " Step: " + step);
                    int dist = Int32.Parse(dir.Substring(1));
                    int dx = 0, dy = 0;
                    switch(dir[0])
                    {
                        case 'U':
                            dy = 1;
                            break;
                        case 'D':
                            dy = -1;
                            break;
                        case 'L':
                            dx = -1;
                            break;
                        case 'R':
                            dx = 1;
                            break;
                    }
                    for (int cdist = 0; cdist < dist; cdist++)
                    {
                        step++;
                        x += dx;
                        y += dy;
                        // w1_w2 will be null if we haven't visited that point yet
                        // returns true if dict contains key
                        if (grid.TryGetValue(new Tuple<int, int>(x, y), out int[] w1_w2))
                        {
                            w1_w2[wire] = 1;
                            if (wire == 0)
                            {
                                if (w1_w2[2] == 0)
                                    w1_w2[2] = step;
                            }
                            else
                            {
                                if (w1_w2[3] == 0)
                                    w1_w2[3] = step;
                            }
                        }
                        else
                        {
                            int[] value;
                            if (wire == 0)
                            {
                                // occupied wire1, wire2, steps wire1, wire2
                                value = new int[] { 1, 0, step, 0 };
                            }
                            else
                            {
                                value = new int[] { 0, 1, 0, step };
                            }
                            grid.Add(new Tuple<int, int>(x, y), value);
                        }
                    }
                }
            }

            int min_steps = int.MaxValue;

            foreach (var kv in grid)
            {
                if (kv.Value[0] == 0 || kv.Value[1] == 0)
                    continue;
                Console.WriteLine("V: " + kv.Value[2] + " W2: " + kv.Value[3]);
                int steps = kv.Value[2] + kv.Value[3];
                if (steps >= min_steps)
                    continue;
                min_steps = steps;
            }

            Console.WriteLine("PART2: " + min_steps);
        }

    }
}
