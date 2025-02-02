input_file = "d12.in"
# = -> match operator, so we match on read succeeding with :ok here
{:ok, input} = File.read(input_file)
# apparently no array in elixir -> use map
# TODO sides on the inside.. :/
# garden =
# """
# AAAAAA
# AAABBA
# AAABBA
# ABBAAA
# ABBAAA
# AAAAAA
# """
# garden =
# """
# YYYYYYYYYYYYYYYYYYYYYYYG
# YYYDDDYYYYYYYYYYYYYYYYGG
# YYYDDDDYDYYYYYYYYYYGGYGG
# YYYYYDDDDDDDDYYYYYYYGGGG
# YIIDYYDDDDDDDYYYYYYYGGGG
# IIIDDYYDDDDDYYYYYYYYYGGG
# IIIDDYDDDDDDDDYYYYYYYYGG
# IIIDDDDDDDDDDYYYYYYYGGGG
# IIDDDDDDDDDDDYYYYYYGGGGG
# IIDIIDDDDDDDDYYYYYYGGGGG
# IIIIIDDDDDDDDYYYYYYYGGGG
# IIIIIDDDDDDYDYYYYCYGGUUU
# IIIIIIDDDDDYYYYYCCCCCCCG
# IIIIIIIDDDDDYYYCCRCCCCCC
# """
# NOTE: this example is valid, but currently not handled, since the
#       the edges are touching FIXME
# NOTE: best trick is to use that fact that the sides of a polygon
#       equal the number of corners
#       -> count the corners (different types like concave and convex
#          in the different directions)
#       -> done
# garden =
# """
# AAAAAA
# AEEAAA
# AEEAAA
# ABBAAA
# ABBAAA
# AAAAAA
# """
garden = input
|> String.split("\n", trim: true)
|> Enum.with_index()
|> Enum.reduce(%{}, fn {line, y}, acc ->
     line
     |> String.graphemes()
     |> Enum.with_index()
     |> Enum.reduce(acc, fn {c, x}, acc ->
          acc
          |> Map.put({y, x}, c)
        end)
   end)

{max_y, max_x} = garden
|> Enum.reduce({0, 0}, fn {{y, x}, _}, {my, mx} -> {max(y, my), max(x, mx)} end)

defmodule Plot do
  # NOTE: should actually be named region
  defstruct char: nil, start: nil, members: [], area: 0, perimeter: 0

  def new(garden, start, visited) do
    {members, visited} = collect_region(garden, start, visited)
    char = garden[start]
    {
      %Plot{
        char: char,
        start: start,
        members: members,
        area: length(members),
        perimeter: calc_perimeter(garden, char, members)
      },
      visited
    }
  end

  def collect_region(garden, start, visited) do
    char = garden[start]
    do_collect_region(garden, [start], char, visited, [])
  end

  defp do_collect_region(_garden, [], _char, visited, members) do
    {members, visited}
  end

  defp do_collect_region(garden, [{y, x} | to_visit], char, visited, members) do
    if garden[{y, x}] != char or MapSet.member?(visited, {y, x}) do
      do_collect_region(garden, to_visit, char, visited, members)
    else
      neighbours = neighbours(garden, {y, x})
      do_collect_region(
        garden,
        neighbours ++ to_visit,
        char,
        MapSet.put(visited, {y, x}),
        [{y, x} | members])
    end
  end

  defp neighbours(garden, {y, x}) do
    # comprehension, filtering positions outside the grid
    for p <- [
      {y - 1, x},
      {y, x + 1},
      {y + 1, x},
      {y, x - 1},
    ], garden[p] != nil, do: p
  end

  defp neighbour_values(garden, {y, x}) do
    # comprehension, filtering nil values
    for c <- [
      garden[{y - 1, x}],
      garden[{y, x + 1}],
      garden[{y + 1, x}],
      garden[{y, x - 1}],
    ], c != nil, do: c
  end

  def calc_perimeter(garden, char, members) do
    do_calc_perimeter(garden, char, members, 0)
  end

  defp perimeter_for_pos(garden, char, pos) do
    # 4 sides, minus each side touching another garden plot of the same region
    4 - (garden
    |> neighbour_values(pos)
    |> Enum.filter(fn c -> c == char end)
    |> length())
  end

  defp do_calc_perimeter(_garden, _char, [], perimeter) do
    perimeter
  end

  defp do_calc_perimeter(garden, char, [p | to_visit], perimeter) do
    do_calc_perimeter(
      garden,
      char,
      to_visit,
      perimeter + perimeter_for_pos(garden, char, p))
  end

  # NOTE: this only works for the sides on the outside,
  #       but there can be holes that also have to be added to the count
  def calc_sides(garden, plot) do
    # start to the top of the topmost, then leftmost point
    # go to the right as start direction and always check
    # 1. if sth is to the right of your direction (e.g. below at the start)
    #   - if not turn right
    #   - otherwise continue
    # 2. if sth is blocking the current direction
    #   - left is open, turn left
    #   - right is open, turn right
    {sy, sx} = plot.start
    do_calc_sides(garden, plot, {sy - 1, sx}, {0, 1}, 0)
  end
  
  def do_calc_sides(_, %Plot{start: {sy, sx}}, {y, x}, _, sides) when sides > 0 and {y, x} == {sy - 1, sx} do
    sides
  end

  def do_calc_sides(garden, plot, {y, x}, {dy, dx}, sides) do
    # IO.inspect({y, x}, label: "pos")
    {dry, drx} = turn({dy, dx}, :RIGHT)
    to_right_pos = {y + dry, x + drx}
    to_right = Enum.member?(plot.members, to_right_pos)
    # IO.inspect({dry, drx}, label: "torightDIR")
    # IO.inspect(to_right_pos, label: "torightPOS")
    # IO.inspect(to_right, label: "toright")

    if not to_right do
      # turn right, e.g.
      #    >
      # AAA 
      # AAA
      #    v
      # AAA 
      # AAA
      # but we also have to step (otherwise to_right will be false again)
      #     
      # AAAv
      # AAA
      next_pos = {y + dry, x + drx}
      do_calc_sides(garden, plot, next_pos, {dry, drx}, sides + 1)
    else
      next_pos = {y + dy, x + dx}
      is_blocked = Enum.member?(plot.members, next_pos)
      if is_blocked do
        # just turn if we're blocked, delay advancing to the next call
        if to_right do
          to_left_dir = turn({dy, dx}, :LEFT)
          do_calc_sides(garden, plot, {y, x}, to_left_dir, sides + 1)
        else
          do_calc_sides(garden, plot, {y, x}, {dry, drx}, sides + 1)
        end
      else
        # advance
        do_calc_sides(garden, plot, next_pos, {dy, dx}, sides)
      end
    end
  end

  def turn(direction, turn_type) do
    # +1 index -> turn right
    directions = [{0, 1}, {1, 0}, {0, -1}, {-1, 0}]
    idx = directions |> Enum.find_index(fn d -> d == direction end)
    next_idx = case turn_type do
      :LEFT -> if idx == 0, do: 3, else: idx - 1
      :RIGHT -> if idx == 3, do: 0, else: idx + 1
    end

    Enum.at(directions, next_idx)
  end

  # NOTE: find holes inside our plot, so we can add the sides of those plots
  #       to our plot
  #       returns: plot items that are enclosed in our plot
  def find_holes(garden, regions, plot) do
    do_find_holes(garden, regions, plot, plot.members, [])
  end

  defp do_find_holes(_garden, _regions, _plot, [], holes) do
    holes
  end

  defp do_find_holes(garden, regions, plot, [p | rest], holes) do
    other_neighbours = for c <- neighbours(garden, p), not Enum.member?(plot.members, c), do: c
    enclosed_pos = other_neighbours |> Enum.filter(fn p -> is_enclosed(garden, plot, garden[p], [p], MapSet.new()) end)
    # look up plot that has this pos as member
    enclosed_plots = enclosed_pos
    |> Enum.map(fn pos ->
      regions |> Enum.filter(fn r -> Enum.member?(r.members, pos) end) |> hd()
    end)
    # only uniques
    |> Enum.reduce([], fn new_hole, acc ->
        if Enum.any?(acc, fn h -> new_hole.start == h.start end)
            or Enum.any?(holes, fn h -> new_hole.start == h.start end) do
          acc
        else
          [new_hole | acc]
        end
    end)

    do_find_holes(garden, regions, plot, rest, enclosed_plots ++ holes)
  end

  defp is_enclosed(_garden, _plot, _char, [], _visited) do
    true
  end

  defp is_enclosed(garden, plot, enclosed_char, [{y, x} | rest], visited) do
    # no if..else if...end .......
    # cond do
    #   # can reach outside
    #   garden[{y, x}] == nil -> false |> IO.inspect(label: "enclosed")
    #   foo == 1 -> ...
    #   (else)
    #   true -> ...
    # NOTE: doing a flood fill now, since walking and turning did not work

    plot_type = garden[{y, x}]
    # can reach outside
    # nil -> outside the grid
    # plot_type that is not the enclosing or enclosed plot -> can reach other plots
    # NOTE: ^ not enough, technically only the nil case makes sure that it's not
    #       enclosed, since this would not be handled:
    # AAAAAA
    # AEEAAA
    # AEEAAA
    # ABBAAA
    # ABBAAA
    # AAAAAA
    # -> E and B would not count as enclosed
    # (but it does work for my input)
    if plot_type == nil do # or (plot_type != plot.start and plot_type != enclosed_char) do
      false
    else
      neighbours = [{y - 1, x}, {y, x + 1}, {y + 1, x}, {y, x - 1}]
      |> Enum.filter(fn p -> not MapSet.member?(visited, p) and not Enum.member?(plot.members, p) end)

      new_visited = MapSet.put(visited, {y, x})
      is_enclosed(garden, plot, enclosed_char, neighbours ++ rest, new_visited)
    end
  end
end

defmodule Garden do
  def map_plots(garden) do
    {max_y, max_x} = garden
    |> Enum.reduce({0, 0}, fn {{y, x}, _}, {my, mx} -> {max(y, my), max(x, mx)} end)

    do_map_plots(garden, {0, 0}, {max_y, max_x}, MapSet.new(), [])
  end

  defp do_map_plots(_garden, {y, _x}, {max_y, _max_x}, _visited, plots) when y > max_y do
    plots
  end

  defp do_map_plots(garden, {y, x}, max_pos, visited, plots) do
    if MapSet.member?(visited, {y, x}) do
      do_map_plots(garden, inc_pos({y, x}, max_pos), max_pos, visited, plots)
    else
      {plot, visited} = Plot.new(garden, {y, x}, visited)
      do_map_plots(garden, inc_pos({y, x}, max_pos), max_pos, visited, [plot | plots])
    end
  end

  defp inc_pos({y, x}, {_max_y, max_x}) do
    if x == max_x do
      {y + 1, 0}
    else
      {y, x + 1}
    end
  end
end

regions = garden
|> Garden.map_plots()

regions
|> Enum.reduce(0, fn p, acc ->
     p.area * p.perimeter + acc
   end)
|> IO.inspect(label: "Part1")

# NOTE: computing many things multiple times here, e.g. is_enclosed
#       or calc_sides, so that's why it takes >5s
#       - could look up the sides in a map
#       - could look up a coordinate (coord->plot map; iter all plots and all
#         member points and map to corresponding plot) to get the plot
#         and then store (on it?) whether it's enclosed
#         (would save the most, since currently we do is_enclosed for 
#         each member point in the worst case)
# NOTE: also broken for some input, see top of the file
regions
|> Enum.reduce(0, fn p, acc ->
     # IO.inspect({p.char, p.start}, label: "plot")
     # IO.inspect(p.area, label: "area")
     sides = Plot.calc_sides(garden, p)
     # |> IO.inspect(label: "sides")
     holes = Plot.find_holes(garden, regions, p)
     # |> IO.inspect(label: "holes")
     # WTTTTTTTTTFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
     # sum = 1 + 1 = 2
     # AND
     # sum = 1 +
     # 1
     # = 2
     # BUT
     # sum = 1
     # + 1
     # = 1
     # WTFF... ELIXIR
     sides_with_holes = (holes
     |> Enum.reduce(0, fn h, acc -> acc + Plot.calc_sides(garden, h) end)
     # |> IO.inspect(label: "sides from enclosed")
     )
     |> Kernel.+(sides)
     # sides_with_holes |> IO.inspect(label: "sides total")
     p.area * sides_with_holes + acc
   end)
|> IO.inspect(label: "Part2")
