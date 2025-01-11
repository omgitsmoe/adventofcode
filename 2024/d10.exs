input_file = "d10.in"
# = -> match operator, so we match on read succeeding with :ok here
{:ok, input} = File.read(input_file)
# apparently no array in elixir -> use map
{_, grid} = input
|> String.split("\n", trim: true)
|> List.foldl({0, %{}}, fn(l, acc) ->
     {y, map} = acc
     {_, updated_map} = l
     |> String.graphemes()
     |> Enum.reduce({0, map}, fn(char, acc) ->
          {x, map} = acc
          {x + 1, map
                  |> Map.put({y, x}, String.to_integer(char))}
        end)
     {y + 1, updated_map}
   end)

defmodule Trail do
  def is_trail_start?(grid, pos), do: grid[pos] == 0

  def find_all_trails(grid) do
    grid
    |> Enum.reduce([], fn({pos, height}, acc) ->
      if height == 0 do
        [{pos, find_trail_ends(pos, grid)}] ++ acc
      else
        acc
      end
    end)
  end

  def find_trail_ends({y, x}, grid) do
    do_find_trails([{y, x}], grid, MapSet.new(), [])
  end

  defp valid_steps({y, x}, current_height, grid) do
    # up, right, down, left
    [{-1, 0}, {0, 1}, {1, 0}, {0, -1}]
    |> List.foldl([], fn({dy, dx}, acc) ->
      next_pos = {y + dy, x + dx}
      case grid[next_pos] do
        next_height when next_height == current_height + 1 ->
          [next_pos] ++ acc
        _ -> acc
      end
    end)
  end

  # different function definitions (with same arity/nr of params)
  # will pattern match on the parameters and call the a matching one,
  # if none found -> FunctionClauseError
  defp do_find_trails([], _, _, trail_ends) do
    trail_ends
  end

  defp do_find_trails([pos | to_visit], grid, visited, trail_ends) do
    # no early return in elixir, need to restructure code
    # (use if/else, cond, case with more nesting
    #  or function pattern matching defp foo(a) when a > 1, do ...)
    # or use a separate method
    # -> apparently this is a positive since it keeps methods small
    # NOTE: prob would be better to just filter valid_steps based on
    #       new_visited
    # if MapSet.member?(visited, pos) do
    #   do_find_trails(to_visit, grid, visited, trail_ends)
    # else
    new_visited = visited |> MapSet.put(pos)
    current_height = grid[pos]
    if current_height == 9 do
      do_find_trails(to_visit, grid, new_visited, [pos] ++ trail_ends)
    else
      do_find_trails(
        valid_steps(pos, current_height, grid)
        |> Enum.filter(fn(p) -> !MapSet.member?(new_visited, p) end)
        # so we can pipe to the list concat operator ++
        |> Kernel.++(to_visit),
        grid, new_visited, trail_ends)
    end
  end
end

# IO.inspect(grid, label: "start")
all_trails = Trail.find_all_trails(grid)
IO.inspect(all_trails
|> List.foldl(0, fn({_, ends}, score) ->
     score + length(ends)
   end),
  label: "Part1")

defmodule TrailDistinct do
  def find_all_trails(grid) do
    grid
    |> Enum.reduce([], fn({pos, height}, acc) ->
      if height == 0 do
        [{pos, find_distinct_trails(pos, grid)}] ++ acc
      else
        acc
      end
    end)
  end

  def find_distinct_trails({y, x}, grid) do
    do_find_trails([[{y, x}]], grid, MapSet.new())
  end

  defp valid_steps({y, x}, current_height, grid) do
    # up, right, down, left
    [{-1, 0}, {0, 1}, {1, 0}, {0, -1}]
    |> List.foldl([], fn({dy, dx}, acc) ->
      next_pos = {y + dy, x + dx}
      case grid[next_pos] do
        next_height when next_height == current_height + 1 ->
          [next_pos] ++ acc
        _ -> acc
      end
    end)
  end

  # different function definitions (with same arity/nr of params)
  # will pattern match on the parameters and call the a matching one,
  # if none found -> FunctionClauseError
  defp do_find_trails([], _, trails) do
    trails
  end

  defp do_find_trails([[pos | trail_rest] | to_visit], grid, trails) do
    current_height = grid[pos]
    if current_height == 9 do
      do_find_trails(to_visit, grid, trails |> MapSet.put([pos | trail_rest]))
    else
      do_find_trails(
        valid_steps(pos, current_height, grid)
        |> Enum.map(fn(next_pos) ->
             [next_pos, pos] ++ trail_rest
           end)
        |> Kernel.++(to_visit),
        grid, trails)
    end
  end
end

all_distinct_trails = TrailDistinct.find_all_trails(grid)
IO.inspect(all_distinct_trails
|> List.foldl(0, fn({_, trails}, score) ->
     score + MapSet.size(trails)
   end),
  label: "Part2")
