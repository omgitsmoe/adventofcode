input_file = "d10_input.txt"
# = -> match operator, so we match on read succeeding with :ok here
{:ok, input} = File.read(input_file)
# apparently no array in elixir -> use map
{_, grid, start} = input
|> String.split("\n", trim: true)
|> Enum.reduce({0, %{}, {0, 0}}, fn(line, {row, map, start}) ->
    {_, new_map, new_start} = line
      |> String.codepoints()
      |> Enum.reduce({0, map, start}, fn(c, {col, map, start}) ->
           {col + 1, map |> Map.put({row, col}, c),
            # NOTE: single quotes 'foo' -> charlist, but "foo" -> utf8 string
            if c == "S" do {row, col} else start end}
         end)
    {row + 1, new_map, new_start}
   end)

IO.inspect(start, label: "starting pos")

# NOTE: all named functions in elixir have to exist inside a module
#       apparently this is to facilitate hot code reloading
defmodule Pipes do
  # keyword list -> access like directions[:NORTH]
  # NOTE: modules can't have variables only comptime constants aka attributes :/
  # only way to make a module attribute `@foo value` (comptime constant) accessible
  # from the outside is to use a public function/macro that returns the value
  @directions [
    NORTH: {-1, 0},
    SOUTH: {1, 0},
    WEST: {0, -1},
    EAST: {0, 1},
  ]

  def directions, do: @directions

  def directions_from_pipe(pipe) do
    case pipe do
      # :foo -> atom foo -> just have their own name as value
      "|" -> [:NORTH, :SOUTH]
      "-" -> [:EAST, :WEST]
      "L" -> [:NORTH, :EAST]
      "J" -> [:NORTH, :WEST]
      "7" -> [:SOUTH, :WEST]
      "F" -> [:SOUTH, :EAST]
      "." -> []
      nil -> []
      _ -> raise "unknown pipe '#{pipe}'"
    end
  end

  # NO anon recursive function... so no way to have a hidden helper
  # to provide a good interface -> use `defp` inside module for a private
  # func
  defp add_deltas({y, x}, [dir | rest], acc) do
    {dy, dx} = @directions[dir]
    # elixir does not have cons? but only concat (++)
    # adding to the front cheap, end not (so acc ++ [..] would be slow)
    add_deltas({y, x}, rest, [{y + dy, x + dx}] ++ acc)
  end

  # to pattern match on function param directly you have to define
  # the func multiple times similar to haskell, but more ugly
  defp add_deltas(_pos, [], acc), do: acc

  def get_connected(map, pos) do
    # get connected positions as {y, x} from the pipe at map[pos]
    # old: aux -> anon function -> has to be called like aux.()
    add_deltas(pos, directions_from_pipe(map[pos]), [])
  end

  # multiple definitions with different arities (nr of args)
  # or pattern matching kind of work like overloads
  def get_connected(_map, pos, pipe) do
    # old: aux -> anon function -> has to be called like aux.()
    add_deltas(pos, directions_from_pipe(pipe), [])
  end

  defp update_steps(steps_required, pos, steps) do
    case steps_required[pos] do
      nil -> steps_required |> Map.put(pos, steps)
      prev_steps when steps < prev_steps -> steps_required |> Map.put(pos, steps)
      prev_steps when steps >= prev_steps -> steps_required
    end
  end

  def walk(_map, _start, steps_required, []), do: steps_required

  # NOTE: since there are at max 2 pipes connected we can use a single
  #       prev_pos instead of a set to track visited positions
  def walk(map, start, steps_required, [{pos, prev_pos, steps} | rest]) do
    # IO.inspect(pos, label: "pos")
    # IO.inspect(prev_pos, label: "prevpos")
    # IO.inspect(steps, label: "steps")
    new_steps_required = steps_required |> update_steps(pos, steps)
    case map[pos] do
      "S" when steps > 0 ->
        # reached cycle end
        walk(map, start, new_steps_required, rest)
      "S" when steps == 0 ->
         # here we need to look at all cardinal directions and get connected
         # and see if we're one of them and then step into those directions
         # since we don't know what kind of pipe 'S' is
         # NOTE: for match <- generator() do .. -> comprehension
         # can't filter inside the comprehension expression
         # -> need to use a filter `for n <- 0..5, filter(n), do: ...`
         # => when filter(n)==false -> will be skipped
         to_visit = for neighbour <- add_deltas(start,
                                                Keyword.keys(Pipes.directions()), []),
                        con <- get_connected(map, neighbour),
                        # we're connected to that pipe
                        pos == con do
           {neighbour, pos, steps + 1}
         end

         walk(map, start, new_steps_required, to_visit ++ rest)
      "." -> raise "got ground as pipe"
      nil -> raise "outside of map"
      pipe ->
        to_visit = for visit <- get_connected(map, pos, pipe),
                                visit != prev_pos do
          # (old) NOTE: if .., do: foo -> returns nil for else
          {visit, pos, steps + 1}
        end
        walk(map, start, new_steps_required, to_visit ++ rest)
    end
  end
end

# NOTE: walk along the loop counting steps to determine max pos
#       keep track of prev_pos so we don't visit positions twice for one direction
#       stop when we reach start again (both directions, since one can be shorter)
steps_required = Pipes.walk(grid, start, %{}, [{start, {-1, -1}, 0}])
farthest_from_start = steps_required
|> Enum.reduce(fn {pos, steps}, {max_pos, max_steps} ->
    if steps > max_steps do
      {pos, steps}
    else
      {max_pos, max_steps}
    end
  end)

IO.inspect(farthest_from_start, label: "part1")
IO.puts("Part1: #{farthest_from_start |> elem(1)}")

# NOTE: have to define this here since it can't be referenced when it's
#       nested in another module
# define a struct to carry our loop variables, so update are easier to
# read/less error prone (by accidentally passing the wrong value)
defmodule EncloseState do
  # needs to be a kw list of `field_name: default`, if no default -> nil
  # NOTE: default direction coming from the left, since we start on the
  # top-left-most 'F'
  defstruct pos: nil, start: nil, direction: {0, -1}, enclosed: %{}
end

defmodule Enclose do
  # get the direction where the enclosed part of the loop is expected to be
  # there we will start finding all tiles that are part of the grid, but not
  # of the loop and mark them as 'enclosed'
  defp walk_direction_to_inner(walk_direction, pipe) do
    dir1 = case walk_direction do
      # when going downwards, tiles to the east get enclosed
      # NOTE: one direction case when on - or |
      {1, 0} -> {0, 1}
      {-1, 0} -> {0, -1}
      {0, -1} -> {1, 0}
      {0, 1} -> {-1, 0}
    end
    # could be done nicer, but at this point idc
    dir2 = case {walk_direction, pipe} do
      # need to look in the other directions as well when on edge/curve points
      # otherwise we might miss points that are e.g. enclosed by edge points
      # NOTE: technichally only on the outer sides, e.g. left and up on 'F'
      #       since inner are just the same point
      #       -> return nil there
      {{-1, 0}, "F"} -> {-1, 0} # dir1 already looks left
      {{1, 0}, "F"} -> nil # {1, 0} dir1 already looks right
      {{0, -1}, "F"} -> {0, 1} # dir1 already looks "down"
      {{0, 1}, "F"} -> {0, -1} # dir1 already looks up
      {{-1, 0}, "J"} -> nil # {-1, 0} # dir1 already looks left
      {{1, 0}, "J"} -> {-1, 0} # dir1 already looks right
      {{0, -1}, "J"} -> {0, 1} # dir1 already looks down
      {{0, 1}, "J"} -> nil # {0, -1} # dir1 already looks up
      {{-1, 0}, "L"} -> {1, 0} # dir1 already looks left
      {{1, 0}, "L"} -> nil # {-1, 0} dir1 already looks right
      {{0, -1}, "L"} -> {0, -1} # dir1 already looks down
      {{0, 1}, "L"} -> nil # {0, 1} dir1 already looks up
      {{-1, 0}, "7"} -> nil # {-1, 0} # dir1 already looks left
      {{1, 0}, "7"} -> {-1, 0} # dir1 already looks right
      {{0, -1}, "7"} -> nil # {0, -1} # dir1 already looks down
      {{0, 1}, "7"} -> {0, 1} # dir1 already looks up
      {_, "|"} -> nil
      {_, "-"} -> nil
    end
    {dir1, dir2}
  end

  # get new direction from pipe
  defp turn(direction, pipe) do
    case {direction, pipe} do
      {_, "-"} -> direction
      {_, "|"} -> direction
      # coming downwards to an 'L' means we now walk right
      {{1, 0}, "L"} -> {0, 1}
      {{1, 0}, "J"} -> {0, -1}
      {{-1, 0}, "F"} -> {0, 1}
      {{-1, 0}, "7"} -> {0, -1}
      {{0, 1}, "7"} -> {1, 0}
      {{0, 1}, "J"} -> {-1, 0}
      {{0, -1}, "F"} -> {1, 0}
      {{0, -1}, "L"} -> {-1, 0}
    end
  end

  # will start finding all tiles that are part of the grid, but not of the loop
  # and mark them as 'enclosed'
  defp discover_enclosed([], _grid, _loop_tiles, enclosed), do: enclosed
  defp discover_enclosed([{y, x} | rest], grid, loop_tiles, enclosed) do
    if Map.has_key?(grid, {y, x}) # part of the map
       and !Map.has_key?(loop_tiles, {y, x}) # not part of the loop
       and !Map.has_key?(enclosed, {y, x}) do # not already visited
      enclosed = enclosed |> Map.put({y, x}, true)
      to_visit = for {dy, dx} <- Pipes.directions() |> Keyword.values(),
                     new_pos = {y + dy, x + dx} do
                     
        new_pos
      end

      discover_enclosed(to_visit ++ rest, grid, loop_tiles, enclosed)
    else
      discover_enclosed(rest, grid, loop_tiles, enclosed)
    end
  end

  # walk along the loop starting at the top-left-most point
  # start downwards then keep track of turns in the loop,
  # everything to the left (in walking direction) is enclosed by the loop
  # so going downward -> east enclosed, right -> north enclosed, etc.
  # NOTE: match on state being of EncloseState to differentiate this function
  #       from the public one below
  #       ... does not work since it has the same nr of args and one is public
  #       the other private? ..>>?>??
  defp _find_enclosed(%EncloseState{} = state, grid, loop_tiles) do
    {y, x} = state.pos
    # what direction is enclosed by the loop
    {dir1, dir2} = walk_direction_to_inner(state.direction, loop_tiles[state.pos])
    inner_points = if dir2 != nil do
      {dy1, dx1} = dir1
      {dy2, dx2} = dir2
      [{y + dy1, x + dx1}, {y + dy2, x + dx2}]
    else
      {dy, dx} = dir1
      [{y + dy, x + dx}]
    end
    # collect all the tiles enclosed by the loop
    enclosed = discover_enclosed(inner_points, grid, loop_tiles, state.enclosed)
    # get the new direction depending on the pipe we're currently on
    {dy, dx} = turn(state.direction, loop_tiles[state.pos])
    new_pos = {y + dy, x + dx}
    # IO.inspect(state.pos, label: "current pos")
    # IO.inspect(inner_points, label: "discover starting points")
    # IO.inspect(new_pos, label: "new pos")
    # IO.inspect({state.enclosed |> Map.keys() |> length(), enclosed |> Map.keys() |> length()}, label: "enclosed len b4/after")

    # NOTE: do this after discover_enclosed, so we don't miss tiles for the
    #       end point
    if state.pos != state.start do
      _find_enclosed(%{state | enclosed: enclosed,
                               pos: new_pos,
                               direction: {dy, dx}},
                     grid, loop_tiles)
    else
      enclosed
    end
  end

  def find_enclosed({y, x}, grid, loop_tiles) do
    # start one below so matching on start/detecting cylce end is easier
    state = %EncloseState{pos: {y + 1, x}, start: {y, x}, direction: {1, 0}}
    _find_enclosed(state, grid, loop_tiles)
  end
end

# NOTE: wtf.. can't access structs in the same context as they're defined in
#       so we have to wrap this in a func call
defmodule Helper do
  def enclosed(grid, start, steps_required) do
    # find top-left 'F' that loop starts with
    min_loop = steps_required
    |> Enum.reduce(
      steps_required |> Map.keys() |> hd(),
      fn ({pos, _steps}, min) ->
         if pos < min do
           pos
         else
           min
         end
       end)
    # Enum.map always returns a list, so we have to use Enum.into, where we pass
    # the target collectable as 2nd arg
    # NOTE: map start position to actual pipe
    {loop_tiles, start_pipe} = steps_required
    |> Enum.reduce({%{}, "."}, fn ({pos, _steps}, {tiles, start_pipe}) ->
      if pos == start do
        {y, x} = pos
        connections = for dir <- Keyword.keys(Pipes.directions()),
                      {dy, dx} = Pipes.directions()[dir],
                      neighbour = {y + dy, x + dx},
                      con <- Pipes.get_connected(grid, neighbour),
                      # we're connected to that pipe
                      pos == con do
          dir
        end

        pipe = case connections |> Enum.sort() do
          [:NORTH, :SOUTH] -> "|"
          [:EAST, :WEST]   -> "-"
          [:EAST, :NORTH]  -> "L"
          [:NORTH, :WEST]  -> "J"
          [:SOUTH, :WEST]  -> "7"
          [:EAST, :SOUTH]  -> "F"
        end

        {tiles |> Map.put(pos, pipe), pipe}
      else
        {tiles |> Map.put(pos, grid[pos]), start_pipe}
      end
    end)
    grid = grid |> Map.put(start, start_pipe)

    IO.inspect({start, grid[start]}, label: "start")
    IO.inspect({min_loop, grid[min_loop]}, label: "min loop")
    Enclose.find_enclosed(
      min_loop, grid, loop_tiles)
  end
end
enclosed_map = Helper.enclosed(grid, start, steps_required)
# IO.inspect(enclosed_map |> Map.keys() |> Enum.sort(), label: "enclosed", limit: :infinity)
enclosed = enclosed_map
|> Enum.reduce(0, fn (_kv, acc) -> acc + 1 end)

IO.puts("Part2: #{enclosed}")
