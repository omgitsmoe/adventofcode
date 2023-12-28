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

# iterate through all the tiles
# if they're not part of the loop (entry in steps_required)
# and if they haven't been visited yet (no entry in visited)
# -> visit all adjacent tiles stopping at loop tiles and visited tiles
#    and mark as visited
# -> if no connection to the outside can be made -> enclosed
# NOTE: just walking all the tiles and recording whether they have a connection
# to the outside is not enough, since there can be tiles that are completely
# enclosed by loop tiles, but are not enclosed in the acutal loop
# => does not work

# NOTE: instead walk through every row/col and keep track of what state
#       we're in on the row/col level (in/outside)
#       where e.g. '|' toggles the row state, '-' the col state and 'L' both
#       ALSO DOES NOT WORK:
#        -
#       |I| <- here it works, but
#        -
#
#       |F7
#       |||
#       |L|
#       |I|
#       |F7
#       |||
#       here it doesn't since the loop starts/closes at F/L but since it's enclosed
#       by another loop below it also counts as inside
# NOTE: does work if only |, F*J and L*7 open/close the loop on the row
#       (* means everything in-between gets ignored)
#       while -, F*J, 7*L open/close the loop on the col
#       (to ensure this we need to keep a stack of opening curves, so we don't
#        open/close too early)


# NOTE: have to define this here since it can't be referenced when it's
#       nested in another module
# define a struct to carry our loop variables, so update are easier to
# read/less error prone (by accidentally passing the wrong value)
defmodule EncloseState do
  # needs to be a kw list of `field_name: default`, if no default -> nil
  # row/col either 2-tuple of `:in/outside`  and a stack of last opening curve
  # pipes (list in col case)
  defstruct pos: nil, row: {:outside, []}, col: nil, enclosed: 0
end

defmodule Enclose do
  defp toggle_state(state) do
    case state do
      :inside -> :outside
      :outside -> :inside
    end
  end

  defp update_row_state(state, loop_tile) do
    case {state.row, loop_tile} do
      # NOTE: only |, F*J and L*7 can open/close the  loop on the row level
      #       (* means ignoring everything else between)
      # NOTE: need to keep a stack of curve pipes so F*J doesnt close early on
      #       F---7LJJ etc.
      {s, "-"} -> s
      {{io, []}, "|"} -> {io |> toggle_state(), []}
      # opening curves
      {{io, curve_stack}, curve} when curve in ["F", "L"] ->
        {io, [curve] ++ curve_stack}
      # closing curves
      {{io, ["F" | rest]}, "J"} -> {io |> toggle_state(), rest}
      {{io, ["L" | rest]}, "7"} -> {io |> toggle_state(), rest}
      # close without toggling
      {{io, [_ | rest]}, _closing} -> {io, rest}
    end
  end

  defp update_col_state(state, loop_tile, x) do
    col_state = case {state.col[x], loop_tile} do
      # only |, F*J and 7*L can open/close on the col level
      {s, "|"} -> s
      {{io, []}, "-"} -> {io |> toggle_state(), []}
      # opening curves
      {{io, curve_stack}, curve} when curve in ["F", "7"] ->
        {io, [curve] ++ curve_stack}
      # closing curves
      {{io, ["F" | rest]}, "J"} -> {io |> toggle_state(), rest}
      {{io, ["7" | rest]}, "L"} -> {io |> toggle_state(), rest}
      # close without toggling
      {{io, [_ | rest]}, _closing} -> {io, rest}
    end
    state.col |> Map.put(x, col_state)
  end

  # walk throug all the coordinates in the grid keeping a row state and
  # a state for each column encountered (since we're walking top-bottom first)
  def count_enclosed(state, {max_y, max_x}, loop_tiles) do
    {y, x} = state.pos
    if y > max_y do
      state.enclosed
    else
      new_pos = if x >= max_x do
        {y + 1, 0}
      else
        {y, x + 1}
      end
      {{row_state, _}, {col_state, _}} = {state.row, state.col[x]}
      # overwrite binding so we can't accidentally pass the old value
      state = case loop_tiles[{y, x}] do
        nil ->
          # not on a loop tile, check row/col state
          new_enclosed = if row_state == :inside and col_state == :inside do
            state.enclosed + 1
          else
            state.enclosed
          end
          # if new_enclosed > state.enclosed do
          #   IO.inspect({y, x}, label: "enclosed pos")
          # end
          # update field `enclosed` in state
          %{state | enclosed: new_enclosed}
        t ->
          %{state | row: update_row_state(state, t),
                    col: update_col_state(state, t, x)}
      end

      count_enclosed(%{state | pos: new_pos}, {max_y, max_x}, loop_tiles)
    end
  end
end

# NOTE: wtf.. can't access structs in the same context as they're defined in
#       so we have to wrap this in a func call
defmodule Helper do
  def enclosed(input, grid, start, steps_required) do
    {len_y, len_x} = input
    |> String.split("\n", trim: true)
    |> (fn (l) -> {l |> length(), l |> hd() |> String.length()} end).()
    # Enum.map always returns a list, so we have to use Enum.into, where we pass
    # the target collectable as 2nd arg
    # NOTE: map start position to actual pipe
    loop_tiles = steps_required
    |> Enum.into(%{}, fn ({pos, _steps}) ->
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

        {pos, pipe}
      else
        {pos, grid[pos]}
      end
    end)

    Enclose.count_enclosed(
      %EncloseState{pos: {0, 0},
                    # col needs to be a list/map for the state of the whole row
                    col: 0..(len_x - 1)
                         |> Enum.into(%{}, fn (i) -> {i, {:outside, []}} end)},
      {len_y - 1, len_x - 1}, loop_tiles)
  end
end
enclosed = Helper.enclosed(input, grid, start, steps_required)

IO.puts("Part2: #{enclosed}")
