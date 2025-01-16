input_file = "d11.in"
# = -> match operator, so we match on read succeeding with :ok here
{:ok, input} = File.read(input_file)
# apparently no array in elixir -> use map
stones = input
|> String.split(" ", trim: true)
|> Enum.map(fn(x) -> x |> String.trim() |> String.to_integer() end)

defmodule Blink do
  def blink(stones) do
    do_blink(stones, [])
    |> Enum.reverse()
  end

  defp do_blink([], acc) do
    acc
  end

  defp do_blink([stone | rest], acc) do
    do_blink(rest, apply_rule(stone) ++ acc)
  end

  def split_digits(stone) do
    str = stone |> Integer.to_string()
    half = div(byte_size(str), 2)
    first_part = binary_part(str, 0, half) |> String.to_integer()
    second_part = binary_part(str, half, half) |> String.to_integer()
    [second_part, first_part]
  end

  def num_digits(num) do
    # elixir does not have its own math lib...
    # use erlangs'
    alias :math, as: Math
    case num do
      0 -> 1
      _ -> Math.log10(num) |> trunc() |> Kernel.+(1)
    end
  end

  defp apply_rule(stone) do
    # NOTE: this does not work, since you can only invoke macros and Kernel
    #       functions inside guards
    # case .. do ... _ when num_digits(stone) |> Kernel.rem(2) == 0 ->
    # => do it before and take the cost or use if/else
    even_digits = num_digits(stone) |> rem(2) == 0
    case stone do
      0 -> [1]
      # even number of digits
      _ when even_digits -> split_digits(stone)
      _ -> [stone * 2024]
    end
  end
end

ExUnit.start()
defmodule AssertionTest do
  use ExUnit.Case

  test "num_digits" do
    assert Blink.num_digits(0) == 1
    assert Blink.num_digits(1) == 1
    assert Blink.num_digits(9) == 1
    assert Blink.num_digits(10) == 2
    assert Blink.num_digits(17) == 2
    assert Blink.num_digits(19) == 2
    assert Blink.num_digits(99) == 2
    assert Blink.num_digits(100) == 3
    assert Blink.num_digits(12345) == 5
  end

  test "split_digits" do
    assert Blink.split_digits(1234) == [12, 34]
    assert Blink.split_digits(19) == [1, 9]
    assert Blink.split_digits(17) == [1, 7]
    assert Blink.split_digits(10) == [1, 0]
  end

  test "blink" do
    assert Blink.blink([125, 17]) == [253000, 1, 7]
    assert Blink.blink([253000, 1, 7]) == [253, 0, 2024, 14168]
    assert Blink.blink([253, 0, 2024, 14168]) == [512072, 1, 20, 24, 28676032]
  end
end

# really hard to just call a function N times and pass along the result
# part1_stones = Stream.transform(1..25, stones, fn _, acc ->
#      Blink.blink(acc)
#    end)
# |> Enum.to_li
part1_stones = Enum.reduce(1..25, stones, fn i, acc ->
     IO.inspect(i, label: "blink")
     IO.inspect(length(acc), label: "#stones")
     Blink.blink(acc)
   end)

IO.inspect(length(part1_stones), label: "Part1")

# NOTE: just store the stone => count pairs in a map, since we'd run
#       out of memory storing all the numbers
# NOTE: ... initially thought this would not be enough and tried to come
#       up with some way to compute how many stones a stone would "spawn"
#       ahead of time, e.g. when a multi-digit stone decays to a single digit
#       and then pre-computing all single-digit steps until they're single
#       digits again, then looking them up while doing the steps etc....
#       => should've tested my first idea before going with the way harder
#          one that is way longer to implement as well -.-
defmodule Blink2 do
  def blink(stone_map) do
    do_blink(stone_map)
  end

  def to_stone_map(stones) do
    add_stone_map(%{}, stones)
  end

  def add_stone_map(stone_map, stones, multiplier \\ 1) do
    stones
    |> Enum.reduce(stone_map, fn stone, acc ->
        # ARRRRRGH... bad operator precedence
        # epression: m[k] || 0 + 1
        # expected: (m[k] || 0) + 1
        # actual: (m[k]) || (0 + 1)
        # ..... -.-
        added = (acc[stone] || 0) + 1 * multiplier
        acc |> Map.put(stone, added)
      end)
  end

  defp do_blink(stone_map) do
    stone_map
    |> Enum.reduce(%{}, fn {stone, count}, acc ->
         to_add = apply_rule(stone)
         acc
         |> add_stone_map(to_add, count)
       end)
  end

  def split_digits(stone) do
    str = stone |> Integer.to_string()
    half = div(byte_size(str), 2)
    first_part = binary_part(str, 0, half) |> String.to_integer()
    second_part = binary_part(str, half, half) |> String.to_integer()
    [second_part, first_part]
  end

  def num_digits(num) do
    # elixir does not have its own math lib...
    # use erlangs'
    alias :math, as: Math
    case num do
      0 -> 1
      _ -> Math.log10(num) |> trunc() |> Kernel.+(1)
    end
  end

  defp apply_rule(stone) do
    # NOTE: this does not work, since you can only invoke macros and Kernel
    #       functions inside guards
    # case .. do ... _ when num_digits(stone) |> Kernel.rem(2) == 0 ->
    # => do it before and take the cost or use if/else
    even_digits = num_digits(stone) |> rem(2) == 0
    case stone do
      0 -> [1]
      # even number of digits
      _ when even_digits -> split_digits(stone)
      _ -> [stone * 2024]
    end
  end

  defp all_single([]) do
    true
  end

  defp all_single([s | rest]) do
    case s do
      s when s < 10 -> all_single(rest)
      _ -> false
    end
  end

  def repeat_until_single(i, stones) do
    next = IO.inspect(blink(stones), label: i)
    if all_single(next) do
      {i, next}
    else
      repeat_until_single(i + 1, next)
    end
  end
end

defmodule AssertionTest2 do
  use ExUnit.Case

  test "add_stone_map" do
    assert Blink2.add_stone_map(%{}, [1,2,3]) == %{1 => 1, 2 => 1, 3 => 1}
    assert Blink2.add_stone_map(%{1 => 3}, [1,2,3]) == %{1 => 4, 2 => 1, 3 => 1}
    assert Blink2.add_stone_map(%{1 => 3}, [1,2,3,1]) == %{1 => 5, 2 => 1, 3 => 1}
  end

  test "blink" do
    assert Blink2.blink(%{125 => 1, 17 => 1}) == %{253000 => 1, 1 => 1, 7 => 1}
    assert Blink2.blink(%{512072 => 1, 1 => 1, 20 => 1, 24 => 1, 28676032 => 1})
      == %{512 => 1, 72 => 1, 2024 => 1, 2 => 2, 0 => 1, 4 => 1, 2867 => 1, 6032 => 1}
  end
end

part2_stones = Enum.reduce(1..75, stones |> Blink2.to_stone_map(), fn _, acc ->
     Blink2.blink(acc)
   end)
part2_stones_sum = Enum.reduce(part2_stones, 0, fn {_k, count}, acc -> count + acc end)
IO.inspect(part2_stones_sum, label: "Part2")
