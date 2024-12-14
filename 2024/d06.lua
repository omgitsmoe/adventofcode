local file = "d06.in"
local lines = {}

function trim(s)
  return s:gsub("^%s+", ""):gsub("%s+$", "")
end

for line in io.lines(file) do
  lines[#lines + 1] = trim(line)
end

local posKey = function(pos)
  return string.format("%d,%d", pos[1], pos[2])
end

-- NOTE: using talbes as keys has a caveat: tables that are created separately
--       are __always different__ even if they contain the __same key/values__
--       (only if you store the table in a variable they will hash the same)
--       chief architect of Lua recommends serializing the table to a string
--       instead
local obstacles = {}
local start = nil
for y, line in ipairs(lines) do
  for x = 1, #line do
    -- line[x] does not work!, need a str method
    local c = line:sub(x, x)
    if c == "#" then
      -- NOTE: table as key is yucky, so using a string instead, see above
      obstacles[posKey({y, x})] = true
    elseif c == "^" then
      start = {y, x}
    end
  end
end

-- NOTE: # can't be used with dict-like tables, we'd have to use pairs
--       and count manually
--       instead make a new type that tracks new keys
-- table where the methods live -> represents the type
local CountingTable = {}
-- metatable that handles built-ins (operators etc.)
-- (would normally be "hidden" away)
local CountingMetaTable = {
  -- NOTE: called when the key is missing
  --       -> methods are stored inside CountingTable, we need to forward
  --          to it by default
  --       -> (this would normally be enough)
  --          __index = CountingTable,
  --       -> but we also have an inner table that stores the actual values
  --       -> so we would also need to forward to the inner table
  --       -> could lead to key clashes with the methods
  --       => rather expose a method that returns the inner table
  --          or separate methods that replace the operators
  --          (just forwarding or even checking the key and erroring
  --           feel bad)
  --       => doing the quick'n'dirty hack for convenience here
  __index = function(self, key)
    -- print("Key " .. key)
    local v = self.inner[key]
    if v ~= nil then
      return v
    end
    return CountingTable[key]
  end,
  -- not working :/
  __len = function(self)
    print("__len " .. self._length)
    return self._length
  end,
  __newindex = function(self, key, value)
    -- NOTE: this is always called since the key is not present on the actual
    --       table, only on the inner one!!
    if self.inner[key] == nil then
      self._length = self._length + 1
    end
    -- print("Length is now " .. self._length)
    self.inner[key] = value
    -- to not trigger a new __newindex
    -- rawset(self, key, value)
  end
}
-- store function inside table as "method"
-- table:function() -> syntactic sugar for: table.function(self)
function CountingTable:length()
  return self._length
end

function CountingTable.new()
  local tbl = {
    -- need _ otherwise it conflicts with length() method
    _length = 0,
    inner = {}
  }
  setmetatable(tbl, CountingMetaTable)
  return tbl
end

local a = CountingTable.new()
local b = CountingTable.new()
a.a = 1
a.b = 2
a.c = 3
a.c = 4
b.a = 1
b.f = 2
assert(a:length() == 3)
-- assert(a:length() == #a)
assert(b:length() == 2)
assert(a.c == 4)
assert(b.f == 2)

-- up, right, down, left
local dir_order = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}}
UP = 1
RIGHT = 2
DOWN = 3
LEFT = 4
-- NOTE: [key] = value (since the key contains a function call here)
-- { [posKey(pos) = ... }
local max_y = #lines
local max_x = #lines[1]
print("Max y " .. max_y .. " Max x " .. max_x)

local sim = function(start, obstacles)
  local visited = CountingTable.new()
  local dir_idx = UP
  local pos = start

  local step = function(pos, dir)
    return {pos[1] + dir[1], pos[2] + dir[2]}
  end
  local in_bounds = function(pos)
     return pos[1] > 0 and pos[1] <= max_y and pos[2] > 0 and pos[2] <= max_x
  end

  local has_dir = function(arr, dir)
    for _, d in ipairs(arr) do
      if d == dir then
        return true
      end
    end
    return false
  end
  assert(has_dir({UP, DOWN}, UP))
  assert(has_dir({UP, RIGHT, DOWN}, DOWN))

  local add_dir = function(arr, dir)
    if arr == nil then
      return {dir}
    else
      arr[#arr + 1] = dir
      return arr
    end
  end
  assert(has_dir(add_dir({UP, DOWN}, RIGHT), RIGHT))

  while in_bounds(pos) do
    local pos_key = posKey(pos)
    local visited_directions = visited[pos_key]
    if visited_directions and has_dir(visited_directions, dir_idx) then
      -- print("Found loop point: " .. pos_key .. " dir: " .. dir_idx)
      return visited, pos, dir_idx
    end
    visited[pos_key] = add_dir(visited_directions, dir_idx)

    local dir = dir_order[dir_idx]
    local next_pos = step(pos, dir)
    -- NOTE: we might turn and be obstructed by another obstacle
    --       (missed this in first part, but my input did not have this case,
    --        only when placing new obstacles this became a problem)
    --       e.g. on a corner like so:
    --       #.
    --       ^O
    while obstacles[posKey(next_pos)] do
      -- NOTE: need to bias by -1 due to 1-based indexing (so +1 -1),
      --       then add the +1 back after the modulo
      dir_idx = math.fmod((dir_idx), #dir_order) + 1
      next_pos = step(pos, dir_order[dir_idx])
    end

    pos = next_pos
  end

  return visited, nil, nil
end


print("Sim start at " .. posKey(start))
local visited, _, _ = sim(start, obstacles)
-- vim.print(visited)
-- NOTE: calling # (len operator) is only valid for strings or arrays (sequences),
--       so tables where the keys are integers and they increase continguously
--       starting at 1
print("Part1: " .. visited:length())

local add_obstacle = function(obstacles, pos)
  local obstacles_plus_one = {}
  for k, v in pairs(obstacles) do
    obstacles_plus_one[k] = v
  end
  if type(pos) ~= 'string' then
    pos = posKey(pos)
  end
  obstacles_plus_one[pos] = true

  return obstacles_plus_one
end
assert(add_obstacle({["5,3"] = true, ["1,3"] = true}, {3,2})["3,2"])
assert(add_obstacle({["5,3"] = true, ["1,3"] = true}, "72,13")["72,13"])
local obst = {["1,1"] = true}
local obst2 = add_obstacle(obst, {3,2})
assert(obst["3,2"] == nil)
assert(obst["1,1"] ~= nil)
assert(obst2["3,2"] ~= nil)

-- NOTE: brute-force, could pre-filter, since new obstacles have to be on
--       visited path in original and one place next to another obstacle
--       (in vert/hor line)
local loops = 0
-- for y=1, max_y do
--   for x=1, max_x do
--     -- no obstacle present in original
--     if obstacles[posKey({y, x})] == nil then
--       local obstacles_new = add_obstacle(obstacles, {y, x})
--       local _, loop_start, loop_start_dir = sim(start, obstacles_new)
--       if loop_start then
--         loops = loops + 1
--       end
--     end
--   end
-- end
for pos, _ in pairs(visited.inner) do
  if pos == posKey(start) then
    goto continue
  end
  local obstacles_new = add_obstacle(obstacles, pos)
  local _, loop_start, loop_start_dir = sim(start, obstacles_new)
  if loop_start then
    loops = loops + 1
  end

  ::continue::
end

print("Part2: " .. loops)
