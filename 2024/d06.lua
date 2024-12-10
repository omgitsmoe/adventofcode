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
local dir_idx = 1
-- NOTE: [key] = value (since the key contains a function call here)
-- { [posKey(pos) = ... }
local visited = CountingTable.new()
local max_y = #lines
local max_x = #lines[1]
local pos = start

local step = function(pos, dir)
  return {pos[1] + dir[1], pos[2] + dir[2]}
end
local in_bounds = function(pos)
   return pos[1] > 0 and pos[1] <= max_y and pos[2] > 0 and pos[2] <= max_x
end

while in_bounds(pos) do
  visited[posKey(pos)] = true

  local dir = dir_order[dir_idx]
  local next_pos = step(pos, dir)
  if obstacles[posKey(next_pos)] then
    -- NOTE: need to bias by -1 due to 1-based indexing (so +1 -1),
    --       then add the +1 back after the modulo
    dir_idx = math.fmod((dir_idx), #dir_order) + 1
    next_pos = step(pos, dir_order[dir_idx])
  end

  pos = next_pos
end


-- vim.print(visited)
-- NOTE: calling # (len operator) is only valid for strings or arrays (sequences),
--       so tables where the keys are integers and they increase continguously
--       starting at 1
print("Part1: " .. visited:length())
