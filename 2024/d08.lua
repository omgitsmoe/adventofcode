local file = "d08.in"
local lines = {}

function trim(s)
  return s:gsub("^%s+", ""):gsub("%s+$", "")
end

for line in io.lines(file) do
  lines[#lines + 1] = trim(line)
end

local max = { y = #lines, x = #lines[1] }
local antennas = {}
for y, line in ipairs(lines) do
  for x = 1, #line do
    local c = line:sub(x, x)
    if c ~= '.' then
      local as = antennas[c]
      if as == nil then
        as = {}
        antennas[c] = as
      end

      as[#as+1] = {y = y, x = x}
    end
  end
end

local function pos_key(p)
  return string.format("%d,%d", p.y, p.x)
end

local function in_bounds(p)
  if p.y < 1 or p.x < 1 or p.y > max.y or p.x > max.x then
    return false
  end
  return true
end

local function get_antinodes(a, b)
  local dy = a.y - b.y
  local dx = a.x - b.x
  local node_a = { y = a.y + dy, x = a.x + dx }
  local node_b = { y = b.y - dy, x = b.x - dx }
  local results = {}
  if in_bounds(node_a) then
    results[#results+1] = node_a
  end
  if in_bounds(node_b) then
    results[#results+1] = node_b
  end

  return results
end

local function all_antinodes(get_antinodes_fn)
  local antinodes = {}
  for freq, ps in pairs(antennas) do
    if #ps < 2 then
      goto continue
    end
    for i=1,#ps do
      for j=i+1,#ps do
        local a = ps[i]
        local b = ps[j]
        local new_nodes = get_antinodes_fn(a, b)
        for _, p in ipairs(new_nodes) do
          local pk = pos_key(p)
          antinodes[pk] = (antinodes[pk] or 0) + 1
        end
      end
    end
    ::continue::
  end

  return antinodes
end

local antinodes = all_antinodes(get_antinodes)
local part1 = 0
for _, _ in pairs(antinodes) do
  part1 = part1 + 1
end
print("Part1: " .. part1)

local function get_antinodes2(a, b)
  local dy = a.y - b.y
  local dx = a.x - b.x
  local results = {}

  -- antenna itself is the first antinode (since a and b are both grid
  -- positions that are in line with at least two antennas of the same freq)
  local p = a
  while in_bounds(p) do
    results[#results+1] = p

    p = { y = p.y + dy, x = p.x + dx }
  end

  -- other direction
  p = b
  while in_bounds(p) do
    results[#results+1] = p

    p = { y = p.y - dy, x = p.x - dx }
  end

  return results
end

local antinodes2 = all_antinodes(get_antinodes2)
local part2 = 0
for _, _ in pairs(antinodes2) do
  part2 = part2 + 1
end
print("Part2: " .. part2)
