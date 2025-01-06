local helper = require("helper")

local file = "d09.in"
local lines = {}

function trim(s)
  return s:gsub("^%s+", ""):gsub("%s+$", "")
end

for line in io.lines(file) do
  lines[#lines + 1] = trim(line)
end

local line = lines[1]
assert(#lines == 1, "Only expected one line!")

local disk = {}
local last_file = nil
local file_id = 0
local expect_file = true
local total_free_space = 0
for i=1, #line do
  local num_blocks = tonumber(line:sub(i, i))

  if expect_file then
    last_file = { file_id = file_id, num_blocks = num_blocks, free_blocks_after = 0 }
    file_id = file_id + 1
    disk[#disk+1] = last_file
    expect_file = false
  else
    assert(last_file ~= nil, "No last file for free space")
    last_file.free_blocks_after = num_blocks
    total_free_space = total_free_space + num_blocks
    last_file = nil
    expect_file = true
  end
end

local function mv_blocks(disk, disk_defragged, rp, free_blocks_max)
  -- don't need to leave free space behind for p1, since every block
  -- can be filled until there is only continuous free space left at the end
  local file_to_mv = disk[rp]
  local blocks_to_mv = math.min(file_to_mv.num_blocks, free_blocks_max)
  disk_defragged[#disk_defragged+1] = { file_id = file_to_mv.file_id, num_blocks = blocks_to_mv }
  file_to_mv.num_blocks = file_to_mv.num_blocks - blocks_to_mv

  if file_to_mv.num_blocks == 0 then
    rp = rp - 1
  end

  return blocks_to_mv, rp
end

local function defrag(disk)
  -- fill the disk completely from scratch, should make state management easier
  local defragged = {}
  -- read pointer where we get the blocks to fill free space
  local rp = #disk
  for _, f in ipairs(disk) do
    -- fill in already placed file if there are blocks left
    if f.num_blocks > 0 then
      defragged[#defragged+1] = { file_id = f.file_id, num_blocks = f.num_blocks }
      f.num_blocks = 0
    end

    local free_blocks = f.free_blocks_after
    -- fill all the free space with blocks from the back
    while free_blocks > 0 and rp > 0 do
      local used_blocks, new_rp = mv_blocks(disk, defragged, rp, free_blocks)
      free_blocks = free_blocks - used_blocks
      rp = new_rp
    end
    f.free_blocks_after = 0
  end

  return defragged
end

local function checksum(defragged)
  local result = 0
  local pos = 0
  for _, f in ipairs(defragged) do
    if f.file_id then
      for block_offset=1, f.num_blocks do
        result = result + ((f.file_id or 0) * pos)
        pos = pos + 1
      end
    else
      -- need to account for free blocks in pt2
      pos = pos + f.free_blocks
    end
  end
  return result
end

local disk_1 = table.copy(disk)
local defragged = defrag(disk_1)
local checksum_1 = checksum(defragged)
print("Part1: " .. checksum_1)

local function mv_file(src, defragged)
  local free_idx = nil
  local free = nil
  for i, f in ipairs(defragged) do
    if (f.free_blocks or 0) >= src.num_blocks then
      free_idx = i
      free = f
      break
    end
  end

  if not free_idx then
    return
  end

  if free.free_blocks == src.num_blocks then
    -- replace
    defragged[free_idx] = { file_id = src.file_id, num_blocks = src.num_blocks }
  else
    table.insert(defragged, free_idx, src)
    free.free_blocks = free.free_blocks - src.num_blocks
  end

  -- replace the old one with free space
  for i=#defragged, 1, -1 do
    local f = defragged[i]
    if f.file_id == src.file_id then
      f.file_id = nil
      f.free_blocks = f.num_blocks
      f.num_blocks = nil
      break
    end
  end
end

local function defrag_whole_file(disk)
  -- need to change our source format to store free blocks separately
  -- to not get in trouble when moving whole files around, since we
  -- need to account for free space when checksumming
  -- (the block is skipped, but the pos is different)
  -- -> need to leave free space behind when moving a file
  -- (before it didn't matter, since all free space would be filled)
  local defragged = {}
  for _, f in ipairs(disk) do
    defragged[#defragged+1] = { file_id = f.file_id, num_blocks = f.num_blocks }
    if f.free_blocks_after > 0 then
      defragged[#defragged+1] = { free_blocks = f.free_blocks_after }
    end
  end

  -- try to move every file once in reverse order
  for i=#disk, 1, -1 do
    local f = disk[i]
    mv_file(f, defragged)
  end

  return defragged
end

local disk_2 = table.copy(disk)
local defragged_2 = defrag_whole_file(disk_2)
local checksum_2 = checksum(defragged_2)
print("Part2: " .. checksum_2)
