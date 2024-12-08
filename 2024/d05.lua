local file = "d05.in"
local lines = {}

function trim(s)
  return s:gsub("^%s+", ""):gsub("%s+$", "")
end

for line in io.lines(file) do
  lines[#lines + 1] = trim(line)
end

local rules = {}
local updates = {}
local in_rules = true
for _, line in ipairs(lines) do
  if line == "" then
    in_rules = false
    goto continue
  end

  if in_rules then
    local rule = {}
    local before, after = string.match(line, "(%d+)|(%d+)")
    rule.before = tonumber(before)
    rule.after = tonumber(after)
    rules[#rules+1] = rule
  else
    local update = {}
    local pages = {}
    local page_to_idx = {}
    -- no split :/
    local idx = 1
    for match in string.gmatch(line, "%d+") do
      local page = tonumber(match)
      page_to_idx[page] = idx
      pages[#pages+1] = page
      idx = idx + 1
    end
    update.page_to_idx = page_to_idx
    update.pages = pages
    updates[#updates+1] = update
  end

  ::continue::
end

function middle(update)
  -- NOTE: 1-based indexing!!! so ceil instead of floor
  return update[math.ceil(#update / 2)]
end

function is_correct(update)
  for _, rule in ipairs(rules) do
    local idx_before = update.page_to_idx[rule.before]
    local idx_after = update.page_to_idx[rule.after]
    if idx_before and idx_after and idx_before >= idx_after then
      return false
    end
  end

  return true
end

function index(haystack, needle)
  for i, v in ipairs(haystack) do
    if v == needle then
      return i
    end
  end
  return nil
end

function fix_order(update)
  -- NOTE: kind of insertion sort in that we have a sorted table `result`
  --       then we insert checking as rule when inserting and moving the
  --       index using min/max insertion points
  local result = {}
  for _, page in ipairs(update.pages) do
    local insert_idx = #result + 1
    -- print("Inserting " .. page .. " into:")
    -- vim.print(result)
    for _, rule in ipairs(rules) do
      if page == rule.before then
        local before_idx = index(result, rule.after)
        if before_idx ~= nil then
          -- same index -> will be inserted before
          insert_idx = math.min(insert_idx, before_idx)
        end
        -- print("Should be inserted BEFORE " .. rule.after .. " at " .. insert_idx)
      elseif page == rule.after then
        local after_idx = index(result, rule.before)
        if after_idx ~= nil then
          insert_idx = math.max(insert_idx, after_idx + 1)
        end
        -- print("Should be inserted AFTER " .. rule.before .. " at " .. insert_idx)
      end
    end

    table.insert(result, insert_idx, page)
  end

  return result
end

local part1 = 0
local part2 = 0
for _, update in ipairs(updates) do
  if is_correct(update) then
    part1 = part1 + middle(update.pages)
  else
    local correct_order = fix_order(update)
    -- vim.print(update.pages, correct_order)
    part2 = part2 + middle(correct_order)
  end
end

print("Part1: " .. part1)
print("Part2: " .. part2)
