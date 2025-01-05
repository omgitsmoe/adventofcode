local file = "d07.in"
local lines = {}

function trim(s)
  return s:gsub("^%s+", ""):gsub("%s+$", "")
end

for line in io.lines(file) do
  lines[#lines + 1] = trim(line)
end

local equations = {}
for _, line in ipairs(lines) do
  local result = nil
  local operands = {}
  for match in string.gmatch(line, "%d+") do
    local number = tonumber(match)
    if not result then
      result = number
    else
      operands[#operands + 1] = number
    end
  end

  equations[#equations+1] = { result = result, operands = operands }
end

-- using `local function foo()` instead of `local foo = function()`
-- since the latter will mean that `foo` can't be accessed inside `foo`,
-- due to not being defined yet
local function is_true(eq, op_idx, acc)
  if op_idx == 1 then
    error("Must start with at least the second operand!")
  end

  if op_idx > #eq.operands then
    if acc == eq.result then
      return true
    end

    return false
  end

  local with_add = is_true(eq, op_idx + 1, acc + eq.operands[op_idx])
  if with_add then
    return true
  else
    return is_true(eq, op_idx + 1, acc * eq.operands[op_idx])
  end
end

local part1 = 0
for _, eq in ipairs(equations) do
  if is_true(eq, 2, eq.operands[1]) then
    part1 = part1 + eq.result
  end
end

print("Part1: " .. part1)

local function comb(a, b)
  local s = string.format("%d%d", a, b)
  return tonumber(s)
end

-- NOTE: the below was built on the wrong assumption that lhs || rhs
--       would combine lhs with the fully evaluated rhs, which is not true
--       (should have read the examples more carefully, although it's not really
--        clear from the text)
-- NOTE: 17 || 8 + 14 -> || just combines 17 and 8, not 17 and the result
--       of 8+14, so 17 || 8 = 178, then 178 + 14 = 192
-- function table.copy(t)
--   local res = {}
--   for k, v in pairs(t) do
--     res[k] = v
--   end
--   return res
-- end

-- local function with(t, value)
--   local res = table.copy(t)
--   res[#res+1] = value
--   return res
-- end

-- local function calc(eq, operators, op_idx, acc)
--   if op_idx > #operators then
--     return acc
--   end

--   local op = operators[op_idx]
--   if op == '+' then
--     return calc(eq, operators, op_idx + 1, acc + eq.operands[op_idx + 1])
--   elseif op == '*' then
--     return calc(eq, operators, op_idx + 1, acc * eq.operands[op_idx + 1])
--   else
--     return comb(acc, calc(eq, operators, op_idx + 1, eq.operands[op_idx + 1]))
--   end
-- end

-- local function is_true_2(eq, op_idx, operators)
--   if op_idx == 1 then
--     error("Must start with at least the second operand!")
--   end

--   if op_idx > #eq.operands then
--     local result = calc(eq, operators, 1, eq.operands[1])
--     vim.print(eq, operators, result)
--     if result == eq.result then
--       vim.print("--TRUE")
--       return true
--     end

--     vim.print("--FALSE")
--     return false
--   end

  
--   if is_true_2(eq, op_idx + 1, with(operators, '+')) then
--     return true
--   end

--   if is_true_2(eq, op_idx + 1, with(operators, '*')) then
--     return true
--   end

--   return is_true_2(eq, op_idx + 1, with(operators, '||'))
-- end

local function is_true_2(eq, op_idx, acc)
  if op_idx == 1 then
    error("Must start with at least the second operand!")
  end

  if op_idx > #eq.operands then
    if acc == eq.result then
      return true
    end

    return false
  end

  
  if is_true_2(eq, op_idx + 1, acc + eq.operands[op_idx]) then
    return true
  elseif is_true_2(eq, op_idx + 1, acc * eq.operands[op_idx]) then
    return true
  else
    -- NOTE: 17 || 8 + 14 -> || just combines 17 and 8, not 17 and the result
    --       of 8+14, so 17 || 8 = 178, then 178 + 14 = 192
    return is_true_2(eq, op_idx + 1, comb(acc, eq.operands[op_idx]))
  end
end

local part2 = 0
for _, eq in ipairs(equations) do
  if is_true_2(eq, 2, eq.operands[1]) then
    part2 = part2 + eq.result
  end
end

print("Part2: " .. string.format("%18.0f", part2))
