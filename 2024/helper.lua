function table.copy(t)
  local copy = {}
  for k, v in pairs(t) do
    if type(v) == "table" then
      copy[k] = table.copy(v)
    else
      copy[k] = v
    end
  end

  return copy
end
