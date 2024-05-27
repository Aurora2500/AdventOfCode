require "util"
require "treeset"

local reqs = {}
local steps = {}

Utils.per_line(function(l)
	local dep, step = string.match(l, "Step (%w) must be finished before step (%w) can begin.")
	table.insert(reqs, { dep = dep, step = step })
	steps[dep] = true
	steps[step] = true
end)

for _, req in pairs(reqs) do
	steps[req.step] = nil
end

local open = treeset.new()

for req in pairs(steps) do
	if steps[req] then
		treeset.insert(open, req)
	end
end

edges = {}
for _, req in pairs(reqs) do
	edges[req.dep .. req.step] = true
end

for edge in pairs(edges) do
	print(edge)
end
