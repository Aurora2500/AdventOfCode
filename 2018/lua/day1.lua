local l = io.read();
local lines = {}

local sum = 0
while l do
	local n = tonumber(l);
	table.insert(lines, n);
	sum = sum + n;
	l = io.read();
end

print("Part 1:", sum)

local visited = {};

local acc = 0;
local i = 1

while true do
	acc = acc + lines[i];
	if visited[acc] then
		break
	end
	visited[acc] = true;
	i = i + 1
	if i > #(lines) then i = 1 end
end

print("Part 2:", acc);