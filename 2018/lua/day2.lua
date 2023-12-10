require "util"

local c2 = 0;
local c3 = 0;

local lines = {}

Utils.per_line(function (l)
	local charcount = {}
	for i=1,string.len(l) do
		local char = string.sub(l, i, i)
		if charcount[char] then
			charcount[char] = charcount[char] + 1
		else
			charcount[char] = 1
		end
	end

	local has2 = false;
	local has3 = false;
	for n in next, charcount do
		if charcount[n] == 2 then has2 = true end;
		if charcount[n] == 3 then has3 = true end;
	end

	if has2 then c2 = c2 + 1 end
	if has3 then c3 = c3 + 1 end

	table.insert(lines, l)
end)

print("Part 1:", c2 * c3)

local sol

for i in next, lines do
	for j in next, lines, i do
		local diff = 0
		local lastdiffidx = 0
		for k=1,string.len(lines[i]) do
			if string.sub(lines[i], k, k) ~= string.sub(lines[j], k, k) then
				diff = diff + 1
				lastdiffidx = k
			end
		end
		if diff == 1 then
			sol = string.sub(lines[i], 1, lastdiffidx - 1)
				.. string.sub(lines[i], lastdiffidx + 1, -1)
			goto after
		end
	end
end
::after::

print("Part 2", sol)

