require "util"

local squares = {}

Utils.per_line(function(l)
	local id, x, y, w, h = string.match(l, "#(%d*) @ (%d*),(%d*): (%d*)x(%d*)")
	x = tonumber(x)
	y = tonumber(y)
	w = tonumber(w)
	h = tonumber(h)
	table.insert(squares, {
		id = tonumber(id),
		left = x,
		top = y,
		right = x + w,
		bot = y + h,
	})
end)

local function intersection(a, b)
	local left = math.max(a.left, b.left)
	local top = math.max(a.top, b.top)
	local right = math.min(a.right, b.right)
	local bot = math.min(a.bot, b.bot)

	local width = math.max(right - left, 0)
	local height = math.max(bot - top, 0)

	return width * height
end

local overlapping = 0

for i = 1, #squares do
	for j = i + 1, #squares do
		local int = intersection(squares[i], squares[j])
		if int > 0 then
			overlapping = overlapping + int
			break
		end
	end
end

print("Part 1:", overlapping)
