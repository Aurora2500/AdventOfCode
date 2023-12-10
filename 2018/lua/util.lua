Utils = {}

function Utils.per_line(f)
	local l = io.read()
	while l do
		f(l)
		l = io.read()
	end
end