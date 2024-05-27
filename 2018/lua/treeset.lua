local P = {}

treeset = P

function P.new()
	return { root = nil, size = 0 }
end

function P.insert(set, value)
	local node = set.root
	local parent = nil
	while node do
		parent = node
		if value < node.value then
			node = node.left
		elseif value > node.value then
			node = node.right
		else
			return false
		end
	end
	node = { value = value, left = nil, right = nil }
	if parent then
		if value < parent.value then
			parent.left = node
		else
			parent.right = node
		end
	else
		set.root = node
	end
	set.size = set.size + 1
	return true
end

function P.popfirst(set)
	local node = set.root
	local parent = nil
	if node then
		while node.left do
			parent = node
			node = node.left
		end
		if parent then
			parent.left = node.right
		else
			set.root = node.right
		end
		set.size = set.size - 1
		return node.value
	end
end

function P.empty(set)
	return set.root == nil
end
