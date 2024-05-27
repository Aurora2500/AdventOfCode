local P = {}

list = P

function P.new()
	return { first = nil, last = nil }
end

function P.push_front(list, value)
	local node = { value = value, next = list.first, prev = nil }
	if list.first then
		list.first.prev = node
	else
		list.last = node
	end
	list.first = node
end

function P.push_back(list, value)
	local node = { value = value, next = nil, prev = list.last }
	if list.last then
		list.last.next = node
	else
		list.first = node
	end
	list.last = node
end

function P.pop_front(list)
	local node = list.first
	if node then
		list.first = node.next
		if list.first then
			list.first.prev = nil
		else
			list.last = nil
		end
	end
	return node and node.value
end

function P.pop_back(list)
	local node = list.last
	if node then
		list.last = node.prev
		if list.last then
			list.last.next = nil
		else
			list.first = nil
		end
	end
	return node and node.value
end

function P.empty(list)
	return list.first == nil
end
