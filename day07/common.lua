function parse()
	local input = io.read("*l")
	local data = {}
	local min = nil
	local max = nil
	for i in string.gmatch(input, "%d+") do
		n = tonumber(i)
		if min == nil or n < min then
			min = n
		end
		if max == nil or n > max then
			max = n
		end
		data[#data + 1] = n
	end
	return min, max, data
end

function solve(min, max, data, f)
	local best = nil
	for i=min,max do
		local cost = 0
		for _,x in ipairs(data) do
			local d = math.abs(x-i)
			cost = cost + f(d)
		end
		if best == nil or cost < best then
			best = cost
		end
	end
	return best
end
