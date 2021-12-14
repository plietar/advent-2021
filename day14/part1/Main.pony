use "collections"
use "itertools"
use "debug"

class val Pair is (Hashable & Equatable[Pair])
	let first: U8
	let second: U8
	new val create(first': U8, second': U8) =>
		first = first'
		second = second'

	fun box eq(that: Pair): Bool =>
		(first == that.first) and (second == that.second)

	fun box hash(): USize => 0
	fun box string(): String =>
		String.from_array([first]) + String.from_array([second])

class Parser
	let main: Main tag
	var input: Array[U8] iso = recover Array[U8] end

	new create(main': Main tag) =>
		this.main = main'

	fun ref apply(data: Array[U8] iso) =>
		input.append(consume data)

	fun ref dispose() =>
		try
			let lines : Array[String] val = String.from_iso_array(this.input = recover Array[U8] end).split("\n")
			let template = lines(0)?.array()
			let rules = recover Map[Pair, U8] end
			for l in Iter[String](lines.values()).skip(2) do
				let parts: Array[String] ref = l.split_by(" -> ")
				if parts.size() < 2 then continue end
				let left = parts(0)?.array()
				let right = parts(1)?.array()
				rules(Pair(left(0)?, left(1)?)) = right(0)?
			end

			this.main.execute(template, consume rules)
		end


actor Main
	let env: Env

	fun tag minmax(input: Iterator[USize] ref): ((USize, USize) | None) =>
		var result : ((USize, USize) | None) = None
		for x in input do
			match result
			| None => result = (x, x)
			| (let min: USize, let max: USize) =>
					if x < min then result = (x, max)
					elseif x > max then result = (min, x)
					end
			end
		end
		result


	fun tag count(input: Array[U8] val): (USize, USize) =>
		var counts = Map[U8, USize]
		for x in input.values() do
			counts(x) = counts.get_or_else(x, 0) + 1
		end

		try
			minmax(counts.values()) as (USize, USize)
		else
			(0, 0)
		end

	be execute(template: Array[U8] val, rules: Map[Pair, U8] val) =>
		var input = template
		for i in Range(0, 10) do
			input = expand(input, rules)
		end
		(let min, let max) = count(input)
		env.out.print((max - min).string())

	fun expand(input: Array[U8] val, rules: Map[Pair, U8] val): Array[U8] val =>
		recover
			let insertions = Array[(USize, U8)]
			for i in Range(0, input.size() - 1) do
				try
					let key = Pair(input(i)?, input(i+1)?)
					match try rules(key)? else None end
					| let x: U8 => insertions.push((i, x))
					| None => None
					end
				end
			end

			let result = Array[U8]
			var j: USize = 0
			for (i, c) in input.pairs() do
				result.push(c)
				try
					if insertions(j)?._1 == i then
						result.push(insertions(j)?._2)
						j = j + 1
					end
				end
			end
			result
		end

	new create(env': Env) =>
		env = env'
		env.input(recover Parser(this) end)
