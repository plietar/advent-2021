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

class Counts
	let counts: Map[U8, USize] = Map[U8, USize]
	fun ref increment(x: U8) =>
		counts(x) = counts.get_or_else(x, USize(0)) + 1

	fun ref merge(other: Counts box) =>
		for (k, v) in other.counts.pairs() do
			counts(k) = counts.get_or_else(k, USize(0)) + v
		end

	fun minmax(): ((USize, USize) | None) =>
		var result : ((USize, USize) | None) = None
		for x in counts.values() do
			match result
			| None => result = (x, x)
			| (let min: USize, let max: USize) =>
					if x < min then result = (x, max)
					elseif x > max then result = (min, x)
					end
			end
		end
		result

class Pairwise
	var old: (U8 | None) = None
	let iter: Iterator[U8] ref

	new create(iter': Iterator[U8] ref) =>
		iter = iter'

	fun ref next(): Pair? =>
		let x =
			match old
			| None => iter.next()?
			| let x: U8 => x
			end

		let y = iter.next()?
		old = x
		Pair(x, y)

	fun box has_next(): Bool => true

class Solver
	let rules: Map[Pair, U8] val
	let cache: Map[USize, Map[Pair, Counts val] ref] ref = Map[USize, Map[Pair, Counts val] ref]

	new create(rules': Map[Pair, U8] val) =>
		rules = rules'

	fun ref expand(n: USize, key: Pair) : Counts val =>
		let c = cache.insert_if_absent(n, Map[Pair, Counts val])
		try
			c(key)?
		else
			if n > 0 then
				match try rules(key)? else None end
				| let x: U8 =>
					let result = recover Counts end
					result.increment(x)
					result.merge(expand(n - 1, Pair(key.first, x)))
					result.merge(expand(n - 1, Pair(x, key.second)))

					let result' : Counts val = consume result
					c(key) = result'
					result'
				| None => recover Counts end
				end
			else
				recover Counts end
			end
		end

actor Main
	let env: Env

	be execute(input: Array[U8] val, rules: Map[Pair, U8] val) =>
		var counts = Counts
		try
			counts.increment(input(0)?)
		end

		var s = Solver(rules)
		for pair in Pairwise(input.values()) do
			counts.merge(s.expand(40, pair))
			counts.increment(pair.second)
		end

		try
			(let min, let max) = counts.minmax() as (USize, USize)
			env.out.print((max - min).string())
		end

	new create(env': Env) =>
		env = env'
		env.input(recover Parser(this) end)
