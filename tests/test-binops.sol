function bintest fst snd:
//fst = 1 snd = 2
	if fst > snd:
		print("failure on GT")
	else
		2
	end
	if fst < snd:
		1
	else
		print("failure on LT")
	end
	if fst == snd:
		print("failure on ==")
	else
		2
	end
	if fst != snd:
		1
	else
		print("failure on !=")
	end
	if {1 2 3} - {1 2} == {3}:
		1
	else
		print("failure on set difference")
	end
	if {2} * {4 3} == {{2 4} {2 3}}:
		1
	else
		print("cartesian product failure")
	end
	if {2 3} & {4 3} == {3}:
		1
	else
		print("intersection failure")
	end
	if {3 2} < {1 2 3 4}:
		1
	else
		print("subset failure")
	end
	if {1 5} !< {4 3}:
		1
	else
		print("disjoint set failure")
	end
	print({3} + {3})
	myotherset = {}
	myset = {"hello " + "world" 6 + 5 12-1 6-5 6*5 6/2 6%5}
end

function main inarg:
	bintest(1 2)
end
