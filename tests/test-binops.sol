function bintest fst snd:
	if fst > snd:
		1
	else
		2
	end
	if fst < fst:
		1
	else
		2
	end
	if fst == snd:
		1
	else
		2
	end
	if fst != snd:
		1
	else
		2
	end
	myotherset = {}
	myset = {"hello " + "world" 6 + 5 6-5 6*5 6/2 6%5}
end

function main inarg:
	bintest(1 2)
end
