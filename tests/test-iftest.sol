function iftest ifarg:
	if {} == {}:
		{6}
		if 2!=1:
			if 2+2 == ifarg:
				ifarg
				myarg = 4
			else
				1
			end
		else
			2
		end
	else
		{}
	end
myarg
end
function main inargs:
	iftest(4)
end
