function iftest ifarg:
	if {} == {}:
		{3}
		if 2!=1:
			if 2+2 == ifarg:
				ifarg
			else
				0
			end
		else
			0
		end
	{5}
	else
		{}
	end
end
function main inargs:
	iftest(5)
end
