function gcd a b:
	if a==b:
		a
	else
		if a>b:
			gcd(a-b b)
		else
			gcd( a b-a)
		end
	end
end

function main:
	gcd( 25 35)
end
