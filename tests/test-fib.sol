function fibonacci n:
	if n < 2:
		n
	else
		fibonacci(n-1) + fibonacci(n-2)
	end
end

function main inarg:
	fibonacci(6)
end
