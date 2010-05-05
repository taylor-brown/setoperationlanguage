function addtwo int:
	int+2
end

function lessthan2 int:
	int < 2
end

function test inarg:
	print(sizeof({1 2 "hi" 5}))
	print(pop({2 4 3}))
  print(push({1 2 3} 2+3))
	print(map(addtwo {1 2 3}))
	print(4 == 5)
	print(filter(lessthan2 {1 2 3 4}))
end
	
