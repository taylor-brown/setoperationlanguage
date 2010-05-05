function goesinset somearg:
	"this is a function"
end
function testset inarg:
	emptyset = {{}{}}
	print({1 2 {1 {2 4} 3} {{4 2} 3 1}})
	nonempty = {5 3 {5 {6 {7}}} goesinset goesinset}
end
