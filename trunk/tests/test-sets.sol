function goesinset somearg:
	"this is a function"
end
function testset inarg:
	emptyset = {{}{}}
	nonempty = {5 3 {5 {6 {7}}} goesinset}
end
