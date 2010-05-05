function treeSearch graph tree:
  if graph == {}:
    tree
  else
    edge = pop(graph)
    vertex = pop(edge)
    v2 = pop(edge - {vertex})
		if unvisitedVertex(tree vertex) | unvisitedVertex(tree v2):
      treeSearch(graph - {edge} push(tree edge))
		else
      treeSearch(graph - {edge} tree)
    end
  end
end

function unvisitedVertex edges vertex:
	if edges == {}:
		1 == 1
	else
		v1 = pop(edges)
		if {vertex} < v1:
			1==2
		else
			unvisitedVertex(edges - {v1} vertex)
		end
	end
end

function main:
  treeSearch({{1 2} {2 3} {3 4} {4 1} {4 2}} {})
end
