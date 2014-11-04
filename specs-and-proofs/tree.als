module tree

open util/integer

-- Node type.
sig Node {}

-- In this exercise, we will consider directed graphs that have no unconnected nodes; 
-- that is, graphs that can be fully described just by their set of edges.
-- In Alloy, we model the edges of a graph as a binary relation from nodes to nodes.
-- We define the following functions for working with graphs.

-- Returns the set of nodes connected by the given graph (edge) relation.
-- The expression g.Node denotes (by the semantics of relational join) the 
-- domain of the graph relation; the expression Node.g denotes its range.
fun nodes[g : Node->Node] : set Node {
  	g.Node + Node.g
}

-- Returns all roots of the given  graph.  These are all nodes in the graph 
-- that have no incoming edges (i.e., they are in the domain but not 
-- the range of the graph relation).
fun roots[g : Node->Node] : set Node {
  	g.Node - Node.g
}

-- Define the notion of a tree, which is a graph with additional 
-- properties.  What are those properties?  
pred tree[t : Node->Node] {
  	-- ~3 constraints
}

-- Define the notion of a spanning tree t for a graph g.  
-- (Hint:  use your tree predicate, given above, to say that spanning 
-- tree is a tree.)
pred spanningTree[t, g: Node->Node] {
  	-- ~3 constraints
}

-- Define the notion of a binary tree in terms of the 
-- left (child) and right (child) relations over nodes.  
-- (Hint:  you may want to use the constraint "one r", 
--  which says that the relation r has exactly one element.)
pred binaryTree[left, right : Node->Node] {
  	-- ~3 constraints
}

-- Define the notion of a binary search tree in terms of 
-- the left and right relations over nodes, and the key
-- function from nodes to integers. 
-- (Hint:  you may want to use the expression "r*", which 
-- denotes the reflexive transitive closure of the binary relation r.)
pred binarySearchTree[left, right : Node->Node, key : Node->Int] {
  	-- ~4 constraints
}


-------------------- Quick Start --------------------

-- Below is a set of sample commands you can use to test 
-- your definitions. These tests are not meant to be exhaustive; 
-- you may want to write more to make sure your definitions are correct. 

-- First, select "Glucose" as your solver from Options -> Solver.
-- To run the ith command, select it from the Execute menu.  Or 
-- just Execute -> Execute All to execute all commands.

-- Click on an "Instance found" link (in the right window pane) to see 
-- a visualization of the model (if any) found by executing a command.

-- To get a nicer visualization of a particular model, use the menu 
-- bar to navigate to Theme -> Load Theme and select the 
-- custom "tree.thm" theme from this file's directory.  The theme 
-- is designed to work well for all sample commands, but it will 
-- have to be loaded separately for each instance.  You can also 
-- get an automatically prettified theme by clicking the "Magic Layout"
-- button.

-- You can drag and move nodes in a visualized graph if 
-- you don't like the way they are automatically layed out.


-------------------- Sample Commands --------------------

-- Check the validity of the following constraints in a universe with 4 Nodes.  
-- If a model is found, it represents a counterexample to our claim of validity.  
-- This particular claim says that a graph containing a node with a self-edge 
-- is not a tree. The expression "iden" represents the built-in identity relation 
-- that maps every element in the universe to itself; "&" stands for set intersection.
--
-- With our current (empty) definitions, this check will fail, and the model 
-- finder will produce a counterexample.  The "expect 0" clause is optional:  
-- it's a comment that says we are expecting these constraints to have 
-- no model.
check  {
  	all g :  Node -> Node { -- for all graphs g such that ...
		(some g & iden) => not tree[g]
	}
} for 4 Node expect 0

-- A graph with some node that has multiple children.
pred interesting[g: Node->Node] {
	not lone Node.g - Node.~g 
}

-- Search for a model that satisfies the tree definition and describes an 
-- interesting tree, which includes a node with multiple children.  
run  {
  	some t :  Node->Node {
   		tree[t]  && 
		interesting[t]
	}
} for 4 Node expect 1



-- Search for a model that satisfies the spanningTree, producing an 
-- interesting tree for on a fully connected graph.
run {
  	some t, g :  Node->Node { -- there exist relations t, g such that ...
   		spanningTree[t, g]  &&  
		interesting[t] &&
		g = nodes[g] -> nodes[g]
	}
} for 4 Node expect 1

-- Search for a model that satisfies the binary tree definition and 
-- describes a tree in which the left and right pointer can be followed 
-- at least twice from some (not necessarily the same) node.   
run  {
  	some left, right :  Node->Node {
   		binaryTree[left, right]  && 
		some left.left && 
		some right.right
	}
} for 8 Node expect 1

-- Search for a model that satisfies the binary tree definition and 
-- describes a tree in which the left and right pointer can be followed 
-- at least twice from some (not necessarily the same) node.   
-- The int parameter controls the number of bits used to represent keys; 
-- we need just 3 bits for a maximum of 8 nodes in the tree.
run {
	some left, right :  Node->Node, key : Node->Int {
   		binarySearchTree[left, right, key]  && 
		some left.left && 
		some right.right
	}
} for 8 Node,  3 int  expect 1
