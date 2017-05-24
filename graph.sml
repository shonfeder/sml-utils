(* A graph data structure consists of a finite (and possibly mutable) set of
   vertices or nodes or points (Given by ORDERED), together with a collection of
   pairs of these vertices.

   - [ ] If the edges have an associated value, it is a _weighted graph_.
    - Dependent on EDGE
   - [x] If the collection of edges is a multiset, it is a _multi-graph_.
    - Dependent on ORD_COLLECTION
   - [x] If the collection of edges is a set, it is a traditional _graph_.
    - Dependent on ORD_COLLECTION
   - [x] If the pairs are ordered, it is a _directed graph_.
    - Dependent on EDGE
   - [x] If the pairs are unordered, it is an _undirected graph_.
    - Dependent on EDGE

   -- https://en.wikipedia.org/wiki/Graph_(abstract_data_type)

  TODO Provide different, special purpose edge representations:
    - TODO Adjacency matrix
    - TODO Adjacency matrix
    - TODO Incidence matrix
    - TODO Functional tree representation? (Or does that require a different interface?)

  TODO Structure thus:
    Graph
      - Make (args)
      - Algorithms (G:Graph)
 *)

signature EDGE =
sig
    include ORDERED
    type t = ord_key
    type node
    val member   : node -> t -> bool
    val toPair   : t -> node * node
    val fromPair : node * node -> t
end

funsig EDGE_FN (O:ORD_KEY) = EDGE where type node = O.ord_key

functor DirectedEdge (O:ORD_KEY) : EDGE where type node = O.ord_key =
struct
    structure OrdPair = Ordered.Pair(O)
    open OrdPair
    type node = O.ord_key
    type t = OrdPair.ord_key

    val member = Pair.member O.compare
    val toPair = Fn.id
    val fromPair = Fn.id
    (* val compare = Pair.compare O.compare *)
end

functor UndirectedEdge (O:ORD_KEY) : EDGE where type node = O.ord_key =
struct
    structure PairSet = PairSet(O)
    structure OrdPairSet = Ordered.Make(struct open PairSet type ord_key = PairSet.t end)
    open OrdPairSet
    open PairSet
    type node = O.ord_key
    type t = OrdPairSet.ord_key
    (* type ord_key = PairSet.t *)
end

signature GRAPH =
sig
    structure Node  : ORDERED
    structure Nodes : ORD_COLLECTION
    functor EdgeFn  : EDGE_FN
    structure Edge  : EDGE
    structure Edges : ORD_COLLECTION

    type node = Node.ord_key
    type nodes
    type edge
    type edges
    datatype graph = GPH of {nodes : nodes,
                             edges : edges}

    val toEdge   : node * node -> edge
    val fromEdge : edge -> node * node

    val getNodes : graph -> nodes
    val getEdges : graph -> edges

    val setNodes : nodes -> graph -> graph
    val setEdges : edges -> graph -> graph

    val nodeExists : graph -> node -> bool
    val edgeExists : graph -> node * node -> bool

    val nodesFromList : node list -> nodes
    val nodesToList   : nodes -> node list
    val edgesFromList : (node * node) list -> edges
    val edgesToList   : edges -> (node * node) list

    val empty : graph
    val order : graph -> int
    val size  : graph -> int

    val graph : nodes -> edges -> graph
    val fromLists : node list * (node * node) list -> graph
    val toLists   : graph -> node list * (node * node) list

    val map : (nodes -> nodes) * (edges -> edges) -> graph -> graph
    val mapNodes : (nodes -> nodes) -> graph -> graph
    val mapEdges : (edges -> edges) -> graph -> graph

    val addNode : node -> graph -> graph
    val addEdge : node * node -> graph -> graph
    val removeEdge : node * node -> graph -> graph
    val removeNode : node -> graph -> graph
    val adjacent : graph -> node * node -> bool
    val neighbors : graph -> node -> nodes

    val identifyNodes : (node * node -> node) -> (node * node) -> graph -> graph
    val contractEdge  : (node * node -> node) -> (node * node) -> graph -> graph
end (* GRAPH *)

functor Graph (structure Node:ORDERED)
              (functor EdgeFn:EDGE_FN)
              (functor EdgesFn:ORD_COLLECTION_FN) : GRAPH =
struct

    structure Node  = Node
    structure Nodes = Collection.Ordered.BinarySet(Node)

    functor EdgeFn = EdgeFn
    structure Edge  = EdgeFn(Node)
    structure Edges = EdgesFn(Edge)

    type node  = Node.ord_key
    type nodes = Nodes.t
    type edge  = Edge.t
    type edges = Edges.t
    datatype graph = GPH of {nodes : nodes,
                             edges : edges}

    exception Precondition of string

    (* Converts a pair of nodes to the appropriate structure for edges *)
    val toEdge   = Edge.fromPair
    val fromEdge = Edge.toPair

    fun getNodes (GPH g) = #nodes g
    fun getEdges (GPH g) = #edges g

    fun setNodes ns (GPH {nodes,edges}) = GPH {nodes = ns, edges = edges}
    fun setEdges es (GPH {nodes,edges}) = GPH {nodes = nodes, edges = es}

    fun nodeExists g node = (Nodes.member node o getNodes) g
    fun edgeExists g pair = (Edges.member (toEdge pair) o getEdges) g

    val nodesFromList = Nodes.fromList
    val nodesToList   = Nodes.toList
    val edgesFromList = (Edges.fromList o List.map toEdge)
    val edgesToList   = (List.map Edge.toPair o Edges.toList)

    val empty = GPH {nodes = Nodes.empty, edges = Edges.empty}

    (* The order of a graph is |V|, its number of vertices.
       The size of a graph is |E|, its number of edges
       -- https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)*)
    val order = (Nodes.size o getNodes)
    val size  = (Edges.size o getEdges)

    fun graph nodes edges  = GPH {nodes = nodes, edges = edges}
    fun fromLists (nodesList, edgesList) = graph (nodesFromList nodesList)
                                                 (edgesFromList edgesList)
    fun toLists g =
      let val nodes = (nodesToList o getNodes) g
          val edges = (edgesToList o getEdges) g
      in
          (nodes, edges)
      end

    fun map (f, g) (GPH {nodes, edges}) = graph (f nodes) (g edges)

    fun mapNodes f graph = map (f, Fn.id) graph
    fun mapEdges f graph = map (Fn.id, f) graph

    (* add_node(G, x): adds the node x, if it is not there; *)
    fun addNode n g = mapNodes (Nodes.add n) g

    (* add_edge(G, x, y): adds the edge from the vertices x to y, if it is not there
       also adds the nodes of the edge *)
    fun addEdge (pair as (x,y)) g =
      let val edge = toEdge pair
          val addEdge  = mapEdges (Edges.add edge)
          val addNodes = (addNode y o addNode x)
      in (addEdge o addNodes) g
      end

    (* remove_edge(G, x, y): removes the edge from the vertices x to y, if it is there; *)
    fun removeEdge pair g = mapEdges (Edges.remove (toEdge pair)) g

    (* remove_node(G, x): removes the node x, if it is there, *)
    (*    along with any edges connecting to x *)
    fun removeNode node g =
      let fun removeNode  nodes = Nodes.delete node nodes
          fun removeEdges edges = Edges.filter (not o Edge.member node) edges
      in map (removeNode, removeEdges) g
      end

    (* adjacent(G, x, y): tests whether there is an edge from the vertices x to y; *)
    (* When dealing with a digraph, the order of x and y matters. otherwise not. *)
    fun adjacent g x_y = edgeExists g x_y

    (* neighbors(G, x): lists all vertices y such that there is an edge from the vertices x to y; *)
    fun neighbors g x =
      let fun isAdjacentToX y = adjacent g (x, y)
      in (Nodes.filter isAdjacentToX) (getNodes g)
      end

    fun identifyNodes identify (nodes as (a, b)) g =
      let val graphNodes = getNodes g
          val ab = identify nodes
          val listNeighbors = nodesToList o neighbors g
          val neighbors = (listNeighbors a) @ (listNeighbors b)
          fun addEdgeToAB (x,g') = addEdge (ab,x) g'
          fun addEdges g' = List.foldl addEdgeToAB g' neighbors
      in
          if Nodes.member ab graphNodes
          then raise Fail "New node id already exists in node set"
          else (removeNode a o removeNode b o addEdges) g
      end

    fun contractEdge identify nodes g =
      if (not o edgeExists g) nodes
      then raise Fail "Edge does not exist in graph"
      else identifyNodes identify nodes g

end

structure Algorithm =
struct
    functor MinCut (G:GRAPH)
                   (Id:sig val identify : 'a * 'a -> 'a end) =
    struct
        structure MultiG = Graph (structure Node  = G.Node)
                                   (functor EdgeFn  = G.EdgeFn)
                                   (* A multi-graph: contracted edges give multiple edges *)
                                   (functor EdgesFn = Collection.Ordered.List)

        structure RandEdge = RandCollection.Make(MultiG.Edges)

        fun fac n = case n
                     of 0 => 1
                      | n => n * (fac n - 1)

        fun contract g =
          let val multiG = (MultiG.fromLists o G.toLists) g
              fun contract' mg =
                if (MultiG.Nodes.size o MultiG.getNodes) mg > 2
                then let val edge = (RandEdge.member o MultiG.getEdges) mg
                     in (contract' o MultiG.contractEdge Id.identify (MultiG.fromEdge edge)) mg
                     end
                else mg
          in
              contract' multiG
          end

        fun find g =
          let val n = G.order g
              val bincoef = (Real.fromInt(fac n) / Real.fromInt((fac 2) * (fac (n - 2))))
              val nln = Math.ln (Real.fromInt n)
              val iterations = Real.round(bincoef * nln)
              val (s::samples) = List.tabulate (iterations, (fn _ => (MultiG.size o contract) g))
              val minCut = List.foldl (Int.max) s samples
          in
              minCut
          end

    end (* MinCut *)

end (* GraphAlgorithms *)
