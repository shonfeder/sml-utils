CM.autoload "$/utils.cm";
(* TEST *)
structure GraphTests : sig end =
struct

open PetiTest

structure Graph = Graph (structure Node  = Ordered.Int)
                        (functor EdgeFn  = UndirectedEdge)
                        (functor EdgesFn = Collection.Ordered.BinarySet)
open Graph

val g = fromLists ([1,2,3,4], [(1,2),(2,3),(3,4)])

fun graph_tests () =
  suite "graph tests"
        [
          test "node exists"
               (nodeExists g 1)
        ,
          test "edge exists"
               (edgeExists g (2,3))
        ,
          test "get nodes"
               let val nodes = (nodesToList o getNodes) g
               in nodes = [1,2,3,4]
               end
        ,
          test "get edges"
               let val edges = edgesToList (getEdges g)
               in edges = [(1,2),(2,3),(3,4)]
               end
        ,
          test "addNode when not present"
               let val ns = (getNodes o addNode 8) g
               in Nodes.member 8 ns
               end
        ,
          test "addEdge"
               let val edges = (edgesToList o getEdges o addEdge (4,5)) g
               in  edges = [(1,2),(2,3),(3,4),(4,5)]
               end
        ,
          test "add multiple edges"
               let val edges = (edgesToList o getEdges o addEdge (4,5) o addEdge (4,5)) g
               in  edges = [(1,2),(2,3),(3,4),(4,5)]
               end
        ,
          test "removeEdge"
               let val edges = (edgesToList o getEdges o removeEdge (1,2)) g
               in edges = [(2,3),(3,4)]
               end
        ,
          test "removeNode"
               let val ns = (getNodes o removeNode 1) g
               in (not o Nodes.member 1) ns
               end
        ,
          test "adjacent"
               (adjacent g (2,3) andalso adjacent g (3,2))
        ,
          test "not adjacent"
               ((not o adjacent g) (4,1))
        ,
          test "neighbors"
               ([2,4] = (nodesToList o neighbors g) 3)
        ,
          test "identify nodes"
               let fun identify _ = 13
                   val (nodes, edges) = (toLists o identifyNodes identify (1,3)) g
               in
                   nodes = [2,4,13]
                   andalso
                   edges = [(2,13),(4,13)]
               end
        ,
          test "identify nodes fails"
               let fun identify _ = 2
               in
                   ((toLists o identifyNodes identify (1,3)) g ; false)
                   handle e => exnName e = "Fail"
               end
        ,
          test "contract edge"
               let fun identify _ = 23
                   val (nodes, edges) = (toLists o contractEdge identify (2,3)) g
               in
                   nodes = [1,4,23]
                   andalso
                   edges = [(1,23),(4,23)]
               end
        ,
          test "contract edge fails"
               let fun identify _ = 13
               in
                   ((toLists o contractEdge identify (1,3)) g ; false)
                   handle e => exnName e = "Fail"
               end
        ]

val _ = (run o graph_tests) ()
end (* GraphTests *)

structure MultiGraphTests : sig end =
struct
open PetiTest

structure Graph = Graph (structure Node  = Ordered.Int)
                        (functor EdgeFn  = UndirectedEdge)
                        (functor EdgesFn = Collection.Ordered.List)

open Graph

val g = fromLists ([1,2,3,4], [(1,2),(2,3),(3,4)])

fun multigraph_tests () =
  suite "multigraph tests"
        [
          test "add multiple edges"
               let val edges = (edgesToList o getEdges o addEdge (1,2)) g
               in edges = [(1,2),(1,2),(2,3),(3,4)]
               end
        ,
          test "remove a multiple of an edge"
               let val edges = (edgesToList o getEdges o removeEdge (1,2) o addEdge (1,2)) g
               in edges = (edgesToList o getEdges) g
               end
        ,
          test "contract edge"
               let fun iden _ = 23
                   val (nodes,edges) = (toLists o contractEdge iden (2,3)) g
               in
                   edges = [(4,23),(1,23)]
                   andalso
                   nodes = [1,4,23]
               end
        ,
          test "contract edges to multiples"
               let fun iden _ = 23
                   val (nodes,edges) = (toLists o contractEdge iden (2,3) o addEdge (2,4)) g
                   fun greater pair = Pair.compare Int.compare pair = GREATER
                   val sortEs = ListMergeSort.sort greater
                   val sortNs = ListMergeSort.sort Int.>
               in
                   sortEs edges = sortEs [(1,23),(4,23),(4,23)]
                   andalso
                   sortNs nodes = sortNs [1,4,23]
               end
        ]

val _ = (run o multigraph_tests) ()
end (* MultiGraphTests *)

structure G = Graph (structure Node  = Ordered.Int)
                    (functor EdgeFn  = UndirectedEdge)
                    (functor EdgesFn = Collection.Ordered.List)

val g = G.fromLists ([1,2,3,4], [(1,2),(2,3),(3,4)])

