package graph

import scala.util.hashing.MurmurHash3


class Graph[N, E] {
    final case class Edge(nodeS: Node, nodeE: Node, value: E)

    final case class Node(value: N){
        private var _adj : List[Edge] = List.empty
        private var _neighbours: Set[Node] = Set.empty[Node]

        def adj: List[Edge] = _adj
        def adj_ : Edge => Unit = (newEdge: Edge) => _adj = newEdge :: _adj

        def neighbours: Set[Node] = _neighbours
        def neighbours_ : Node => Unit = (newNode: Node) => _neighbours = _neighbours + newNode

        def degree: Int = adj.length

        override lazy val hashCode: Int = MurmurHash3.productHash(this)
    }

    private var _nodes: Map[N, Node] = Map.empty
    private var _edges: List[Edge] = Nil

    def nodes: Map[N, Node] = _nodes
    def edges: List[Edge] = _edges

    def addNode(value: N): Node = {
        val node = Node(value)
        _nodes = _nodes + (value -> node)

        node
    }

    def addEdge(nodeS: N, nodeE: N, edgeValue: E): Unit = {
        val ns = Node(nodeS)
        val ne = Node(nodeE)

        val edge = Edge(ns, ne, edgeValue)

        _edges = edge :: _edges

        ns.adj_(edge)
        ne.adj_(edge)

        ne.neighbours_(ns)
        ns.neighbours_(ne)
    }
}