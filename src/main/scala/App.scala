object App {
  def main(args: Array[String]): Unit = {
    type Graph = Array[Array[Node]]
    val graph: Graph = io.Source
      .fromFile(args(0))
      .getLines()
      .drop(1)
      .zipWithIndex
      .map {
        case (row, i) =>
          row.split(" ").toArray.zipWithIndex.map {
            case (elevation, j) => new Node(elevation.trim.toInt, (i, j))
          }
      }
      .toArray

    def searchPaths(graph: Graph) = {
      for (i <- 0 until graph.length) {
        for (j <- 0 until graph(i).length) {
          val cur = graph(i)(j)
          // North
          if (i - 1 >= 0) cur.addPath(graph(i - 1)(j))
          // South
          if (i + 1 < graph.length) cur.addPath(graph(i + 1)(j))
          // West
          if (j - 1 >= 0) cur.addPath(graph(i)(j - 1))
          // East
          if (j + 1 < graph(i).length) cur.addPath(graph(i)(j + 1))

        }
      }
    }

    searchPaths(graph)
    val slope = Node.maxNode.slope
    val dist = Node.maxNode.dist
    println(s"$dist$slope")
  }
}
