import scala.collection.mutable

case class Node(elevation: Int, pos: (Int, Int)) {
  def isLongerPath(dest: Node): Boolean = {
    val destPath = this:: dest.longestPath
    val dist = destPath.length
    val slope = destPath.head.elevation - destPath.last.elevation
    (dist > this.dist) ||
      (dist == this.dist && slope > this.slope)
  }

  def addPath(dest: Node) = {
    if (this.elevation > dest.elevation) {
      // Add reference to nodeRefs
      if (Node.nodeRefs contains dest) Node.nodeRefs(dest) += this
      else Node.nodeRefs += dest -> mutable.ListBuffer(this)

      if (isLongerPath(dest)) {
        // Update longest path
        longestPath = this :: dest.longestPath
        // Update parent nodes
        updateParents()
      }
    }
  }
  def updateParents(): Unit = {
    if (Node.nodeRefs contains this)
      Node
        .nodeRefs(this)
        .filter(_.isLongerPath(this))
        .map(n => {
          n.longestPath = n :: this.longestPath
          n.updateParents()
        })
    else Node.updateIfMax(this)
  }
  var longestPath: List[Node] = List(this)
  def dist: Int = longestPath.length
  def slope: Int =
    if (longestPath.isEmpty) 0
    else longestPath.head.elevation - longestPath.last.elevation
}

object Node {
  def updateIfMax(node: Node): Unit = {
    if ((node.dist > maxNode.dist) ||
        (node.dist == maxNode.dist && node.slope > maxNode.slope))
      maxNode = node
  }
  var maxNode: Node = Node(0, (0, 0))
  val nodeRefs = new mutable.HashMap[Node, mutable.ListBuffer[Node]]
}
