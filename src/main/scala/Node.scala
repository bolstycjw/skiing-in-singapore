import scala.collection.mutable

class Node(val elevation: Int, val pos: (Int, Int)) {
  def isLongerPath(dest: Node): Boolean = {
    val destPath = this :: dest.longestPath
    val dist = destPath.length
    val slope = destPath.head.elevation - destPath.last.elevation
    (dist > this.dist) ||
      (dist == this.dist && slope > this.slope)
  }

  def addPath(dest: Node) = {
    if (this.elevation > dest.elevation) {
      // Add this node as parent node of dest
      dest.parentNodes += this

      if (isLongerPath(dest)) {
        // Update longest path
        longestPath = this :: dest.longestPath
        // Update parent nodes
        updateParents()
      }
    }
  }
  def updateParents(): Unit = {
    if (this.parentNodes.isEmpty)
      Node.updateIfMax(this)
    else this.parentNodes
      .filter(_.isLongerPath(this))
      .map(n => {
        n.longestPath = n :: this.longestPath
        n.updateParents()
      })
  }

  val parentNodes = new mutable.HashSet[Node]
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
  var maxNode = new Node(0, (0, 0))
}

