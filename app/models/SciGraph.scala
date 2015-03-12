package models

case class SciGraphNode(id: String, lbl: String, meta: SciGraphCategory)
case class SciGraphEdge(sub: String, obj: String, pred: String)
case class SciGraphCategory(category: List[String])