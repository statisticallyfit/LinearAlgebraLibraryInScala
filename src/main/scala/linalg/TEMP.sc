import linalg.util.Util
import linalg.vector.{SetOfVectors, Vector}
import linalg.implicits._


val vset = SetOfVectors(
     Vector(1,2,2,-1),Vector(-3,-6,-6,3) , Vector(4,9,9,-4), Vector(-2,-1,-1,2),
     Vector(5,8,9,-5), Vector(4,2,7,-4)
)
Util.getIndicesOfFreeColumns(vset.rowReducedEchelon())