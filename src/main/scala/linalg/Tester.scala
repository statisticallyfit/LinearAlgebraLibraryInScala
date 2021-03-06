package linalg

import linalg.implicits._
import linalg.kernel.{Complex, Rational, Real}
import linalg.matrix.{AugmentedMatrix, HilbertMatrix, Matrix}
import linalg.vector.{SetOfVectors, Vector}
import linalg.util._

import scala.collection.mutable.ListBuffer

/**
  */



object Tester extends App {

     val a1: Complex[Rational] = Rational(3,5) + Rational(2, 4).i + Rational(1) + Rational(5)
     val a2: Complex[Int] = 3 + 5.i + 3
     val a3: Complex[Int] = 1 - 2.i

     assert(Rational(2).den == 1 && Rational(2).num == 2)
     assert(Rational(4, 5).den == 5 && Rational(4,5).num == 4)
     assert(a1.re === Rational(33,5) && a1.im === Rational(1,2))
     assert(a2.re === 6 && a2.im === 5)
     assert(a3.re === 1 && a3.im === 2)

     //nroot test
     assert(Complex(1.0, 2.0).nRoot(2) === )
     println("NROOT TEST: " + Complex(1.0, 2.0).nRoot(2))
     println("^ test: " + (Complex(1.0, 2.0) ^ 2))
     println("^ test and + after: " + ((Complex(1 ,3) ^ 2) + 4))
     println("^ test and + after: " + ((Complex(1.0 ,3.0) ^ 2.0) + 4.0))
     println("^ test and ^ after: " + ((Complex(1.0 ,3.0) ^ 2) ^ 4))
     println("NROOT TEST: " + (Rational(2) ^ Rational(2)))
     println("ABS TEST: " + Real(-2).abs())
     println("ABS TEST: " + Complex[Double](-1, 2).abs())


     println(r1 + r2)
     println(a3)

     println(a2 < a3)
     println(a2 === a3)
     println((4 + 3.i) === (4 + 3.i))
     println((2 + 5.i) < (2 + 7.i))
     println((2 + 5.i) < (2 - 5.i))
     println((8 + 2.i) + (9 + 2.i))
     println((8 + 2.i) - (9 + 2.i))
     println((8 + 2.i) < (9 + 2.i))

     println(a1)
     println(a2)
     println(a1 + Rational(1))
     println(Rational(33) + a1)
     println(23.0 + (1.0 + 3.0.i))
     println((1.5 + 3.2.i) + 23.2)
     println((1 + 3.i) + 1)
     println(1 + (1 + 3.i))

     println(new Rational(4, 8))
     println(Rational(4, 8) + Rational(5, 15))
     println(Complex(1,2))
     println(Complex(1,2) + Complex(3,4))



     // Vectors ---------------------------------------------------------------------------
     //TODO fix int so that we can declare vectors using int nums but operations are in double and are returned as
     // TODO ... double.
     var v1: Vector[Rational] = Vector(1,2,3)
     val v2: Vector[Rational] = Vector(2,0,4, 5)

     println("NEgate: " + v1.negate())
     println("Show v2: " + v2)
     println("Plus: " + (Vector(2,3,4) + Vector(-2, 3, -6)))
     println("is zero?: " + v1.isZero)
     v1 = Vector(3,8,-1,5)
     println("Dot product: " + v1.dotProduct(v2))
     println("v1 norm: " + v1.norm()) //TODO fix
     println("is normalized: " + v1.isNormalized())
     println("get: " + v2.get(3) + "\n\n")
     //println(v1.crossProduct(v2))

     //Set vec
     //TODO implicit from int to rational and from double to real
     /*val s1: SetOfVectors[Real] = SetOfVectors(
          Vector(1.0, 2.0, 3.0, 4.0, 5.0),
          Vector(8.0, 8.0, 7.0, 2.0, 3.0),
          Vector(-8.0, 9.0, -3.0, 0.0, 1.0))*/
     val s1: SetOfVectors[Rational] = SetOfVectors(
          Vector(1, 2, 3, 4, 5),
          Vector(8, 8, 7, 2, 3),
          Vector(-8, 9, -3, 0, 1))
     println("Rows: " + s1.getRows())
     println("Cols: " + s1.getColumns())
     println(s1)
     println(s1.get(1,2)) //should be 9
     s1.getColumn(1).set(1)(333)
     println("After (1,1) == 333: " + s1)
     s1.set(0,0)(111)
     println("After (0,0) == 111: " + s1)
     println(s1.get(0,0))
     println(s1.getRow(0))

     println("copy: " + s1.copy())


     // set vec linalg things
     val vset1: SetOfVectors[Real] = SetOfVectors(
          Vector(1.0,2.0,0.0,2.0),Vector(3.0,6.0,0.0,6.0), Vector(-2.0,-5.0,5.0,0.0), Vector(0.0,-2.0,10.0,8.0),
          Vector(2.0,4.0,0.0,4.0), Vector(0.0,-3.0,15.0,18.0)
     )
     val b: Vector[Real] = Vector(0.0,-1.0,5.0,6.0)
     println("\n\nvset1: " + vset1)

     val aug1: AugmentedMatrix[Real] = AugmentedMatrix(vset1, b)
     println("Augmented: " + aug1)

     val augref1 = aug1.reducedEchelon()
     println("Aug rref: " + augref1)
     println("Augmented solved: " + aug1.solve())





     val vset2: SetOfVectors[Rational] = SetOfVectors(
          Vector(1,2,0,2),Vector(3,6,0,6), Vector(-2,-5,5,0), Vector(0,-2,10,8),
          Vector(2,4,0,4), Vector(0,-3,15,18)
     )
     val b2: Vector[Rational] = Vector(0,-1,5,6)
     println("\n\nvset2: " + vset2)

     val aug2: AugmentedMatrix[Rational] = AugmentedMatrix(vset2, b2)
     println("Augmented2: " + aug2)

     val augref2 = aug2.reducedEchelon()
     println("Aug rref2: " + augref2)
     println("Augmented solved: " + aug2.solve())



     val mat1A: Matrix[Rational] = Matrix(Vector(1,2,-1), Vector(3,-1,2),  Vector(2,-1,1))
     val v1A: Vector[Rational] = Vector(5,1,3)
     val aug1A = AugmentedMatrix[Rational](mat1A, v1A)
     println(aug1A.reducedEchelon())
     Console.println("1A: " + aug1A.solve())

     val mat3: Matrix[Rational] = Matrix(
          Vector(-1,1,2), Vector(3,2,1), Vector(2,-3,-2)
     )
     val b3: Vector[Rational] = Vector(1,-9, -3)
     val aug3 = AugmentedMatrix[Rational](mat3, b3)
     println(aug3.reducedEchelon())
     println(aug3.echelon())
     println("Example 2: " + aug3.solve())


     val mat4: Matrix[Rational] = Matrix(Vector(3,2,0),
          Vector(-1,1,-5), Vector(5,3,1))
     println(mat4.echelon())


     val mat5: Matrix[Rational] = Matrix(Vector(1,2,-3,4),
          Vector(1,-1,2,1), Vector(3,0,1,6), Vector(1,1,-2,1),
          Vector(6,-1,1,3))
     val aug5: AugmentedMatrix[Rational] = AugmentedMatrix(mat5, Vector[Rational](0,0,0,0))
     println("Aug 5 solve: " + aug5.solve()) //this is the basis for the nullspace: solution to Ax = 0



     val mat6: SetOfVectors[Rational] = SetOfVectors(
          Vector(1,2,3),
          Vector(4,5,6),
          Vector(7,8,9)
     )
     //TODO make basis return Seq of type V
     println(mat6)
     println(mat6.reducedEchelon())
     println("mat6 Basis: " + mat6.basis())



     val mat7: SetOfVectors[Rational] = SetOfVectors(
          Vector(-1,1,-1,1), Vector(-5,-9,-7,-1),
          Vector(0,7,1,3), Vector(-6,-1,-7,3), Vector(2,5,3,1)
     )
     println("mat7: " + mat7)
     println(mat7.reducedEchelon())
     println("mat7 transpose rref: " + mat7.transpose().reducedEchelon())
     println("mat7 basis: " + mat7.basis())


     val mat8: SetOfVectors[Rational] = SetOfVectors(
          Vector(0,2,-1), Vector(2,2,0), Vector(6,16,-5)
     )
     println("span equality: ")
     println(mat8.span())
     println(SetOfVectors(mat8.getColumnsAt(0,1):_*).span())



     val mat9: SetOfVectors[Rational] = SetOfVectors(
          Vector(1,0,-2,3), Vector(0,1,2,3), Vector(2,-2,-8,0),
          Vector(2,-1,10,3), Vector(3,-1,-6,9)
     )
     println("mat 9 basis for space spanned by the col vecs: ")
     println(mat9.basis())
     println("is basis: " + mat9.isBasisOfSet(mat9.basis()))
     // row approach form (question 4 in part exercises 3.5)
     println("transpose: " + mat9.transpose())
     //val res = mat9.transpose().reducedEchelon()
     //println("is other way basis: " + SetOfVectors(res.transpose().getColumnsAt(1,2,3,4):_*).isBasisOfSet())




     val mat10: SetOfVectors[Rational] = SetOfVectors(
          Vector(1,2,3),
          Vector(-1,0,1),
          Vector(4,9,7)
     )
     println(mat10.reducedEchelon())
     println(mat10.basis())


     val mat11: SetOfVectors[Rational] = mat4.transpose()
     println("Mat11: " + mat11) //example 3.44 david poole
     println("basis: " + mat11.basis()) //TODO rename to be 'basis of set' and make new methods to be 'basis of space'
     println("check: " + mat11.isBasisOfSet(mat11.basis()))
     println("row method basis: " + mat11.alternateBasis())
     println("check: " + mat11.isBasisOfSet(mat11.alternateBasis()))
     println("row echelon of mat4: " + mat4.echelon())



     // TESTING spanning set method from david lay execises 4.3 book
     val sv1: SetOfVectors[Rational] = SetOfVectors(Vector(1,0,0), Vector(1,1,0), Vector(1,1,1))
     assert(sv1.isSpanningSpace(3), "setvec 1 - span TRUE")

     val sv2: SetOfVectors[Rational] = SetOfVectors(Vector(1,1,0), Vector(0,0,0), Vector(0,1,1))
     assert( ! sv2.isSpanningSpace(3), "setvec 2 - span FALSE")

     val sv3: SetOfVectors[Rational] = SetOfVectors(Vector(1,0,-3), Vector(3,1,-4), Vector(-2,-1,1))
     assert( ! sv3.isSpanningSpace(3), "setvec 3 - span FALSE")

     val sv4: SetOfVectors[Rational] = SetOfVectors(Vector(2,-1,1), Vector(2,-3,2), Vector(-8,5,4))
     assert(sv4.isSpanningSpace(3), "setvec 4 - span TRUE")

     val sv5: SetOfVectors[Rational] = SetOfVectors(Vector(3,-3,0), Vector(-3,7,0), Vector(0,0,0),
          Vector(0,-3,5))
     assert(sv5.isSpanningSpace(3), "setvec 5 - span TRUE")

     val sv6: SetOfVectors[Rational] = SetOfVectors(Vector[Rational](1,2,-4), Vector[Rational](-4,3,6))
     assert( ! sv6.isSpanningSpace(3), "setvec 6 - span FALSE")

     val sv7: SetOfVectors[Rational] = SetOfVectors(Vector[Rational](-2,3,0), Vector[Rational](6,-1,5))
     assert( ! sv7.isSpanningSpace(3), "setvec 7 - span FALSE")

     val sv8: SetOfVectors[Rational] = SetOfVectors(Vector(1,-2,3), Vector(0,3,-1), Vector(2,-1,5),
          Vector(0,0,-1))
     assert(sv8.isSpanningSpace(3), "setvec 8 - span TRUE")



     val uneven: SetOfVectors[Rational] = SetOfVectors(Vector(1,2,8,4,-1), Vector(1,1,-1,0,2),
          Vector(6,3,-2,0,5))
     val unevenVec: Vector[Rational] = Vector(1,2,3)
     val augUneven: AugmentedMatrix[Rational] = AugmentedMatrix(uneven, unevenVec)
     println(augUneven)
     println(augUneven.rrefEntire)
}



/*
// todo:
// 1) testing simple rref for square system
// 2) testing non-square inverse using toRow sedenion (gauss-jordan)
// 3) testing solving system (for a vector)



//error fixed 1/3 on bottom
val mat = new Matrix[Rational](
     Vector(1, 2, 0, 2),
     Vector(3, 6, 0, 6),
     Vector(-2, -5, 5, 0),
     Vector(0, -2, 10, 8),
     Vector(2, 4, 0, 4),
     Vector(0, -3, 15, 18))
val b = Vector[Rational](0, -1, 5, 6)
val aug = new AugmentedMatrix[Rational](mat, b)
print(aug.solve())


-- arbitrary test
val mat = new Matrix[Rational](
               Vector(3, 6, -3),
               Vector(2, 4, -2),
               Vector(-1, -2, 1)
          )
          val b = Vector[Rational](2, 4, -2)
          val aug = new AugmentedMatrix[Rational](mat, b)
          print(aug.solve())


val mat1: Matrix[Complex] = Matrix(Vector(1,3,6), Vector(2, -1, 5), Vector(3, 0, 1), Vector(1, 0, 0), Vector(0,
     1, 0), Vector(0, 0, 1))
val mat2: Matrix[Complex] = Matrix(
     Vector(1, 3, 6, 5),
     Vector(2, -1, 5, -1),
     Vector(3, 0, 1, 0),
     Vector(1, 0, 0, 0),
     Vector(0, 1, 0, 0),
     Vector(0, 0, 1, 0))
val mat3: Matrix[Complex] = Matrix(Vector(1, 3, 6), Vector(2, -1, 5), Vector(3,0,1), Vector(1,4,8))
val mat4: Matrix[Rational] = Matrix(Vector(1,3,6), Vector(2, -1, 5), Vector(3, 0, 1))

println("test 1: simple rref for square: " + mat1.reducedRowEchelon())
println("\ntest 2: gauss jordan for non-square: " + mat2.reducedRowEchelon())
println("\ntest 3: solving for system: " + mat3.reducedRowEchelon())

val v1: Vector[Complex] = new Vector[Complex](Complex(1,2), Complex(2,0))
val v2: Vector[Rational] = new Vector(Rational(1,2), Rational(2,4))
zeroer(v1.get(0))
zeroer(v2.get(0))
def zeroer[N <: Number[N]: TypeTag](arg: N) = {
     println(Number.ZERO[N] + ", type: " + Number.ZERO[N].getClass.getSimpleName + "; insideType: " + arg.insideType)
}




//todo 1) testing kernel
val mat5A: VectorSet[Rational] = VectorSet(Vector[Rational](1,2),
Vector[Rational](-4,5), Vector[Rational](-9, -7))
val z5: Vector[Rational] = Vector.ZERO[Rational](2)
val aug = new AugmentedMatrix[Rational](mat5A, z5)
Console.println(aug.solve())
Console.println(new NullSpace(mat5A).kernel())

val mat5B: Matrix[Rational] = new Matrix(Vector[Rational](1,2,-14), Vector[Rational](3, 5,-37))
Console.println(new NullSpace(mat5B).kernel())

val mat5C: Matrix[Rational] = new Matrix(Vector[Rational](1,2,1),
Vector[Rational](3, 6, 3), Vector[Rational](-9,7,-8), Vector[Rational](5,1,1))
Console.println(new NullSpace(mat5C).kernel())


val mat3A: Matrix[Rational] = new Matrix(Vector[Rational](1,4,7),
Vector[Rational](-2, -5, -8), Vector[Rational](-3,-6,-9))
Console.println("mat3A: " + new NullSpace(mat3A).kernel())

val mat3B: Matrix[Rational] = new Matrix(Vector[Rational](2,4,8),
Vector[Rational](-2,-4,-8), Vector[Rational](-2,-4,-8))
Console.println("mat3B: " + new NullSpace(mat3B).kernel())

val mat3C: Matrix[Rational] = new Matrix(Vector[Rational](2, 5, 9),
Vector[Rational](9, 6, 8), Vector[Rational](-3, -1, -9))
Console.println("mat3C: " + new NullSpace(mat3C).kernel())

val mat3D: Matrix[Rational] = new Matrix(Vector[Rational](-3, 2, 4),
Vector[Rational](1, 5, 8), Vector[Rational](-1, -7, -4))
Console.println("mat3D: " + new NullSpace(mat3D).kernel())



//todo 2) testing solve augmented matrix
val mat1A: Matrix[Rational] = Matrix(Vector[Rational](1,2,-1), Vector[Rational](3,-1,2),
     Vector[Rational](2,-1,1))
val v1A: Vector[Rational] = Vector[Rational](5,1,3)
val aug1A = new AugmentedMatrix[Rational](mat1A, v1A)
Console.println("1A: " + aug1A.solve())

val mat1B: Matrix[Rational] = Matrix(Vector[Rational](-1,3,4), Vector[Rational](1,-2,-1),
     Vector[Rational](1,5,-2))
val v1B: Vector[Rational] = Vector[Rational](0,0,0)
val aug1B = new AugmentedMatrix[Rational](mat1B, v1B)
Console.println("1B: " + aug1B.solve())

val mat1C: Matrix[Rational] = Matrix(Vector[Rational](-1,2,6), Vector[Rational](1,2,6),
     Vector[Rational](1,3,9))
val v1C: Vector[Rational] = Vector[Rational](2,5,7)
val aug1C = new AugmentedMatrix[Rational](mat1C, v1C)
Console.println("1C: " + aug1C.solve())

val mat1D: Matrix[Rational] = Matrix(Vector[Rational](1,1,3), Vector[Rational](1,2,3),
     Vector[Rational](-1,1,-3))
val v1D: Vector[Rational] = Vector[Rational](2,4,6)
val aug1D = new AugmentedMatrix[Rational](mat1D, v1D)
Console.println("1D: " + aug1D.solve())

val mat1E: Matrix[Rational] = Matrix(Vector[Rational](3,6,1,2), Vector[Rational](-3,-7,-1,-2),
     Vector[Rational](-1,1,-2,6), Vector[Rational](2, 1, -1, 8))
val v1E: Vector[Rational] = Vector[Rational](0,0,0, 0)
val aug1E = new AugmentedMatrix[Rational](mat1E, v1E)
Console.println("1E: " + aug1E.solve())

val mat1F: Matrix[Rational] = Matrix(Vector[Rational](2,2,8,1), Vector[Rational](3,3,12,2),
     Vector[Rational](5,2,20,4), Vector[Rational](2,2,8,5))
val v1F: Vector[Rational] = Vector[Rational](6,7,24,6)
val aug1F = new AugmentedMatrix[Rational](mat1F, v1F)
Console.println("1F: " + aug1F.solve())

val mat1G: Matrix[Rational] = Matrix(Vector[Rational](0,5,10), Vector[Rational](-10,6,7),
     Vector[Rational](0,-8,3), Vector[Rational](38,4,8))
val v1G: Vector[Rational] = Vector[Rational](6,3,9)
val aug1G = new AugmentedMatrix[Rational](mat1G, v1G)
Console.println("1G: " + aug1G.solve())

val mat1H: Matrix[Rational] = Matrix(Vector[Rational](0,3,15,12), Vector[Rational](-1,-4,-20,-16),
     Vector[Rational](0,1,2,7), Vector[Rational](0,6,30,24), Vector[Rational](5,7,3,60))
val v1H: Vector[Rational] = Vector[Rational](3,5,-1,10)
val aug1H = new AugmentedMatrix[Rational](mat1H, v1H)
Console.println("1H: " + aug1H.solve())

val mat1I: Matrix[Rational] = Matrix(Vector[Rational](0,3,15,12), Vector[Rational](-1,-4,-20,-16),
     Vector[Rational](0,1,2,7), Vector[Rational](0,6,30,24), Vector[Rational](5,7,3,60))
val v1I: Vector[Rational] = Vector[Rational](3,5,-1,46)
val aug1I = new AugmentedMatrix[Rational](mat1I, v1I)
Console.println(aug1I)
Console.println("1I: " + aug1I.solve())

val mat1J: Matrix[Rational] = Matrix(Vector[Rational](2,1,1,2), Vector[Rational](-1,7,-2,-6),
     Vector[Rational](3,6,6,0), Vector[Rational](5,-11,4,14), Vector[Rational](5,7,1,2))
val v1J: Vector[Rational] = new Vector(Rational(1),Rational(-8), Rational(5),Rational(20,3))
val aug1J = new AugmentedMatrix[Rational](mat1J, v1J)
Console.println("1J: " + aug1J.solve())


val mat: Matrix[Rational] = Matrix(Vector[Rational](1,2,0,2), Vector[Rational](3,6,0,6),
     Vector[Rational](-2,-5,5,0), Vector[Rational](0,-2,10,8), Vector[Rational](2,4,0,4),
     Vector[Rational](0,-3,15,18))
Console.println(new NullSpace[Rational](mat).kernel())

//todo rowspace/colspace tester
val mat1E: Matrix[Rational] = Matrix(
               Vector[Rational](1,2,2,-1),
               Vector[Rational](-3,-6,-6,3),
               Vector[Rational](4, 9,9,-4),
               Vector[Rational](-2,-1,-1,2),
               Vector[Rational](5,8,9,-5),
               Vector[Rational](4,2,7,-4))




//todo 3) testing spanned methods now
val vset: VectorSet[Rational] = new VectorSet(Vector[Rational](1,2,3), Vector[Rational](1,5,4),
Vector[Rational](0,0,9))
val uset: VectorSet[Rational] = new VectorSet(Vector[Rational](1,4,7), Vector[Rational](2,5,8),
Vector[Rational](3,6,9))
val v = Vector[Rational](1,3,4)
Console.println(vset)
Console.println(vset.reducedRowEchelon())
Console.println(vset.linearlyIndependent())
Console.println(vset.spansSpaceWith(vset.dimension()))
Console.println(vset.isSpanned(v))
Console.println(vset.getSpanningCoefficients(v))

Console.println("\n\n" + uset)
Console.println(uset.reducedRowEchelon())
Console.println(uset.linearlyIndependent())
Console.println(uset.spansSpaceWith(uset.dimension()))
Console.println(uset.isSpanned(v))
Console.println(uset.getSpanningCoefficients(v))


val wset: VectorSet[Rational] = new VectorSet(Vector[Rational](9,4,-2), Vector[Rational](-1,1,-2),
Vector[Rational](3,3,1))
val w = Vector[Rational](3,4,5)
Console.println(wset.isSpanned(w))
Console.println(wset.getSpanningCoefficients(w))
*/
