/*
 * Copyright University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package registration.modelbuilding

import breeze.linalg.DenseMatrix
import registration.modelbuilding.FaceKernel.LevelWithScale
import scalismo.common._
import scalismo.geometry.{Point, SquareMatrix, _3D}
import scalismo.kernels.{BSplineKernel, DiagonalKernel, MatrixValuedPDKernel}
import scalismo.mesh.TriangleMesh

case class SpatiallyVaryingMultiscaleKernel(levelsWithScale : Seq[LevelWithScale],
                                            mask: FaceMask, referenceMesh: TriangleMesh[_3D]) extends MatrixValuedPDKernel[_3D] {


  val bSplineKernel = DiagonalKernel(BSplineKernel[_3D](order = 3, scale = 0), 3)

  val smoothedRegionWeights = levelsWithScale.map(levelWithScale =>
    (levelWithScale.level, mask.computeSmoothedRegions(referenceMesh,levelWithScale.level, 40))
  ).toMap

  def k(x: Point[_3D], y: Point[_3D]): DenseMatrix[Double] = {

    var sum = SquareMatrix.zeros[_3D].toBreezeMatrix

    for (LevelWithScale(level, scale) <- levelsWithScale) {

      val weightX = smoothedRegionWeights(level)(x)
      val weightY = smoothedRegionWeights(level)(y)

      sum += bSplineKernel((x.toVector * Math.pow(2, level)).toPoint, (y.toVector * Math.pow(2, level)).toPoint) * scale * weightX * weightY

    }

    sum
  }

  override def outputDim = 3

  override def domain = RealSpace[_3D]

}

case class FaceKernel(faceMask : FaceMask, referenceMesh: TriangleMesh[_3D]) extends MatrixValuedPDKernel[_3D] {


  private val faceKernel = {

    val levelsAndScales =   Seq(
      LevelWithScale(-6,128.0),
      LevelWithScale(-5, 64.0),
      LevelWithScale(-4, 32.0),
      LevelWithScale(-3, 10.0),
      LevelWithScale(-2, 4.0))

    val spatiallyVaryingKernel = SpatiallyVaryingMultiscaleKernel(levelsAndScales, faceMask, referenceMesh)

    val symmetricKernel = symmetrize(spatiallyVaryingKernel)

    symmetricKernel * 0.7 + spatiallyVaryingKernel * 0.3
  }

  override protected def k(x: Point[_3D], y: Point[_3D]): DenseMatrix[Double] = faceKernel(x,y)

  override def domain: Domain[_3D] = RealSpace[_3D]

  override def outputDim: Int = 3

  private def symmetrize(kernel: MatrixValuedPDKernel[_3D]) :  MatrixValuedPDKernel[_3D] = {

    new MatrixValuedPDKernel[_3D] {
      override def outputDim = 3

      override def k(x: Point[_3D], y: Point[_3D]): DenseMatrix[Double] = {

        val ybar = Point(-y.x, y.y, y.z)
        val xbar = Point(-x.x, x.y, x.z)

        val I = DenseMatrix.eye[Double](3)
        I(0, 0) = 1

        val IBar = DenseMatrix.eye[Double](3)
        IBar(0, 0) = -1

        I * kernel(x, y) + IBar * (kernel(x, ybar))
      }

      override def domain: Domain[_3D] = kernel.domain
    }

  }


}

object FaceKernel {
  case class LevelWithScale(level : Int, scale : Double)
}
