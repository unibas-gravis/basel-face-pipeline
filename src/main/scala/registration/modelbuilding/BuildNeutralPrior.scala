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
import ch.unibas.cs.gravis.facepipeline.BU3DDataProvider._
import ch.unibas.cs.gravis.facepipeline._
import com.typesafe.scalalogging.StrictLogging
import scalismo.common._
import scalismo.geometry.{Vector, _3D}
import scalismo.numerics.PivotedCholesky
import scalismo.numerics.PivotedCholesky.StoppingCriterion
import scalismo.statisticalmodel._

case class BuildNeutralPrior(dataProvider: DataProvider) extends PipelineStep with StrictLogging {


  def approximatePointSet(points: UnstructuredPointsDomain[_3D], D: Double, gp : GaussianProcess[_3D,Vector[_3D]], sc: StoppingCriterion) : (DiscreteLowRankGaussianProcess[_3D,Vector[_3D]],UnstructuredPointsDomain[_3D]) = {

    def phiWithDim(i: Int, dim : Int, ptId : Int, phi: DenseMatrix[Double]) = {
      phi(ptId*3 + dim,i)
    }

    def phiVec(i : Int, ptID : PointId,phi : DenseMatrix[Double]) = {
      Vector(phiWithDim(i,0,ptID.id,phi),phiWithDim(i,1,ptID.id,phi),phiWithDim(i,2,ptID.id,phi))
    }
    val (phi,lambda) = PivotedCholesky.computeApproximateEig(gp.cov,points.points.toIndexedSeq,D,sc)

    val nPhi = phi.cols

    val klBasis: DiscreteLowRankGaussianProcess.KLBasis[_3D, Vector[_3D]] = for(i <- 0 until nPhi) yield {
      val v = DiscreteField[_3D,Vector[_3D]](points,points.pointsWithId.toIndexedSeq.map(f => phiVec(i,f._2,phi)))
      DiscreteLowRankGaussianProcess.Eigenpair(lambda(i),v)
    }
    val mean = DiscreteField[_3D,Vector[_3D]](points,points.points.toIndexedSeq.map(p => gp.mean(p)))

    val r = DiscreteLowRankGaussianProcess[_3D,Vector[_3D]](mean, klBasis)
    (r,points)

  }


  override def run(): Unit = {

    scalismo.initialize()

    logger.info(s"building model for neutral expression")
    val referenceMesh = dataProvider.incoming.reference.loadMesh(Neutral).get
    val mask = dataProvider.incoming.reference.loadFaceMask().get
    val faceKernel = FaceKernel(mask,referenceMesh)
    val gp = GaussianProcess[_3D, Vector[_3D]](faceKernel)
    val ldg = approximatePointSet(referenceMesh.pointSet, 1.0, gp, PivotedCholesky.NumberOfEigenfunctions(1000))
    val lowRankGaussianProcess = ldg._1.interpolateNearestNeighbor
    logger.info("computed nystrom approximation")

    val model = StatisticalMeshModel(referenceMesh, lowRankGaussianProcess)

    dataProvider.registration.savePriorModel(model, Neutral)

    logger.info("model building done")

  }

}

object BuildNeutralPrior {

  def main(args: Array[String]): Unit = {
    BuildNeutralPrior(BU3DDataProvider).run()
  }

}

