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
package preprocessing

import breeze.linalg.{DenseMatrix, DenseVector}
import ch.unibas.cs.gravis.facepipeline.BU3DDataProvider
import ch.unibas.cs.gravis.facepipeline.BU3DDataProvider.Expressions
import ch.unibas.cs.gravis.facepipeline.{DataProvider, PipelineStep}
import scalismo.faces.io.TLMSLandmarksIO
import scalismo.statisticalmodel.MultivariateNormalDistribution

object PrepareReferenceLandmarks {

  def main(args: Array[String]): Unit = {
    scalismo.initialize()

    PrepareReferenceLandmarks(BU3DDataProvider).run()
  }

}

case class PrepareReferenceLandmarks(dataProvider : DataProvider) extends PipelineStep {

  override def run(): Unit = {

    scalismo.initialize()

    val rawRefLmsFile = (dataProvider.repositoryRoot / "data" / "incoming" / "reference" / "landmarks" / "mean2012_l7_bfm_nomouth.tlms").jfile

    val referenceLandmarksTLMS = TLMSLandmarksIO.read3D(rawRefLmsFile).get
    val referenceLandmarks = for (lmTlms <- referenceLandmarksTLMS if lmTlms.visible) yield {
      val lm = lmTlms.toLandmark
      val noiseVariance = lm.id.trim match {
        case lmid if lmid.contains("eyebrow") => 3.0
        case lmid if lmid.contains("eye.bottom") => 3.0
        case lmid if lmid.contains("eye.top") => 3.0
        case _ => 1.0
      }
      lm.copy(uncertainty = Some(MultivariateNormalDistribution(DenseVector.zeros[Double](3), DenseMatrix.eye[Double](3) * noiseVariance)))
    }

    // Transfer the reference landmarks to all the expressions and save them.
    for (expression <- Expressions.expressionModelTypes()) {

      val neutralRef = dataProvider.incoming.reference.loadMesh(dataProvider.Neutral).get
      val expressionRef = dataProvider.registration.loadPriorModel(expression).get.referenceMesh
      val expressionLms = for (lm <- referenceLandmarks) yield {
        val id = neutralRef.pointSet.findClosestPoint(lm.point).id
        lm.copy(point = expressionRef.pointSet.point(id))
      }

      dataProvider.incoming.reference.saveLandmarks(expression, expressionLms)
    }
  }

}