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

import ch.unibas.cs.gravis.facepipeline.BU3DDataProvider._
import ch.unibas.cs.gravis.facepipeline.{BU3DDataProvider, DataProvider, PipelineStep}
import com.typesafe.scalalogging.StrictLogging
import scalismo.common._
import scalismo.geometry.{Point, Vector, _3D}
import scalismo.statisticalmodel._

case class BuildCoreExpressionPrior(dataProvider: DataProvider) extends PipelineStep with StrictLogging {

  def run(): Unit = {

    scalismo.initialize()

    val dataProvider = BU3DDataProvider

    logger.info("load reference mesh ...")

    val referenceMesh = dataProvider.incoming.reference.loadMesh(Neutral).get

    logger.info("make core model from expressions ...")

    val references = for(exp <- Seq(Neutral, Sadness, Joy, Disgust, Anger, Fear, Surprise) ) yield {

      val expression = dataProvider.incoming.reference.loadMesh(exp).get

      def t(p : Point[_3D]) : Vector[_3D] = {
        val id = referenceMesh.pointSet.findClosestPoint(p).id
        val cp = expression.pointSet.point(id)
        cp - p
      }

      Field[_3D,Vector[_3D]](RealSpace[_3D],t)

    }

    logger.info("augment neutral with core model ...")

    val ssm = StatisticalMeshModel.createUsingPCA(referenceMesh,references)
    val neutralModel = dataProvider.registration.loadPriorModel(Neutral).get
    val augmentedExpressionModel = StatisticalMeshModel.augmentModel(ssm,neutralModel.gp.interpolateNearestNeighbor)

    logger.info("save core model ...")

    dataProvider.registration.savePriorModel(augmentedExpressionModel, dataProvider.CoreExpression)

  }

}

object BuildCoreExpressionPrior {

  def main(args: Array[String]): Unit = {

    BuildCoreExpressionPrior(BU3DDataProvider).run()

  }

}
