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

package registration.experiments

import ch.unibas.cs.gravis.facepipeline.BU3DDataProvider.Neutral
import ch.unibas.cs.gravis.facepipeline.BU3DDataProvider._
import scalismo.geometry.{Landmark,_3D}
import scalismo.io.LandmarkIO
import java.io.File
import breeze.linalg.DenseVector
import breeze.numerics.sqrt
import ch.unibas.cs.gravis.facepipeline.BU3DDataProvider
import scalismo.common.PointId

case class FeaturePoints(f : Feature, points : Seq[PointId], lms: Seq[Landmark[_3D]])

case class Feature(lmids: Seq[Int],name: String) {
  override def toString: String = name
}

object Bu3DFELandmarkEvaluation {

  def main(args: Array[String]): Unit = {

    scalismo.initialize()

    val dataProvider = BU3DDataProvider

    val eyeLeft = Feature(Seq(2,3,4,6,7,8),"eye.left")
    val eyeRight = Feature(Seq(10,11,12,14,15,16),"eye.right")
    val eyebrowLeft = Feature((17 to 26),"eyebrow.left")
    val eyebrowRight = Feature((27 to 36),"eyebrow.right")
    val nose = Feature((37 to 47),"nose")
    val mouth = Feature((49 to 60),"mouth")
    val faceRight = Feature((69 to 74),"face.right")
    val faceLeft = Feature((78 to 83),"face.left")
    val chin = Feature((75 to 77),"chin")

    val features = Seq(eyeLeft,eyeRight,eyebrowLeft,eyebrowRight,nose,mouth,faceRight,faceLeft,chin)

    val referenceMesh = dataProvider.incoming.reference.loadMesh(Neutral).get

    val bu3DreferenceLandmarks = {
      LandmarkIO.readLandmarksCsv[_3D](new File(dataProvider.repositoryRoot.jfile,"/data/incoming/reference/landmarks/mean2012-bu3dfe-eval-landmarks.csv")).get
    }

    val featuresWithLandmarks = features.map{
      f =>
        val ids = f.lmids
        val lms = ids.map(id => bu3DreferenceLandmarks.find(lm => lm.id.replace("bu3dfe-lm-","") == s"$id").get)
        val points = lms.map(lm => referenceMesh.pointSet.findClosestPoint(lm.point).id)
        FeaturePoints(f,points,lms)
    }

    val finalRes = for (expression <- Seq(Neutral,Sadness,Joy,Disgust,Anger,Fear,Surprise).reverse) yield {

      val evalPerSample = for (id <- dataProvider.incoming.ids(expression) if dataProvider.incoming.loadLandmarks(id,expression, F3D, BU3DDataProvider.Aligned).isSuccess && dataProvider.incoming.loadLandmarks(id, expression).isSuccess) yield {

        val bu3Dlandmarks = dataProvider.incoming.loadLandmarks(id, expression, F3D, BU3DDataProvider.Aligned).get
        val registeredMesh = dataProvider.registration.loadMesh(id, expression).get

        val alldists = for (f <- featuresWithLandmarks) yield {

          val dists = for (id <- f.lms.zip(f.points)) yield {

            val registeredPoint = registeredMesh.pointSet.point(id._2)
            val gtPoint = bu3Dlandmarks.find(lm => lm.id == id._1.id).get.point

            val dist = sqrt((registeredPoint - gtPoint).norm2)
            dist

          }

          (dists.sum / dists.length)

        }

        alldists

      }

      evalPerSample

    }

    println("Landmark Evaluation Result:")

    val table = for((f,i) <- features.zipWithIndex) yield {
      val data = DenseVector(finalRes.map(d => d.map(_(i))).flatten.toArray)
      val error = breeze.stats.meanAndVariance(data)
      Seq(f.name,s"${error.mean}",s"${error.stdDev}")
    }

    println(Tabulator.format(Seq(Seq("Region","Mean","Std")) ++ table))

  }
}

object Tabulator {
  def format(table: Seq[Seq[Any]]) = table match {
    case Seq() => ""
    case _ =>
      val sizes = for (row <- table) yield (for (cell <- row) yield if (cell == null) 0 else cell.toString.length)
      val colSizes = for (col <- sizes.transpose) yield col.max
      val rows = for (row <- table) yield formatRow(row, colSizes)
      formatRows(rowSeparator(colSizes), rows)
  }

  def formatRows(rowSeparator: String, rows: Seq[String]): String = (
    rowSeparator ::
      rows.head ::
      rowSeparator ::
      rows.tail.toList :::
      rowSeparator ::
      List()).mkString("\n")

  def formatRow(row: Seq[Any], colSizes: Seq[Int]) = {
    val cells = (for ((item, size) <- row.zip(colSizes)) yield if (size == 0) "" else ("%" + size + "s").format(item))
    cells.mkString("|", "|", "|")
  }

  def rowSeparator(colSizes: Seq[Int]) = colSizes map { "-" * _ } mkString("+", "+", "+")
}

