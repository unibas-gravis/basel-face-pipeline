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

package fitting.experiments

import java.io.File

import ch.unibas.cs.gravis.facepipeline.BU3DDataProvider
import fitting.StandardFitScript
import scalismo.faces.color.RGBA
import scalismo.faces.io.{MoMoIO, RenderParameterIO}
import scalismo.faces.momo.MoMo
import scalismo.faces.sampling.face.MoMoRenderer
import scalismo.utils.Random

import scala.reflect.io.Path

// script to fit multiPie Neutral Samples with landmarks
object RecognitionMultiPiePose extends App{
  scalismo.initialize()
  val seed = 1986L
  implicit val rnd = Random(seed)

  def fitModel(model:MoMo, modelName: String) = {
    val targetsPath = BU3DDataProvider.repositoryRoot + "/recognition-experiment"
    val outPath = targetsPath + "/results/" + modelName + "/"


    val files = new File(targetsPath + "/originals/").listFiles.filter(_.getName.endsWith(".png"))
    val listTarget = files.map(p => p.getName.substring(0, p.getName.length - 4)).toList



    listTarget.foreach(targetName => {
      val outPathTarget = outPath + targetName + "/"

      if (!Path(outPathTarget).exists) {
        try {
          Path(outPathTarget).createDirectory(failIfExists = false)

          val renderer = MoMoRenderer(model, RGBA.BlackTransparent).cached(5)

          val targetFn = targetsPath + "/originals/" +  targetName + ".png"
          val targetLM = targetsPath + "/landmarks/" + targetName + "_face0.tlms"

          StandardFitScript.fit(targetFn, targetLM, outPathTarget, renderer, false)
        }
      }
    })
  }
  val bfm = MoMoIO.read(new File(BU3DDataProvider.repositoryRoot + "data/modelbuilding/model/model2017-1_face12_nomouth.h5")).get.neutralModel
  val bfmOld = MoMoIO.read(new File(BU3DDataProvider.repositoryRoot + "/export/faces/model/bfm2009/model2009-face12.h5")).get.neutralModel
  val bu3d = MoMoIO.read(new File(BU3DDataProvider.repositoryRoot + "/export/faces/projects/pami-ppm2017/basel-face-pipeline/data/modelbuilding/model/bu3d-face12_nomouth.h5")).get.neutralModel

  fitModel(bfm, "bfm")
  fitModel(bfmOld, "bfmOld")
  fitModel(bu3d, "bu3d")


}

// script to fit multiPie Expression Samples with landmarks
object RecognitionMultiPieExpression extends App{
  scalismo.initialize()
  val seed = 1986L
  implicit val rnd = Random(seed)

  def fitModel(model:MoMo, modelName: String) = {
    val targetsPath = "/export/faces/projects/pami-ppm2017/experiments/fit-multipie-recognition/multipie/"
    val outPath = targetsPath + "/results/" + modelName + "/"


    val files = new File(targetsPath + "/originalsExpressionsNotForPublishing").listFiles.filter(_.getName.endsWith(".png"))
    val listTarget = files.map(p => p.getName.substring(0, p.getName.length - 4)).toList



    listTarget.foreach(targetName => {
      val outPathTarget = outPath + targetName + "/"

      if (!Path(outPathTarget).exists) {
        try {
          Path(outPathTarget).createDirectory(failIfExists = false)

          val renderer = MoMoRenderer(model, RGBA.BlackTransparent).cached(5)

          val targetFn = targetsPath + "/originalsExpressionsNotForPublishing/" +  targetName + ".png"
          val targetLM = targetsPath + "landmarksExpressions/" + targetName + "_face0.tlms"

          StandardFitScript.fit(targetFn, targetLM, outPathTarget, renderer)
        }
      }
    })
  }
  val bu3dEx = MoMoIO.read(new File(BU3DDataProvider.repositoryRoot + "/data/modelbuilding/model/bu3d-face12_nomouth.h5")).get
  val bfmEx = MoMoIO.read(new File(BU3DDataProvider.repositoryRoot + "/data/modelbuilding/model/model2017-1_face12_nomouth.h5")).get

  fitModel(bfmEx, "bfmEx")
  fitModel(bu3dEx, "bu3dEx")

}

// Script to calculate recognition results over pose fitted multipie data
object RecognitionEvaluation extends App {

  case class Fit( id: String, pose: String, coeffs: IndexedSeq[Double])

  case class Match( id: String, similarity: Double)

  val models = IndexedSeq("bfmOld", "bu3d", "bfm")
  val resultsPath = BU3DDataProvider.repositoryRoot + "/recognition-experiment/results/"

  models.foreach { model =>
    val resultPath = resultsPath + model + "/"
    val files = new File(resultPath).listFiles.filter(_.isDirectory).toIndexedSeq.sortBy(_.getAbsoluteFile)
    val allFits = files.map { f =>
      val name = f.getName
      val id = name.substring(0, 3)
      val pose = name.substring(10, 13)
      val rps = RenderParameterIO.read(new File(resultPath + name + "/fit-best.rps")).get
      val coeffs = rps.momo.color ++ rps.momo.shape

      Fit(id, pose, coeffs)
    }

    val gallery = allFits.filter(fit => fit.pose == "051")

    val listOfPoses = IndexedSeq("051", "140", "130", "080")
    val listOfExperiments = listOfPoses.map { queryPose =>
      allFits.filter(fit => fit.pose == queryPose)
    }

    val queriesWithSimilarities = listOfExperiments.map { queriesInExperiment =>
      queriesInExperiment.map { query =>
        val similaritiesForQuery = gallery.map { subject =>
          Match(subject.id, cosineAngle(query.coeffs, subject.coeffs))
        }
        (query.id, similaritiesForQuery)
      }
    }

    val correctMatchesPerExperiment: IndexedSeq[Double] = queriesWithSimilarities.map { experiment =>
      val correctMatches = experiment.map { case (query_id, similarities) =>
        val bestMatch = similarities.maxBy(m => m.similarity)
        //println(query_id, bestMatch)
        if (bestMatch.id == query_id) 1.0 else 0.0
      }.sum
      correctMatches / experiment.length
    }

    println(model + correctMatchesPerExperiment)

  }


  def cosineAngle(aa: IndexedSeq[Double], bb: IndexedSeq[Double]): Double = {
    import breeze.linalg._

    val a = DenseVector(aa.toArray)
    val b = DenseVector(bb.toArray)

    (a dot b) / (norm(a) * norm(b))
  }

}

// Script to calculate recognition results over expression on fitted multipie data

object RecognitionEvaluationEx extends App {

  case class Fit( id: String, pose: String, coeffs: IndexedSeq[Double])

  case class Match( id: String, similarity: Double)

  val models = IndexedSeq("bfmEx", "bu3dEx")
  val resultsPath =  BU3DDataProvider.repositoryRoot + "/recognition-experiment/results/"

  models.foreach { model =>
    val resultPath = resultsPath + model + "/"
    val files = new File(resultPath).listFiles.filter(_.isDirectory).toIndexedSeq.sortBy(_.getAbsoluteFile)
    val allFits = files.map { f =>
      val name = f.getName
      val id = name.substring(0, 3)
      val expression = name.substring(7, 9)
      val rps = RenderParameterIO.read(new File(resultPath + name + "/fit-best.rps")).get
      val coeffs = rps.momo.color ++ rps.momo.shape

      Fit(id, expression, coeffs)
    }
    val gallery = allFits.filter(fit => fit.pose == "01")

    val listOfPoses = IndexedSeq("01","02")
    val listOfExperiments = listOfPoses.map { queryPose =>
      allFits.filter(fit => fit.pose == queryPose)
    }

    val queriesWithSimilarities = listOfExperiments.map { queriesInExperiment =>
      queriesInExperiment.map { query =>
        val similaritiesForQuery = gallery.map { subject =>
          Match(subject.id, cosineAngle(query.coeffs, subject.coeffs))
        }
        (query.id, similaritiesForQuery)
      }
    }

    val correctMatchesPerExperiment: IndexedSeq[Double] = queriesWithSimilarities.map { experiment =>
      val correctMatches = experiment.map { case (query_id, similarities) =>
        val bestMatch = similarities.maxBy(m => m.similarity)
       // println(query_id, bestMatch)
        if (bestMatch.id == query_id) 1.0 else 0.0
      }.sum
      correctMatches / experiment.length
    }

    println(model + correctMatchesPerExperiment)

  }


  def cosineAngle(aa: IndexedSeq[Double], bb: IndexedSeq[Double]): Double = {
    import breeze.linalg._

    val a = DenseVector(aa.toArray)
    val b = DenseVector(bb.toArray)

    (a dot b) / (norm(a) * norm(b))
  }

}