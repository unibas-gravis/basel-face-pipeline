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
import scalismo.faces.io.MoMoIO
import scalismo.faces.momo.MoMo
import scalismo.faces.sampling.face.MoMoRenderer
import scalismo.utils.Random

import scala.reflect.io.Path

object QualitativeLFW extends App{
  scalismo.initialize()
  val seed = 1986L
  implicit val rnd = new Random(seed)

  def fitModel(model:MoMo, modelName: String) = {
    val targetsPath =  BU3DDataProvider.repositoryRoot + "/recognition-experiment/fit-lfw-qualitative/lfwSelection/"
    val outPath =  BU3DDataProvider.repositoryRoot + "/recognition-experiment/fit-lfw-qualitative/lfwResults/" + modelName + "/"


    val files = new File(targetsPath).listFiles.filter(_.getName.endsWith(".png"))
    val listTarget = files.map(p => p.getName.substring(0, p.getName.length - 4)).toList



    listTarget.foreach{ targetName =>
      val outPathTarget = outPath + targetName + "/"

      if (!Path(outPathTarget).exists) {
        try {
          Path(outPathTarget).createDirectory(failIfExists = false)

          val renderer = MoMoRenderer(model, RGBA.BlackTransparent).cached(5)

          val targetFn = targetsPath + targetName + ".png"
          val targetLM = targetsPath + targetName + "_face0.tlms"

          StandardFitScript.fit(targetFn, targetLM, outPathTarget, renderer)
        }
      }
    }
  }

  val bfm = MoMoIO.read(new File( BU3DDataProvider.repositoryRoot + "/data/modelbuilding/model/model2017-1_face12_nomouth.h5")).get
  val bfmOld = MoMoIO.read(new File(BU3DDataProvider.repositoryRoot + "/model2009-face12.h5")).get
  val bu3d = MoMoIO.read(new File(BU3DDataProvider.repositoryRoot + "/data/modelbuilding/model/bu3d-face12_nomouth.h5")).get

  fitModel(bfm, "bfm")
  fitModel(bfmOld, "bfmOld")
  fitModel(bu3d, "bu3d")
}