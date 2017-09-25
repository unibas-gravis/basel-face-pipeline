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

package registration.utils

import java.awt.Color

import breeze.linalg.DenseVector
import scalismo.geometry._3D
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.StatisticalMeshModel
import scalismo.ui.api._

object VisualLogger {
  var ui : Option[ScalismoUI] = None//Some(ScalismoUI("Visual Logger"))

  val modelGroup = ui.map(_.createGroup("Model"))
  var modelView : Option[StatisticalMeshModelViewControls] = None

  val targetGroup = ui.map(_.createGroup("Target"))
  var targetMeshView : Option[TriangleMeshView] = None



  def showTargetMesh(targetMesh : TriangleMesh[_3D]) : Unit = {
    remove(targetMeshView)
    targetMeshView = show(VisualLogger.targetGroup, targetMesh, "target")
    targetMeshView.map(_.color = Color.RED)
  }

  def showStatisticalShapeModel(ssm : StatisticalMeshModel) : Unit = {
    removeModel(modelView)
    modelView = show(modelGroup, ssm, "gpmodel")
    modelView.map(_.meshView.opacity = 0.7)
  }

  def updateModelView(coeffs : DenseVector[Double]) : Unit = {
    if (modelView.isDefined) {
      modelView.get.shapeModelTransformationView.shapeTransformationView.coefficients = coeffs
    }
  }


  private def show[A](group : Option[Group], t : A, name : String)(implicit sic : ShowInScene[A]): Option[sic.View] = {
    for {
      ui <- ui
      g <- group
    } yield {
      ui.show(g, t, name)
    }
  }

  def remove[V <: ObjectView](view : Option[V]): Unit = {
    view.foreach(_.remove())
  }

  def removeModel(view : Option[StatisticalMeshModelViewControls]): Unit = {
    for {v <- view} {
      v.meshView.remove()
      v.shapeModelTransformationView.remove()
    }
  }

}