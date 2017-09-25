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

package ch.unibas.cs.gravis.facepipeline

import registration.modelbuilding.FaceMask
import scalismo.faces.mesh.{BinaryMask, ColorNormalMesh3D}
import scalismo.faces.momo.MoMo
import scalismo.geometry.{Landmark, _3D}
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.StatisticalMeshModel

import scala.reflect.io.Path
import scala.util.Try

trait ExpressionType {
  override def toString : String
}

trait MaskType {
  override def toString: String
}

trait DataFlag {
  override def toString: String
}

trait DataProvider {

  trait Person {
    def id: String
    def raceTag: String
  }

  trait WithMesh {
    def loadMesh(id: Person, expression: ExpressionType): Try[TriangleMesh[_3D]]
    def loadMesh(id: Person, expression: ExpressionType, mask: MaskType): Try[TriangleMesh[_3D]]
    def loadMesh(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag): Try[TriangleMesh[_3D]]
    def loadColoredMesh(id: Person, expression: ExpressionType): Try[ColorNormalMesh3D]
    def loadColoredMesh(id: Person, expression: ExpressionType, mask: MaskType): Try[ColorNormalMesh3D]
    def loadColoredMesh(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag): Try[ColorNormalMesh3D]
    def saveMesh(id: Person, expression: ExpressionType, mesh: TriangleMesh[_3D]): Try[Unit]
    def saveMesh(id: Person, expression: ExpressionType, mask: MaskType, mesh: TriangleMesh[_3D]): Try[Unit]
    def saveMesh(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag, mesh: TriangleMesh[_3D]): Try[Unit]
    def saveMesh(id: Person, expression: ExpressionType, mesh: ColorNormalMesh3D): Try[Unit]
    def saveMesh(id: Person, expression: ExpressionType, mask: MaskType, mesh: ColorNormalMesh3D): Try[Unit]
    def saveMesh(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag, mesh: ColorNormalMesh3D): Try[Unit]
  }

  trait WithLandmarks {
    def loadLandmarks(id: Person, expression: ExpressionType): Try[Seq[Landmark[_3D]]]
    def loadLandmarks(id: Person, expression: ExpressionType, mask: MaskType): Try[Seq[Landmark[_3D]]]
    def loadLandmarks(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag): Try[Seq[Landmark[_3D]]]
    def saveLandmarks(id: Person, expression: ExpressionType, landmarks: Seq[Landmark[_3D]]): Try[Unit]
    def saveLandmarks(id: Person, expression: ExpressionType, mask: MaskType, landmarks: Seq[Landmark[_3D]]): Try[Unit]
    def saveLandmarks(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag, landmarks: Seq[Landmark[_3D]]): Try[Unit]
  }

  trait WithLineLandmarks {
    def loadLineLandmarks(id: Person, expression: ExpressionType): Try[Seq[Landmark[_3D]]]
    def saveLineLandmarks(id: Person, expression: ExpressionType): Try[Seq[Landmark[_3D]]]
  }

  trait WithIds {
    def ids(expression: ExpressionType): Seq[Person]
  }

  trait Reference {
    def loadMesh(expression: ExpressionType): Try[TriangleMesh[_3D]]
    def loadFaceMask(): Try[FaceMask]
    def loadLandmarks(expression: ExpressionType): Try[Seq[Landmark[_3D]]]
    def saveLandmarks(expression: ExpressionType, landmarks : Seq[Landmark[_3D]]): Try[Unit]
    def loadLineLandmarks(expression: ExpressionType): Try[Seq[Landmark[_3D]]]
  }

  trait Incoming extends WithIds with WithMesh with WithLandmarks {

    def reference: Reference
  }

  trait SurfaceRegistration extends WithIds with WithMesh {
    def loadPriorModel(expression: ExpressionType): Try[StatisticalMeshModel]
    def savePriorModel(model: StatisticalMeshModel, expressionType: ExpressionType): Try[Unit]
  }

  trait ModelBuilding {
    def loadModel( mask: MaskType ) : Try[MoMo]
    def saveModel( mask: MaskType, momo: MoMo ) : Try[Unit]
    def loadColoredMesh(id: Person, expression: ExpressionType): Try[ColorNormalMesh3D]
    def saveColoredMesh(id: Person, expression: ExpressionType, mesh: ColorNormalMesh3D): Try[Unit]
  }

  trait Fitting {}

  def Neutral: ExpressionType

  def repositoryRoot: Path

  def incoming: Incoming

  def registration: SurfaceRegistration

  def model: ModelBuilding

  def fitting: Fitting

  def expressions: Seq[ExpressionType]

  def masks: Seq[MaskType]

  def loadMeshMask(from: String, to: String): Try[BinaryMask]

  def personFromFilename(filename: String): Person
}

