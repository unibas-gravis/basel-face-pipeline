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

import java.io.File

import registration.modelbuilding.FaceMask
import scalismo.faces.io.{MoMoIO, TLMSLandmarksIO}
import scalismo.faces.landmarks.TLMSLandmark3D
import scalismo.faces.mesh.{BinaryMask, ColorNormalMesh3D}
import scalismo.faces.momo.MoMo
import scalismo.geometry.{Landmark, _3D}
import scalismo.io.{LandmarkIO, MeshIO, StatismoIO}
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.StatisticalMeshModel

import scala.io.Source
import scala.reflect.io.Path
import scala.util.{Failure, Success, Try}

object BU3DDataProvider extends DataProvider {

  override case object Neutral extends ExpressionType { override def toString: String = "_NE00" }
  case object Sadness extends ExpressionType { override def toString: String = "_SA04" }
  case object Surprise extends ExpressionType { override def toString: String = "_SU04" }
  case object Disgust extends ExpressionType { override def toString: String = "_DI04" }
  case object Fear extends ExpressionType { override def toString: String = "_FE04" }
  case object Joy extends ExpressionType { override def toString: String = "_HA04" }
  case object Anger extends ExpressionType { override def toString: String = "_AN04" }
  case object CoreExpression extends ExpressionType { override def toString: String = "_ALLEXP" }

  object Expressions {
    def expressionList(): Seq[ExpressionType] = Seq(Neutral, Sadness, Surprise, Disgust, Fear, Joy, Anger)
    def expressionModelTypes(): Seq[ExpressionType] = Seq(Neutral,CoreExpression)
  }
  override def expressions() = Expressions.expressionList()



  case object RAW extends MaskType { override def toString: String = "_RAW" }
  case object F3D extends MaskType { override def toString: String = "_F3D" }

  object Masks {
    def maskList(): Seq[MaskType] = Seq(RAW, F3D)
  }

  override def masks: Seq[MaskType] = Masks.maskList()



  case class BU3DID(override val id: String, override val raceTag: String) extends Person
  object BU3DID {
    def fromFilename(filename: String): BU3DID = {
      BU3DID(filename.substring(0, 5), filename.substring(10, 12))
    }
  }

  override def personFromFilename(filename: String): Person = BU3DID.fromFilename(filename)




  case object Basel extends DataFlag { override def toString: String = "_basel" }
  case object Original extends DataFlag { override def toString: String = "" }
  case object Aligned extends DataFlag { override def toString: String = "_aligned" }

  object Flags {
    def lmFlagList(): Seq[DataFlag] = Seq(Basel, Original, Aligned)
  }



  private def setFileAccessMode(filename: String): Unit = setFileAccessMode(new File(filename))
  private def setFileAccessMode(path: Path): Unit = setFileAccessMode(path.jfile)
  private def setFileAccessMode(file: File): Unit = {
    file.setReadable(true,false)
    file.setWritable(true,false)
  }

  override def repositoryRoot: Path = Path("pipeline-data/")

  override def incoming: BU3DDataProvider.Incoming = {

    new BU3DDataProvider.Incoming {

      val incomingPath = repositoryRoot / "data" / "incoming"
      incomingPath.jfile.mkdirs()

      override def reference: BU3DDataProvider.Reference = new BU3DDataProvider.Reference {

        val referencePath = incomingPath / "reference"
        referencePath.jfile.mkdirs()

        override def loadFaceMask(): Try[FaceMask] = {

          val maskPath = referencePath / "masks"
          maskPath.jfile.mkdirs()

          for {
            level_mask <- MeshIO.readScalarMeshField[Int](new File(maskPath.jfile, "level-mask-l7.vtk"))
            semantic_mask <- MeshIO.readScalarMeshField[Short](new File(maskPath.jfile, "semantic-mask-l7.vtk")).map(_.map(_.toInt))
          } yield {
            FaceMask(level_mask,semantic_mask)
          }

        }

        override def loadMesh(expression: ExpressionType): Try[TriangleMesh[_3D]] = {
          import scalismo.faces.io.MeshIO
          val mshPath = referencePath / "mesh"
          mshPath.jfile.mkdirs()
          expression match {
            case Neutral => MeshIO.read(new File(mshPath.jfile, "mean2012_l7_bfm_nomouth.ply"))
              .map(_.shape)
            case Sadness => MeshIO.read(new File(mshPath.jfile, "mean2015.1_l7_bfm_nomouth-sadness.ply"))
              .map(_.shape)
            case Surprise => MeshIO.read(new File(mshPath.jfile, "mean2015.1_l7_bfm_nomouth-surprise.ply"))
              .map(_.shape)
            case Disgust => MeshIO.read(new File(mshPath.jfile, "mean2015.1_l7_bfm_nomouth-disgust.ply"))
              .map(_.shape)
            case Fear => MeshIO.read(new File(mshPath.jfile, "mean2015.1_l7_bfm_nomouth-fear.ply"))
              .map(_.shape)
            case Joy => MeshIO.read(new File(mshPath.jfile, "mean2015.1_l7_bfm_nomouth-joy.ply"))
              .map(_.shape)
            case Anger => MeshIO.read(new File(mshPath.jfile, "mean2015.1_l7_bfm_nomouth-anger.ply"))
              .map(_.shape)
          }
        }

        override def loadLandmarks(expression: ExpressionType): Try[Seq[Landmark[_3D]]] = {
          val lmPath = referencePath / "landmarks"
          lmPath.jfile.mkdirs()
            LandmarkIO.readLandmarksJson[_3D](new File(lmPath.jfile, s"reference${expression.toString}.json"))
        }

        override def saveLandmarks(expression: ExpressionType, landmarks: Seq[Landmark[_3D]]): Try[Unit] = {
          val lmPath = referencePath / "landmarks"
          lmPath.jfile.mkdirs()
          val res = LandmarkIO.writeLandmarksJson[_3D](landmarks.toIndexedSeq, new File(lmPath.jfile, s"reference${expression.toString}.json"))
          res match {
              case Success(_) => setFileAccessMode(lmPath)
              case _ =>
            }
          res
        }


        override def loadLineLandmarks(expression: ExpressionType): Try[Seq[Landmark[_3D]]] = ???
      }

      def landmarksPath(id: Person, expression: ExpressionType, mask: MaskType = RAW, flag: DataFlag = Basel): Path = {
        incomingPath / "landmarks" / s"${id.id}$expression${id.raceTag}${mask}$flag.tlms"
      }

      override def loadLandmarks(id: Person, expression: ExpressionType): Try[Seq[Landmark[_3D]]] = loadLandmarks(id, expression, RAW, Basel)
      override def loadLandmarks(id: Person, expression: ExpressionType, mask: MaskType): Try[Seq[Landmark[_3D]]] = loadLandmarks(id, expression, mask, Basel)
      override def loadLandmarks(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag): Try[Seq[Landmark[_3D]]] = {
        val path = landmarksPath(id, expression, mask, flag)
        TLMSLandmarksIO.read3D(path.jfile) match {
          case Success(tlmsLandmarks) => Success(tlmsLandmarks.filter(_.visible).map(_.toLandmark))
          case Failure(t) => Failure(t)
        }
      }

      override def saveLandmarks(id: Person, expression: ExpressionType, landmarks: Seq[Landmark[_3D]]): Try[Unit] = saveLandmarks(id, expression, RAW, Basel, landmarks)
      override def saveLandmarks(id: Person, expression: ExpressionType, mask: MaskType, landmarks: Seq[Landmark[_3D]]): Try[Unit] = saveLandmarks(id, expression, mask, Basel, landmarks)
      override def saveLandmarks(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag, landmarks: Seq[Landmark[_3D]]): Try[Unit] = {
        val path = landmarksPath(id, expression, mask, flag)
        path.jfile.getParentFile.mkdirs()
        val tlms = landmarks.map { lm =>
          TLMSLandmark3D(lm.id, lm.point, visible = true)
        }.toIndexedSeq
        val res = TLMSLandmarksIO.write3D(tlms, path.jfile)
        res match {
          case Success(_) => setFileAccessMode(path)
          case _ =>
        }
        res
      }

      def meshPath(id: Person, expression: ExpressionType, mask: MaskType = RAW, flag: DataFlag = Original): Path = {
        incomingPath / "mesh" / s"${id.id}${expression}${id.raceTag}${mask}$flag.ply"
      }

      override def loadMesh(id: Person, expression: ExpressionType) = loadMesh(id, expression, RAW, Original)
      override def loadMesh(id: Person, expression: ExpressionType, mask: MaskType) = loadMesh(id, expression, mask, Original)
      override def loadMesh(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag): Try[TriangleMesh[_3D]] = {
        import scalismo.faces.io.MeshIO
        val path = meshPath(id, expression, mask, flag)
        MeshIO.read(path.jfile).map(_.shape)
      }

      override def loadColoredMesh(id: Person, expression: ExpressionType): Try[ColorNormalMesh3D] = loadColoredMesh(id, expression, RAW, Original)
      override def loadColoredMesh(id: Person, expression: ExpressionType, mask: MaskType): Try[ColorNormalMesh3D] = loadColoredMesh(id, expression, mask, Original)
      override def loadColoredMesh(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag): Try[ColorNormalMesh3D] = {
        import scalismo.faces.io.MeshIO
        val path = meshPath(id, expression, mask, flag)
        MeshIO.read(path.jfile).map(ocnm => ColorNormalMesh3D(ocnm.shape,ocnm.color.get,ocnm.normals.get))
      }

      override def saveMesh(id: Person, expression: ExpressionType, mesh: TriangleMesh[_3D]): Try[Unit] = saveMesh(id, expression, RAW, Original, mesh)
      override def saveMesh(id: Person, expression: ExpressionType, mask: MaskType, mesh: TriangleMesh[_3D]): Try[Unit] = saveMesh(id, expression, mask, Original, mesh)
      override def saveMesh(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag, mesh: TriangleMesh[_3D]): Try[Unit] = {
        import scalismo.faces.io.MeshIO
        val path = meshPath(id, expression, mask, flag)
        path.jfile.getParentFile.mkdirs()
        MeshIO.write(mesh, None, None, path.jfile)
        setFileAccessMode(path)
        Success(Unit)
      }

      override def saveMesh(id: Person, expression: ExpressionType, mesh: ColorNormalMesh3D): Try[Unit] = saveMesh(id, expression, RAW, Original, mesh)
      override def saveMesh(id: Person, expression: ExpressionType, mask: MaskType, mesh: ColorNormalMesh3D): Try[Unit] = saveMesh(id, expression, mask, Original, mesh)
      override def saveMesh(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag, mesh: ColorNormalMesh3D): Try[Unit] = {
        import scalismo.faces.io.MeshIO
        val path = meshPath(id, expression, mask, flag)
        path.jfile.getParentFile.mkdirs()
        MeshIO.write(mesh, path.jfile)
        setFileAccessMode(path)
        Success(Unit)
      }

      override def ids(expression: ExpressionType): Seq[Person] = {
        new File(incomingPath.jfile, "mesh").listFiles()
          .filter(_.getName.endsWith(".ply"))
          .filter(_.getName.contains(RAW.toString))
          .filter(_.getName.contains(expression.toString))
          .map(file => BU3DID.fromFilename(file.getName))
          .toSeq
      }
    }
  }

  override def registration: BU3DDataProvider.SurfaceRegistration = {

    new BU3DDataProvider.SurfaceRegistration {
      val registrationPath = repositoryRoot / "data" / "registered"
      registrationPath.jfile.mkdirs()

      val referencePath = registrationPath / "reference"
      referencePath.jfile.mkdirs()

      val modelPath = referencePath / "gpmodels"
      modelPath.jfile.mkdirs()

      override def loadPriorModel(expression: ExpressionType): Try[StatisticalMeshModel] = {
        expression match {
          case Neutral => StatismoIO.readStatismoMeshModel(new File(modelPath.jfile, "face-model-neutral.h5"))
          case Sadness => StatismoIO.readStatismoMeshModel(new File(modelPath.jfile, "face-model-sadness.h5"))
          case Surprise => StatismoIO.readStatismoMeshModel(new File(modelPath.jfile, "face-model-surprise.h5"))
          case Disgust => StatismoIO.readStatismoMeshModel(new File(modelPath.jfile, "face-model-disgust.h5"))
          case Fear => StatismoIO.readStatismoMeshModel(new File(modelPath.jfile, "face-model-fear.h5"))
          case Joy => StatismoIO.readStatismoMeshModel(new File(modelPath.jfile, "face-model-joy.h5"))
          case Anger => StatismoIO.readStatismoMeshModel(new File(modelPath.jfile, "face-model-anger.h5"))
          case CoreExpression => StatismoIO.readStatismoMeshModel(new File(modelPath.jfile, "face-model-combined-expressions.h5"))
        }
      }

      override def savePriorModel(model: StatisticalMeshModel, expression: ExpressionType): Try[Unit] = {
        expression match {
          case Neutral => StatismoIO.writeStatismoMeshModel(model, new File(modelPath.jfile, "face-model-neutral.h5"))
          case Sadness => StatismoIO.writeStatismoMeshModel(model, new File(modelPath.jfile, "face-model-sadness.h5"))
          case Surprise => StatismoIO.writeStatismoMeshModel(model, new File(modelPath.jfile, "face-model-surprise.h5"))
          case Disgust => StatismoIO.writeStatismoMeshModel(model, new File(modelPath.jfile, "face-model-disgust.h5"))
          case Fear => StatismoIO.writeStatismoMeshModel(model, new File(modelPath.jfile, "face-model-fear.h5"))
          case Joy => StatismoIO.writeStatismoMeshModel(model, new File(modelPath.jfile, "face-model-joy.h5"))
          case Anger => StatismoIO.writeStatismoMeshModel(model, new File(modelPath.jfile, "face-model-anger.h5"))
          case CoreExpression => StatismoIO.writeStatismoMeshModel(model, new File(modelPath.jfile, "face-model-combined-expressions.h5"))
        }

      }

      def meshPath(id: Person, expression: ExpressionType, mask: MaskType = RAW, flag: DataFlag = Original): Path = {
        registrationPath / "mesh" / s"${id.id}${expression}${id.raceTag}${mask}${flag}.ply"
      }

      override def loadMesh(id: Person, expression: ExpressionType): Try[TriangleMesh[_3D]] = {
        import scalismo.faces.io.MeshIO
        val path = meshPath(id, expression)
        MeshIO.read(path.jfile).map(_.shape)
      }

      override def loadMesh(id: Person, expression: ExpressionType, mask: MaskType): Try[TriangleMesh[_3D]] = ???
      override def loadMesh(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag): Try[TriangleMesh[_3D]] = ???
      override def loadColoredMesh(id: Person, expression: ExpressionType): Try[ColorNormalMesh3D] = ???
      override def loadColoredMesh(id: Person, expression: ExpressionType, mask: MaskType): Try[ColorNormalMesh3D] = ???
      override def loadColoredMesh(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag): Try[ColorNormalMesh3D] = ???

      override def saveMesh(id: Person, expression: ExpressionType, mesh: TriangleMesh[_3D]): Try[Unit] = {
        import scalismo.faces.io.MeshIO
        val path = meshPath(id, expression)
        MeshIO.write(mesh, None, None, path.jfile)
        setFileAccessMode(path)
        Success(Unit)
      }

      override def saveMesh(id: Person, expression: ExpressionType, mask: MaskType, mesh: TriangleMesh[_3D]): Try[Unit] = ???
      override def saveMesh(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag, mesh: TriangleMesh[_3D]): Try[Unit] = ???

      override def saveMesh(id: Person, expression: ExpressionType, mesh: ColorNormalMesh3D): Try[Unit] = ???
      override def saveMesh(id: Person, expression: ExpressionType, mask: MaskType, mesh: ColorNormalMesh3D): Try[Unit] = ???
      override def saveMesh(id: Person, expression: ExpressionType, mask: MaskType, flag: DataFlag, mesh: ColorNormalMesh3D): Try[Unit] = ???

      override def ids(expression: ExpressionType): Seq[Person] = {
        new File(registrationPath.jfile, "mesh").listFiles()
          .filter(_.getName.endsWith("ply"))
          .filter(_.getName.contains(expression.toString))
          .map(file => BU3DID.fromFilename(file.getName))
          .toSeq
      }

    }
  }

  override def model : BU3DDataProvider.ModelBuilding = BU3DModelBuilding

  object BU3DModelBuilding extends BU3DDataProvider.ModelBuilding {
    val modelBuildingPath = repositoryRoot / "data" / "modelbuilding"
    modelBuildingPath.jfile.mkdirs()

    val modelDirectoryPath = modelBuildingPath / "model"
    modelDirectoryPath.jfile.mkdirs()

    val colorExtractdMeshPath = modelBuildingPath / "mesh"
    colorExtractdMeshPath.jfile.mkdirs()

    def meshPath(id: Person, expression: ExpressionType, mask: MaskType = RAW, flag: DataFlag = Original): Path = {
      colorExtractdMeshPath / s"${id.id}${expression}${id.raceTag}${mask}$flag.ply"
    }

    override def loadColoredMesh(id: Person, expression: ExpressionType): Try[ColorNormalMesh3D] = {
      loadColoredMesh(id,expression,RAW,Original)
    }

    def loadColoredMesh(id: Person, expression: ExpressionType, mask: MaskType = RAW, flag: DataFlag = Original): Try[ColorNormalMesh3D] = {
      import scalismo.faces.io.MeshIO
      val path = meshPath(id, expression, mask, flag)
      MeshIO.read(path.jfile).map(_.colorNormalMesh3D.get)
    }

    override def saveColoredMesh(id: Person, expression: ExpressionType, mesh: ColorNormalMesh3D): Try[Unit] = {
      saveColoredMesh(id,expression,mesh,RAW,Original)
    }

    def saveColoredMesh(id: Person, expression: ExpressionType, mesh: ColorNormalMesh3D, mask: MaskType = RAW, flag: DataFlag = Original): Try[Unit] = {
      import scalismo.faces.io.MeshIO
      val path = meshPath(id, expression, mask, flag)
      path.jfile.getParentFile.mkdirs
      MeshIO.write(mesh, path.jfile)
      setFileAccessMode(path)
      Success(Unit)
    }

    def modelPath(mask: MaskType): Path = {
      modelDirectoryPath / s"bu3d_pami17${mask}.h5" // @todo think about registration identifier in name
    }

    override def saveModel(mask: MaskType, momo: MoMo): Try[Unit]  = {
      val path = modelPath(mask)
      path.jfile.getParentFile.mkdirs
      val res = MoMoIO.write(momo, path.jfile, "")
      res match {
        case Success(_) => setFileAccessMode(path)
        case _ =>
      }
      res
    }

    override def loadModel(mask: MaskType): Try[MoMo] = {
      MoMoIO.read(modelPath(mask).jfile, "")
    }

  }

  override def fitting: BU3DDataProvider.Fitting = {
    new BU3DDataProvider.Fitting {

    }
  }





  override def loadMeshMask(from: String, to: String): Try[BinaryMask] = {
    BinaryMask.load(Source.fromFile(new File(repositoryRoot / "data" / "incoming" / "reference" / "masks" / from+"_TO_"+to+".mask")))
  }


}
