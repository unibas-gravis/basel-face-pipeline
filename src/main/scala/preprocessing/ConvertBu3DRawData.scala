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

import java.awt.image.BufferedImage
import java.io.{File, FileInputStream, InputStream}
import javax.imageio.ImageIO

import ch.unibas.cs.gravis.facepipeline._
import scalismo.faces.landmarks.TLMSLandmark3D
import scalismo.faces.mesh.{ColorNormalMesh3D, TextureMappedProperty, VertexPropertyPerTriangle}
import scalismo.faces.render.Transform3D
import scalismo.faces.utils.ResourceManagement
import scalismo.faces.color.RGBA
import scalismo.faces.image.PixelImage
import scalismo.common.PointId
import scalismo.geometry._
import scalismo.mesh._
import scalismo.registration.{LandmarkRegistration, RigidTransformation, TranslationTransform}

import scala.io.Source
import scala.reflect.io._
import scala.collection.mutable.ListBuffer
import scala.util.Try



object ConvertBu3DRawData {

  def main(args: Array[String]) {
    import ch.unibas.cs.gravis.facepipeline.BU3DDataProvider
    scalismo.initialize()
    ConvertBu3DRawData(BU3DDataProvider).run()
  }

}



case class ConvertBu3DRawData(dataProvider : DataProvider) extends PipelineStep {

  override def run() {
    val inputDirectory = dataProvider.repositoryRoot / "data" / "bu3dfe" / "original"
    val ids = getIds(inputDirectory)
    println(s"found ${ids.size} ids....")

    ids.take(4).foreach { id => // todo: remove take(4) to build on full database
      preprocessMaximalExpressions(id, inputDirectory)
    }
  }

  def getIds(directory: Path): Seq[dataProvider.Person] = {
    require(
      directory.isDirectory,
      "Expected path to the parent DIRECTORY containing the 100 folders of the BU3D, one for each person."
    )
    val subDirectories = directory.toDirectory.list
    val subDirectoryNames = subDirectories.map(_.name).toIndexedSeq
    val sortedList = subDirectoryNames.sortWith((l, r) => l.compareTo(r) < 0)
    sortedList.map { subdir =>
      val filenames = (directory / subdir).toDirectory.list.map(_.name).toIndexedSeq
      val filename = filenames.filter(file => file.contains("NE00") && file.contains("RAW") && file.contains(".wrl")).head
      dataProvider.personFromFilename(filename)
    }
  }

  def getFilestem(id: dataProvider.Person, expression: ExpressionType, mask: MaskType): String = {
    val base = id + expression.toString
    s"${id.id}${expression}${id.raceTag}${mask}"
  }

  def preprocessMaximalExpressions(id: dataProvider.Person, inputDirectory: Path): Unit = {
    import BU3DDataProvider.{RAW,F3D,Original,Aligned}

    val expressions = dataProvider.expressions
    val outputPath = dataProvider.repositoryRoot / "data" / "incoming"

    expressions.foreach { expression =>
      Seq(RAW,F3D).map { mask =>
        val meshStem = getFilestem(id, expression, mask)
        val textureStem = getFilestem(id, expression, F3D)

        val meshPath = outputPath / "mesh" / s"${id.id}${expression}${id.raceTag}${mask}.ply"
        if (! meshPath.jfile.exists() ) {
          println(meshStem + " converting mesh")
          val texture = loadTexture(id, textureStem, inputDirectory)
          val mesh = loadBu3DWRLHacky(id, meshStem, inputDirectory, texture)
          dataProvider.incoming.saveMesh(id, expression, mask, Original, mesh).get
        } else
          println(meshStem + " already converted mesh")

        // convert all available landmarks
        val landmarkPath = outputPath / "landmarks" / s"${id.id}${expression}${id.raceTag}${mask}.tlms"
        if (! landmarkPath.jfile.exists() ) {
          println(meshStem + " converting landmarks")
          if (meshStem.endsWith("F3D")) {
            val landmarks = readBU3DFEbnd(inputDirectory / id.id / s"${meshStem}.bnd").get
            dataProvider.incoming.saveLandmarks(id, expression, mask, Original, landmarks.map { lm => Landmark[_3D](id = lm.id, point = lm.point) }).get
          }
          if (meshStem.endsWith("RAW")) {
            val landmarks = readBU3DFEpse(inputDirectory / id.id / s"${meshStem}.pse").get
            dataProvider.incoming.saveLandmarks(id, expression, mask, Original, landmarks.map { lm => Landmark[_3D](id = lm.id, point = lm.point) }).get
          }
        } else {
          println(meshStem + " already converted landmarks")
        }
      }

    }

    // do alignment after converting all data as we need for each id raw+f3d available
    expressions.foreach { expression =>
      // align f3d data to original raw data
      val meshStem = getFilestem(id, expression, F3D)
      val landmarkPath = outputPath / "landmarks" / s"${id.id}${expression}${id.raceTag}${F3D}_aligned.tlms"
      val meshPath = outputPath / "mesh" / s"${id.id}${expression}${id.raceTag}${F3D}_aligned.ply"
      if ( (!landmarkPath.jfile.exists()) && (!meshPath.jfile.exists()) ) {
        println(meshStem + " calculating alignment")
        val rawMesh = dataProvider.incoming.loadColoredMesh(id, expression, RAW).get
        val rawLandmarks = dataProvider.incoming.loadLandmarks(id, expression, RAW, Original).get
        val f3dMesh = dataProvider.incoming.loadColoredMesh(id, expression, F3D).get
        val f3dLandmarks = dataProvider.incoming.loadLandmarks(id, expression, F3D, Original).get

        val aligned = align(f3dMesh, f3dLandmarks, rawMesh, rawLandmarks)

        aligned match {
          case Some((meshAligned, landmarksAligned)) =>
            dataProvider.incoming.saveLandmarks(id, expression, F3D, Aligned, landmarksAligned).get
            dataProvider.incoming.saveMesh(id, expression, F3D, Aligned, meshAligned).get
          case None =>
        }
      } else {
        println(meshStem + " already aligned")
      }
    }

  }

  def loadTexture(id: dataProvider.Person, stem: String, inputDirectory: Path): PixelImage[RGBA] = {
    import scalismo.faces.image.BufferedImageConverter
    val textureName = inputDirectory / id.id / s"${stem}.bmp"
    println(textureName.toString())
    val img: BufferedImage = ImageIO.read(new java.io.File(textureName.toString()))
    BufferedImageConverter.toPixelImage(img)
  }

  def loadBu3DWRLHacky(id: dataProvider.Person, stem: String, inputDirectory: Path, texture: PixelImage[RGBA]): ColorNormalMesh3D = {
    val meshName = inputDirectory / id.id / s"$stem.wrl"
    val lines = Source.fromFile(meshName.toString()).getLines()

    val coordinates = ListBuffer[Point[_3D]]()
    val textureCoordinates = ListBuffer[Point[_2D]]()
    val textureCoordinateIndex = ListBuffer[TriangleCell]()
    val triangleVertexIndex = ListBuffer[TriangleCell]()
    parseBu3DWRL(lines, coordinates, triangleVertexIndex, textureCoordinates, textureCoordinateIndex)

    val vc = coordinates.toIndexedSeq
    val tc = textureCoordinates.toIndexedSeq
    val tvi = TriangleList(triangleVertexIndex.toIndexedSeq)
    val tci = TriangleList(textureCoordinateIndex.toIndexedSeq)

    val mesh = TriangleMesh3D(vc, tvi)
    val texCoords = VertexPropertyPerTriangle(tvi, tci.triangles.map(_.toIntVector3D), tc)
    val tex = TextureMappedProperty(tvi, texCoords, texture)
    ColorNormalMesh3D(mesh, tex, mesh.cellNormals)

  }

  def readBU3DFEbnd(path: Path): Try[IndexedSeq[TLMSLandmark3D]] = {
    ResourceManagement.usingTry(Try(new FileInputStream(path.toString())))(readBU3DFEbnd)
  }

  def readBU3DFEbnd(stream: InputStream): Try[IndexedSeq[TLMSLandmark3D]] = Try {
    // little bit unsafe, we read each line and expect to have a landmark!

    var counter = 0
    val lines = Source.fromInputStream(stream).getLines()
    lines.map { line =>
      val fields = line.split("\\s+").map(_.trim)

      counter += 1
      val name = "bu3dfe-lm-" + counter
      val x = fields(1).toFloat
      val y = fields(2).toFloat
      val z = fields(3).toFloat
      TLMSLandmark3D(name, Point(x, y, z), true)
    }.toIndexedSeq
  }

  def readBU3DFEpse(path: Path): Try[IndexedSeq[TLMSLandmark3D]] = {
    ResourceManagement.usingTry(Try(new FileInputStream(path.toString())))(readBU3DFEpse)
  }

  def readBU3DFEpse(stream: InputStream): Try[IndexedSeq[TLMSLandmark3D]] = Try {
    // little bit unsafe, we read each line and expect to have an landmark

    var counter = 0
    val lines = Source.fromInputStream(stream).getLines().toIndexedSeq

    val idOrder = Array(1, 5, 9, 13, 40, 45, 84, 85).toIndexedSeq

    // filter out pose normal (entry starting with n)
    val linesFiltered = lines.filterNot(_.substring(0, 1) == "n")
    if (linesFiltered.length == 8) {
      for (line <- linesFiltered) yield {

        val fields = line.split("\\s+").map(_.trim)

        val name = "bu3dfe-lm-" + idOrder(counter)
        counter += 1
        val x = fields(1).toFloat
        val y = fields(2).toFloat
        val z = fields(3).toFloat
        TLMSLandmark3D(name, Point(x, y, z), true)
      }
    } else {
      // empty list
      IndexedSeq[TLMSLandmark3D]()
    }
  }

  def align(
    f3dMeshOrig: ColorNormalMesh3D,
    f3dLandmarks: Seq[Landmark[_3D]],
    rawMeshOrig: ColorNormalMesh3D,
    rawLandmarks: Seq[Landmark[_3D]]
  ): Option[(ColorNormalMesh3D, Seq[Landmark[_3D]])] = {

    if (f3dLandmarks.isEmpty || rawLandmarks.isEmpty) {
      println(s"Not enough landmarks: f3dLandmarks.size=${f3dLandmarks.size} / rawLandmarks.size=${rawLandmarks.size}")
      return None
    }

    // landmark transformation
    val transLM = LandmarkRegistration.rigid3DLandmarkRegistration(f3dLandmarks, rawLandmarks, Point(0,0,0))
    val transLMNormal: RigidTransformation[_3D] = RigidTransformation(transLM.rotation, TranslationTransform(Vector(0, 0, 0)))
    val transLMFaces: Transform3D = new Transform3D {
      override def apply(x: Point[_3D]): Point[_3D] = transLM.f(x)
      override def apply(v: Vector[_3D]): Vector[_3D] = transLMNormal.f(v.toPoint).toVector
    }

    var f3dLandmarksT1 = transformLandmarks(f3dLandmarks, transLM)

    var f3dPointsT1 = f3dMeshOrig.transform(transLMFaces)

    val n0 = for (p <- f3dPointsT1.shape.pointSet.points) yield {
      rawMeshOrig.shape.pointSet.findClosestPoint(p).point
    }
    var avgError = f3dPointsT1.shape.pointSet.points.zip(n0).map(p => (p._1 - p._2).norm2).sum / f3dMeshOrig.shape.pointSet.points.toIndexedSeq.length

    var iteration = 0
    var r = scala.util.Random
    while (avgError > 0.2 && iteration < 500) {
      val f3DPointsSampled = for (i <- 1 to 1000) yield {
        f3dPointsT1.shape.pointSet.pointsWithId.toIndexedSeq(r.nextInt(f3dPointsT1.shape.pointSet.points.length - 1))
      }

      val nn = for (f3dPoint <- f3DPointsSampled) yield {
        val rawNeighbor = rawMeshOrig.shape.pointSet.findClosestPoint(f3dPoint._1)
        if ((f3dPoint._1 - rawNeighbor.point).norm < 5) {
          (f3dPoint._1, rawNeighbor.point)
        } else {
          (None, None)
        }
      }
      val nnFiltered = nn.filterNot(p => p._1 == None || p._2 == None).map(p => (p._1.asInstanceOf[Point3D], p._2.asInstanceOf[Point3D]))
      if (nnFiltered.length > 0) {

        val transT2 = LandmarkRegistration.rigid3DLandmarkRegistration(nnFiltered, Point(0,0,0))
        val transT2Normal: RigidTransformation[_3D] = RigidTransformation(transT2.rotation, TranslationTransform(Vector(0, 0, 0)))
        val transT2Faces: Transform3D = new Transform3D {
          override def apply(x: Point[_3D]): Point[_3D] = transT2.f(x)

          override def apply(v: Vector[_3D]): Vector[_3D] = transT2Normal.f(v.toPoint).toVector
        }

        val f3dLandmarksT2 = transformLandmarks(f3dLandmarksT1, transT2)
        val f3dPointsT2 = f3dPointsT1.transform(transT2Faces)
        val sampledPointsTrans = transformPoints(nnFiltered.map(p => p._1), transT2)

        avgError = sampledPointsTrans.zip(nnFiltered).map(p => (p._1 - p._2._2).norm).sum / sampledPointsTrans.length

        f3dPointsT1 = f3dPointsT2
        f3dLandmarksT1 = f3dLandmarksT2
        iteration = iteration + 1
      } else {
        println("icp alignment did not find corresponding points!")
        avgError = -1
      }

    }
    println("... alignment error after iteration " + iteration + ": " + avgError)

    val outTLMSLandmarks = for (lm <- f3dLandmarksT1.zip(f3dLandmarks)) yield {
      TLMSLandmark3D(lm._2.id, lm._1.point, true)
    }

    Some((f3dPointsT1, f3dLandmarksT1))
  }

  def transformLandmarks(landmarks: Seq[Landmark[_3D]], trans: RigidTransformation[_3D]): Seq[Landmark[_3D]] = {
    for (lm <- landmarks) yield {
      Landmark[_3D](lm.id, trans(lm.point), lm.description, lm.uncertainty)
    }
  }

  def transformPoints(points: Seq[Point[_3D]], trans: RigidTransformation[_3D]): Seq[Point[_3D]] = {
    for (p <- points) yield {
      trans(p)
    }
  }

  def parseBu3DWRL(
    lines: Iterator[String],
    coordinates: ListBuffer[Point[_3D]],
    tvi: ListBuffer[TriangleCell],
    textureCoordinates: ListBuffer[Point[_2D]],
    tci: ListBuffer[TriangleCell]
  ): Unit = {

    val BOILERPLATE = "boilerplate"
    val COORDINATES = "coord Coordinate"
    val COORDINATES2 = "coord Coordinate {"
    val TEXTURECOORDS = "texCoord TextureCoordinate"
    val TEXTURECOORDS2 = "texCoord TextureCoordinate {"
    val TEXCOORDINDEX = "texCoordIndex ["
    val VERTEXINDEX = "coordIndex ["

    var mode = "boilerplate"

    for (line <- lines) {
      val trimmed = line.trim
      mode match {
        case BOILERPLATE => {
          trimmed match {
            case COORDINATES => mode = COORDINATES
            case COORDINATES2 => mode = COORDINATES
            case TEXTURECOORDS => mode = TEXTURECOORDS
            case TEXTURECOORDS2 => mode = TEXTURECOORDS
            case TEXCOORDINDEX => mode = TEXCOORDINDEX
            case VERTEXINDEX => mode = VERTEXINDEX
            case _ =>
          }
        }
        case COORDINATES => {
          trimmed match {
            case "{" =>
            case "point" =>
            case "point [" =>
            case "[" =>
            case "" =>
            case "]" => mode = BOILERPLATE
            case data => {
              data.split(',').map(_.trim).filter(s => s.nonEmpty).foreach { tripplet =>
                val point = Point[_3D](tripplet.split(" ").map { w =>
                  try {
                    w.toDouble
                  } catch {
                    case e: Throwable => {
                      println(w)
                      throw (e)
                    }
                  }
                })
                coordinates += point
              }
            }
          }
        }
        case TEXTURECOORDS => {
          trimmed match {
            case "{" =>
            case "point" =>
            case "point [" =>
            case "[" =>
            case "" =>
            case "]" => mode = BOILERPLATE
            case data => {
              data.split(",").map(_.trim).filter(_.nonEmpty).flatMap(_.split(" ")).map(_.trim).filter(_.nonEmpty).grouped(2).foreach { pair =>
                val point = Point[_2D](pair.map { w =>
                  try {
                    w.toDouble
                  } catch {
                    case e: Throwable => {
                      println(w)
                      throw (e)
                    }
                  }
                })
                textureCoordinates += point
              }
            }
          }
        }
        case VERTEXINDEX => {
          trimmed match {
            case "[" =>
            case "" =>
            case "]" => mode = BOILERPLATE
            case data => {
              val stringToParse = if (data.last == ',') data.init else data
              val indices = stringToParse.split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toIndexedSeq
              require(indices.last == -1)
              val alwaysFirst = indices.head
              val firstAndlastRemoved = indices.init.tail
              val seconds = firstAndlastRemoved.init
              val thirds = firstAndlastRemoved.tail
              val grouped = seconds.zip(thirds)
              grouped.foreach { pair =>
                tvi += TriangleCell(
                  PointId(alwaysFirst),
                  PointId(pair._1),
                  PointId(pair._2)
                )
              }
            }
          }
        }
        case TEXCOORDINDEX => {
          trimmed match {
            case "[" =>
            case "" =>
            case "]" => mode = BOILERPLATE
            case data => {
              val stringToParse = if (data.last == ',') data.init else data
              val indices = stringToParse.split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toIndexedSeq
              require(indices.last == -1)
              val alwaysFirst = indices.head
              val firstAndlastRemoved = indices.init.tail
              val seconds = firstAndlastRemoved.init
              val thirds = firstAndlastRemoved.tail
              val grouped = seconds.zip(thirds)
              grouped.foreach { pair =>
                tci += TriangleCell(
                  PointId(alwaysFirst),
                  PointId(pair._1),
                  PointId(pair._2)
                )
              }
            }
          }
        }
      }
    }
  }

}
