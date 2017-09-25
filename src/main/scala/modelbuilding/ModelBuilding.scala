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

package modelbuilding

import breeze.linalg.DenseMatrix
import ch.unibas.cs.gravis.facepipeline.{BU3DDataProvider, DataProvider, ExpressionType, PipelineStep}
import scalismo.common._
import scalismo.faces.color.{RGB, RGBA}
import scalismo.faces.io.MoMoIO
import scalismo.faces.mesh.{ColorNormalMesh3D, VertexColorMesh3D}
import scalismo.faces.momo.MoMo.NeutralWithExpression
import scalismo.faces.momo.{MoMo, PancakeDLRGP}
import scalismo.faces.render.Transform3D
import scalismo.geometry.{Point, Vector, _3D}
import scalismo.kernels.{DiagonalKernel, GaussianKernel, MatrixValuedPDKernel}
import scalismo.mesh.{SurfacePointProperty, TriangleMesh, TriangleMesh3D}
import scalismo.numerics.{UniformMeshSampler3D}
import scalismo.registration.{LandmarkRegistration, RigidTransformation}
import scalismo.statisticalmodel.{GaussianProcess, LowRankGaussianProcess}
import scala.util.{Success, Try}

object ModelBuilding {

  def main(args: Array[String]) {
    scalismo.initialize()
    ModelBuilding(BU3DDataProvider).run()
  }

}

case class ModelBuilding(dataProvider: DataProvider)  extends PipelineStep {

  override def run() {
    createMeshesWithVertexColor()
//    buildMoMoExpress()
    buildMoMoExpress("face12_nomouth")
  }

  /**
   * Extracts color for all registration results from the input meshes.
   */
  def createMeshesWithVertexColor(): Unit = {
    println("Extracting color from meshes ...")

    val expressions = dataProvider.expressions
    expressions.zipWithIndex.flatMap { case (exp,idx) =>
      println(s"... processing expression ${exp} (${idx+1}/${expressions.size})")

      val ids = dataProvider.registration.ids(exp)
      ids.zipWithIndex.map { case(id,idx) =>
        println(s"... ... processing id ${id} (${idx+1}/${ids.size})")

        val registeredShape: TriangleMesh[_3D] = dataProvider.registration.loadMesh(id, exp).get
        val coloredMesh: ColorNormalMesh3D = dataProvider.incoming.loadColoredMesh(id, exp).get
        val vertexColor = extractVertexColor(registeredShape, coloredMesh)

        val registeredShapeWithVertexColor = ColorNormalMesh3D(registeredShape, vertexColor, registeredShape.vertexNormals)
        dataProvider.model.saveColoredMesh(id, exp, registeredShapeWithVertexColor).get
      }

    }
  }

  /**
   * Extracts per vertex color for the registered mesh from the input mesh. The correspondence for each
   * vertex is sought after along the normal.
   *
   * @param registeredMesh Registration result without color.
   * @param colorMesh Input mesh with color.
   * @return Registeration result with color.
   */
  def extractVertexColor(registeredMesh: TriangleMesh3D, colorMesh: ColorNormalMesh3D): SurfacePointProperty[RGBA] = {

    val pointsReg = registeredMesh.pointSet.points.toIndexedSeq
    val normalsReg = registeredMesh.vertexNormals.pointData

    val shapeFromColorMesh = colorMesh.shape
    val meshOperations = shapeFromColorMesh.operations

    val colors = pointsReg.zip(normalsReg).map {
      case (point, normal) =>

        val intersections = meshOperations.getIntersectionPointsOnSurface(point, normal)

        val sortedIntersections = intersections.map { i =>
          val pointI = shapeFromColorMesh.position(i._1, i._2)
          val dist = (pointI - point).norm
          (i, dist)
        }.sortWith((a, b) => a._2 < b._2)

        if (sortedIntersections.nonEmpty && sortedIntersections.head._2 < 2.0) {
          val i = sortedIntersections.head._1
          colorMesh.color(i._1, i._2)
        } else {
          RGBA(0.9, 0.8, 0.1, 0.0)
        }
    }

    new SurfacePointProperty[RGBA](registeredMesh.triangulation, colors)
  }




  /**
   * Build a model from meshes with vertex color.
   * This step assumes that the registration was performed using the "bfm_nomouth" masked reference.
   */
  def buildMoMoExpress(maskType: String = "bfm_nomouth"): Unit = {
    println("Building model from meshes with vertex color...")

    val mask = {
      if ( maskType != "bfm_nomouth" ) Some(dataProvider.loadMeshMask("bfm_nomouth", "face12_nomouth").get)
      else None
    }

    val reference = {
      val originalRef = dataProvider.registration.loadPriorModel(dataProvider.Neutral).get.referenceMesh
      if ( mask.isDefined ) originalRef.operations.maskPoints(mask.get).transformedMesh
      else originalRef
    }
    val ids = dataProvider.registration.ids(dataProvider.Neutral)
    val otherExpressions = dataProvider.expressions.filter(exp => exp != dataProvider.Neutral)

    buildModel(ids, otherExpressions, reference)


    def buildModel(ids: Seq[dataProvider.Person], expressions: Seq[ExpressionType], reference: TriangleMesh3D) = {
      val data: Seq[Try[(DiscreteField[_3D, RGBA], VertexColorMesh3D, Seq[NeutralWithExpression])]] = ids.zipWithIndex.map {
        case (id, idx) =>
          println(s"... loading data for ${id.id} (${idx + 1}/${ids.size})")
          prepareData(reference, expressions, id)
      }

      val neutralMeshes: Seq[VertexColorMesh3D] = data.collect({ case Success(e) => e._2 })
      val neutralWithExpressions: Seq[NeutralWithExpression] = data.collect({ case Success(e) => e._3 }).flatten

      println(".. data loaded ...")

      val momo = MoMo.buildFromRegisteredSamples(
        reference = reference,
        samplesShape = neutralMeshes.toIndexedSeq,
        samplesColor = neutralMeshes.toIndexedSeq,
        samplesExpression = neutralWithExpressions.toIndexedSeq,
        shapeNoiseVariance = 0,
        colorNoiseVariance = 0,
        expressionNoiseVariance = 0)

      println("... initial model is built - (not handling missing color) ...")
      println(s"... ... shape rank: ${momo.shape.rank}")
      println(s"... ... color rank: ${momo.color.rank}")
      println(s"... ... exp rank: ${momo.expression.rank}")


      val colors: Seq[DiscreteField[_3D, RGBA]] = data.collect({ case Success(e) => e._1 })

      val colorModel = buildColorModel(reference, colors.toIndexedSeq, colors.size - 1)
      println("... color model is built ...")

      val bu3dModel = MoMo(momo.referenceMesh, momo.shape, colorModel, momo.expression, momo.landmarks)

      val newModelPath = dataProvider.repositoryRoot / "data" / "modelbuilding" / "model" / s"bu3d-${maskType}.h5"
      newModelPath.jfile.getParentFile.mkdirs()
      MoMoIO.write(bu3dModel, newModelPath.jfile)
      println("... model building finished!")
    }


    /** Align vcm to reference. */
    def align(reference: TriangleMesh3D, vcm: VertexColorMesh3D): VertexColorMesh3D = {
      val t: RigidTransformation[_3D] = LandmarkRegistration.rigid3DLandmarkRegistration(
        vcm.shape.pointSet.points.zip(reference.pointSet.points).toSeq,
        Point(0, 0, 0)
      )

      val transform = new Transform3D {
        override def apply(x: Point[_3D]): Point[_3D] = t(x)

        override def apply(v: Vector[_3D]): Vector[_3D] = t.rotation(v.toPoint).toVector
      }

      vcm.transform(transform)
    }

    /** Align mesh to reference. */
    def alignIt(vcm: VertexColorMesh3D): VertexColorMesh3D = align(reference, vcm)

    /** Execute function f for neutral and expression in ne. */
    def doForBothInNWE(ne: NeutralWithExpression, f: VertexColorMesh3D => VertexColorMesh3D): NeutralWithExpression = {
      ne match {
        case NeutralWithExpression(n, e) =>
          NeutralWithExpression(f(n), f(e))
      }
    }

    /** Mask mesh with precalculated mask. */
    def maskMesh(mesh: VertexColorMesh3D): VertexColorMesh3D = {
      if (mask.isDefined) {
        val reducer = mesh.shape.operations.maskPoints(mask.get)
        VertexColorMesh3D(
          reducer.transformedMesh,
          SurfacePointProperty.sampleSurfaceProperty(reducer.applyToSurfaceProperty(mesh.color), _.head)
        )
      } else {
        println("Warning: maskMesh is called but no mask is defined!")
        println("\t Hence the mesh is left unaltered.")
        mesh
      }
    }


    /**
      * Loads the neutral face and the expressions.
      *
      * @param reference
      * @param otherExpressions
      * @param id
      * @return
      */
    def prepareData(reference: TriangleMesh3D, otherExpressions: Seq[ExpressionType], id: dataProvider.Person) = {

      for {
        unalignedRegistrationWithColor <- dataProvider.model.loadColoredMesh(id, dataProvider.Neutral)
      } yield {

        val unaligned = VertexColorMesh3D(
          unalignedRegistrationWithColor.shape,
          unalignedRegistrationWithColor.color.asInstanceOf[SurfacePointProperty[RGBA]]
        )
        val masked = if ( mask.isDefined) maskMesh(unaligned) else unaligned
        val neutral = alignIt(masked)

        val neutralWithExpressionList = otherExpressions.par.map {
          exp =>
            println(s"... ... ${exp}")
            val unalignedExpression = dataProvider.model.loadColoredMesh(id, exp).get

            val unaligned = VertexColorMesh3D(
              unalignedExpression.shape,
              unalignedExpression.color.asInstanceOf[SurfacePointProperty[RGBA]]
            )
            val masked = if (mask.isDefined) maskMesh(unaligned) else unaligned

            val alignedExpression = alignIt(masked)
            NeutralWithExpression(neutral, alignedExpression)
        }.toIndexedSeq

        val neutralColor = DiscreteField[_3D, RGBA](neutral.shape.pointSet, neutral.color.pointData)
        (
          neutralColor,
          neutral,
          neutralWithExpressionList
        )
      }

    }
  }



  /**
    * Calculate the rigid 3d transform that aligns the mesh to the target.
    */
  def calculateShapeAligningTransform(mesh: TriangleMesh3D, target: TriangleMesh3D): Transform3D = {
    val t: RigidTransformation[_3D] = LandmarkRegistration.rigid3DLandmarkRegistration(
      mesh.pointSet.points.zip(target.pointSet.points).toSeq,
      Point(0,0,0)
    )

    val transform = new Transform3D{
      override def apply(x: Point[_3D]): Point[_3D] = t(x)
      override def apply(v: Vector[_3D]): Vector[_3D] = t.rotation(v.toPoint).toVector
    }

    transform
  }


  /**
   * Builds a color model. This model building accounts for missing color values.
   *
   * @param referenceMesh Reference mesh.
   * @param colorFields Colorfields to build the model from.
   * @param numberOfComponents Number of desired components.
   * @return Color model.
   */
  def buildColorModel(
    referenceMesh: TriangleMesh3D,
    colorFields: IndexedSeq[DiscreteField[_3D, RGBA]],
    numberOfComponents: Int
  ): PancakeDLRGP[_3D, RGB] = {

    val domain = referenceMesh.pointSet

    val meanRGBA = DiscreteField[_3D, RGBA](domain, saveMean(colorFields.map(_.data)))

    val meanFreeColors = saveMeanFreeColors(colorFields.map(_.data), meanRGBA.data)
    val meanFreeColorFields = meanFreeColors.map{ a => DiscreteField(domain, a) }

    val meanRGB = DiscreteField[_3D, RGB](domain, meanRGBA.data.map(_.toRGB))

    val kernel: MatrixValuedPDKernel[_3D] = ReducedEntryKernel(meanFreeColorFields)
    val gp: GaussianProcess[_3D, RGB] = GaussianProcess(meanRGB.interpolateNearestNeighbor(), kernel)
    val lrgp = LowRankGaussianProcess.approximateGP[_3D,RGB](gp,UniformMeshSampler3D(referenceMesh,500),numberOfComponents)
    val grf = lrgp.discretize(domain)

    PancakeDLRGP(grf)
  }

  /**
   * Calculates the mean based on available samples only.
   * If no value is available, i.e. the alpha channel is zero for all samples at a given vertex, BlackTransparent is set as mean color.
   */
  def saveMean(colorVectors: Seq[IndexedSeq[RGBA]]): IndexedSeq[RGBA] = {
    val numberOfColorValues = colorVectors.size
    val numberOfSamples = colorVectors.head.size

    val accumulatedColor = Array.fill(numberOfSamples)(RGBA.BlackTransparent)
    val numberOfUsedColors = Array.fill(numberOfSamples)(0)

    for (
      i <- 0 until numberOfColorValues;
      j <- 0 until numberOfSamples
    ) {
      val color = colorVectors(i)(j)
      if (color.a == 1.0) {
        accumulatedColor(j) = accumulatedColor(j) + color
        numberOfUsedColors(j) += 1
      }
    }

    val mean = accumulatedColor.zip(numberOfUsedColors).map {
      case (sumOfColors, counter) =>
        if (counter == 0) RGBA.BlackTransparent
        else sumOfColors / counter
    }

    mean
  }

  /**
   * Substracts the mean color vector from all color samples. Else BlackTransparent.
   */
  def saveMeanFreeColors(colorVectors: Seq[IndexedSeq[RGBA]], meanColorVector: IndexedSeq[RGBA]): Seq[IndexedSeq[RGBA]] = {
    colorVectors.map { colorVector =>
      colorVector.zip(meanColorVector).map {
        case (color, meanColor) =>
          if (color.a == 1.0) {
            val d = RGBA(color.r - meanColor.r, color.g - meanColor.g, color.b - meanColor.b, color.a)
            d
          } else {
            RGBA.BlackTransparent
          }
      }
    }
  }

  /**
   * Covariance kernel that has a backupkernel used when some of the data is missing.
   *
   * @param colorFields Input data with possibly some data missing.
   */
  case class MissingEntryKernel(
                                 colorFields: Seq[DiscreteField[_3D, RGBA]],
                                 backupKernel: MatrixValuedPDKernel[_3D] = DiagonalKernel(GaussianKernel[_3D](10), 3) * 0.0001
                               ) extends MatrixValuedPDKernel[_3D] {

    val fs = colorFields.map(f => f.interpolateNearestNeighbor())

    override protected def k(x: Point[_3D], y: Point[_3D]): DenseMatrix[Double] = {

      val correlation = fs.foldLeft(DenseMatrix.zeros[Double](outputDim, outputDim)) { (sum, field) =>
        val xc = field(x)
        val yc = field(y)
        val addend =
          if (xc.a > 0.5 && yc.a > 0.5) {
            xc.toRGB.toVector.outer(yc.toRGB.toVector).toBreezeMatrix
          } else {
            backupKernel(x, y)
          }
        sum + addend * (1.0 / fs.size)
      }

      correlation
    }

    override def domain: Domain[_3D] = RealSpace[_3D]

    override def outputDim: Int = 3
  }


  /**
    * Kernel estimating the covariance only on the available data.
    */
  case class ReducedEntryKernel(
                                 colorFields: Seq[DiscreteField[_3D, RGBA]],
                                 backupKernel: MatrixValuedPDKernel[_3D] = DiagonalKernel(GaussianKernel[_3D](5), 3) * 0.1
                               ) extends MatrixValuedPDKernel[_3D] {

    val fs = colorFields.map(f => f.interpolateNearestNeighbor())
    private val originalDomain = colorFields.head.domain
    val countEntries = DiscreteField(originalDomain,colorFields.foldLeft(
      IndexedSeq.fill[Int](originalDomain.numberOfPoints)(0)
    ) { case (sum, field) =>
        field.data.map(c => if(c.a==1.0) 1 else 0).zip(sum).map(p => p._1+p._2)
    }).interpolateNearestNeighbor()

    override protected def k(x: Point[_3D], y: Point[_3D]): DenseMatrix[Double] = {
      var count = 0

      val correlation = fs.foldLeft(DenseMatrix.zeros[Double](outputDim, outputDim)) { (sum, field) =>
        val xc = field(x)
        val yc = field(y)
        if (xc.a > 0.5 && yc.a > 0.5) {
          val addend = xc.toRGB.toVector.outer(yc.toRGB.toVector).toBreezeMatrix
          count += 1
          sum + addend
        } else {
          sum
        }
      }

      if (count>0) {
        correlation*(1.0/count)
      } else {
        backupKernel(x,y)
      }
    }

    override def domain: Domain[_3D] = RealSpace[_3D]

    override def outputDim: Int = 3
  }

}
