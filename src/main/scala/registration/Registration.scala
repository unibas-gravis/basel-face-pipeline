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

package registration

import breeze.linalg.DenseVector
import ch.unibas.cs.gravis.facepipeline._
import com.typesafe.scalalogging.LazyLogging
import ch.unibas.cs.gravis.facepipeline.BU3DDataProvider.{CoreExpression, Neutral,Sadness,Joy,Disgust,Anger,Fear,Surprise}
import registration.utils.VisualLogger
import registration.modelbuilding.FaceMask
import scalismo.common.{PointId, UnstructuredPointsDomain}
import scalismo.geometry.{Landmark, Point, _3D}
import scalismo.mesh.{MeshBoundaryPredicates, TriangleMesh, TriangleMesh3DOperations}
import scalismo.numerics.{LBFGSOptimizer, Sampler, UniformMeshSampler3D}
import scalismo.registration._
import scalismo.statisticalmodel.{DiscreteLowRankGaussianProcess, StatisticalMeshModel}
import scalismo.utils.Random

case class Registration(dataProvider: DataProvider)(implicit rng: Random) extends PipelineStep with LazyLogging {

  type CoefficientVector = DenseVector[Double]

  case class LandmarkPair(referenceLandmark: Landmark[_3D], targetLandmark: Landmark[_3D])

  case class LevelConfig(regularizationWeight : Double, outlierThreshold : Option[Double], numBasisFunctions : Int)

  case class OutlierAwarePointSampler(referenceMesh: TriangleMesh[_3D], sampledNumberOfPoints: Int, isValidTargetPoint: Point[_3D] => Boolean) extends Sampler[_3D] with LazyLogging {


    private val points = UniformMeshSampler3D(referenceMesh, sampledNumberOfPoints).sample().map(_._1)
    private val validPointsOnly = points.filter(isValidTargetPoint)
    override val numberOfPoints: Int = validPointsOnly.size
    logger.info(s"sampling $numberOfPoints points")
    override def volumeOfSampleRegion: Double = referenceMesh.area

    override def sample(): IndexedSeq[(Point[_3D], Double)] = {
      validPointsOnly.map(p => (p, 1.0 / referenceMesh.area))
    }

  }

  def registration(gpModel: StatisticalMeshModel,
                   targetMesh: TriangleMesh[_3D],
                   faceMask : FaceMask,
                   landmarkPairs: Seq[LandmarkPair])(implicit rng: Random): TriangleMesh[_3D] = {

    val referenceMesh = gpModel.referenceMesh

    VisualLogger.showTargetMesh(targetMesh)

    val landmarkConstraints = for (landmarkPair <- landmarkPairs.toIndexedSeq) yield {
      val referencePointId = referenceMesh.pointSet.findClosestPoint(landmarkPair.referenceLandmark.point).id
      val targetPoint = landmarkPair.targetLandmark.point
      (referencePointId, targetPoint, landmarkPair.referenceLandmark.uncertainty.get)
    }

    val posteriorModel = gpModel.posterior(landmarkConstraints)

    VisualLogger.ui.map(_.show(posteriorModel,"M"))

    var initialCoefficients = DenseVector.zeros[Double](posteriorModel.rank)

    val levelConfigs = Seq(LevelConfig(1.0, None, gpModel.rank),
                           LevelConfig(1E-1, None, gpModel.rank),
                           LevelConfig(1E-3, None, gpModel.rank),
                          LevelConfig(1E-4, Some(4.0), gpModel.rank),
                          LevelConfig(1E-5, Some(2.0), gpModel.rank),
                          LevelConfig(1E-6, Some(1.0), gpModel.rank)
    )
    val finalCoefficients = levelConfigs.foldLeft[DenseVector[Double]](initialCoefficients){
      case(currentCoefficients, levelConfig) => {
        registrationForLevel(posteriorModel, targetMesh, faceMask, levelConfig, numberOfIterations = 20, currentCoefficients)
      }
    }

    posteriorModel.instance(finalCoefficients)

  }

  def registrationForLevel(gpModel: StatisticalMeshModel,
                           targetMesh : TriangleMesh[_3D],
                           faceMask: FaceMask,
                           levelConfig : LevelConfig,
                           numberOfIterations: Int,
                           initialCoefficients: CoefficientVector)(implicit rng: Random): CoefficientVector = {

    val LevelConfig(regularizationWeight, outlierThreshold,  numBasisFunctions) = levelConfig


    val reducedGPModel = reduceModel(gpModel, numBasisFunctions)
    val reducedInitialCoefficients = initialCoefficients(0 until numBasisFunctions)

    val referenceMesh = reducedGPModel.referenceMesh
    val currentFit = reducedGPModel.instance(reducedInitialCoefficients)


    VisualLogger.showStatisticalShapeModel(reducedGPModel)
    VisualLogger.updateModelView(reducedInitialCoefficients)

    // here we need to compute a new posterior based on the line landmarks

    def isValidTargetPoint(currentFit: TriangleMesh[_3D],
                           targetMeshOps: TriangleMesh3DOperations,
                           targetMeshBoundary: UnstructuredPointsDomain[_3D])
                          (p: Point[_3D]): Boolean = {

      val ptId = referenceMesh.pointSet.findClosestPoint(p).id
      val closestPt = targetMeshOps.closestPointOnSurface(currentFit.pointSet.point(ptId))
      val closestPtId = targetMesh.pointSet.findClosestPoint(closestPt.point).id

      def isOnValidBoundary(ptId : PointId, closestPtId : PointId) : Boolean = {

        if(faceMask.isLipPoint(ptId)) {
          true
        } else {
          (closestPt.point - targetMeshBoundary.findClosestPoint(closestPt.point).point).norm > 8.0 // Points that are close to a border
        }

      }

      def getOutlierTreshold(ptId : PointId) : Double = {

        if(faceMask.isLipPoint(ptId)) {
          Double.MaxValue
        } else {
          outlierThreshold.getOrElse(Double.MaxValue)
        }
      }


      Math.sqrt(closestPt.distanceSquared) < getOutlierTreshold(ptId) &&
        isOnValidBoundary(ptId,closestPtId) && !faceMask.isEarRegion(ptId) && !faceMask.isNoseRegion(ptId)
    }

    val targetMeshBoundaryPred = MeshBoundaryPredicates(targetMesh)
    val targetMeshBoundary = UnstructuredPointsDomain(targetMesh.pointSet.pointIds
      .filter(targetMeshBoundaryPred.pointIsOnBoundary)
      .map(targetMesh.pointSet.point).toIndexedSeq
    )

    val optimizationPointSampler = OutlierAwarePointSampler(referenceMesh,
      sampledNumberOfPoints = referenceMesh.pointSet.numberOfPoints,
      isValidTargetPoint(currentFit, targetMesh.operations, targetMeshBoundary))


    val transformationSpace = GaussianProcessTransformationSpace(reducedGPModel.gp.interpolateNearestNeighbor)

    // Scalismo implements registration always as image to image registration.
    // Therefore we compute distance images from the meshes
    val fixedImage = referenceMesh.operations.toDistanceImage
    val movingImage = targetMesh.operations.toDistanceImage

    val registrationIterator = new scalismo.registration.Registration(
      metric = MeanHuberLossMetric[_3D](fixedImage,movingImage,transformationSpace,optimizationPointSampler),
      regularizer = L2Regularizer(transformationSpace),
      regularizationWeight = regularizationWeight,
      optimizer = LBFGSOptimizer(maxNumberOfIterations = numberOfIterations)
    ).iterator(reducedInitialCoefficients)
    val iteratorWithLogging = for ((regState, itNum) <- registrationIterator.zipWithIndex) yield {
      logger.debug(s"Iteration $itNum: value = ${regState.optimizerState.value}")
      VisualLogger.updateModelView(regState.optimizerState.parameters)
      regState
    }

    val lastRegistrationState = iteratorWithLogging.toSeq.last


    val fullFinalParameters = DenseVector.zeros[Double](initialCoefficients.length)
    fullFinalParameters(0 until numBasisFunctions) := lastRegistrationState.optimizerState.parameters
    fullFinalParameters
  }


  private def reduceModel(model : StatisticalMeshModel, numBasisFunctions : Int) : StatisticalMeshModel = {
    val reducedGp = DiscreteLowRankGaussianProcess(model.gp.mean, model.gp.klBasis.take(numBasisFunctions))
    model.copy(gp = reducedGp)
  }

  override def run(): Unit = {
    // transforms the mesh using the best similarity transform between the reference and target landmarks.

    for (expression <- Seq(Neutral,Sadness,Joy,Disgust,Anger,Fear,Surprise).reverse) {

      val referenceLandmarks = dataProvider.incoming.reference.loadLandmarks(expression = if(expression == Neutral) Neutral else CoreExpression).get
      val model = dataProvider.registration.loadPriorModel(expression = if(expression == Neutral) Neutral else CoreExpression).get

      val faceMask = dataProvider.incoming.reference.loadFaceMask().get

      logger.info("Successfully loaded reference and model")

      for (id <- scala.util.Random.shuffle(dataProvider.incoming.ids(expression)) if dataProvider.registration.loadMesh(id,expression).isFailure &&  dataProvider.incoming.loadLandmarks(id,expression).isSuccess) {

        logger.info("Performing registration for id " + id)
        val targetMesh = dataProvider.incoming.loadMesh(id,expression).get
        val targetLandmarks = dataProvider.incoming.loadLandmarks(id,expression).get

        val correspondingLandmarks = correspondingLandmarkPairs(referenceLandmarks, targetLandmarks)

        val correspondingLandmarkPoints = correspondingLandmarks.map(lmPair => (lmPair.targetLandmark.point, lmPair.referenceLandmark.point))
        val alignmentTransform = LandmarkRegistration.similarity3DLandmarkRegistration(correspondingLandmarkPoints, center = Point(0.0, 0.0, 0.0))
        val alignedTargetMesh = targetMesh.transform(alignmentTransform)
        val alignedLandmarkPairs = correspondingLandmarks.map(lmPair =>
          LandmarkPair(lmPair.referenceLandmark, lmPair.targetLandmark.transform(alignmentTransform))
        )

        VisualLogger.ui.map(_.show(alignedLandmarkPairs.map(_.targetLandmark),"Test"))
        VisualLogger.ui.map(_.show(alignedLandmarkPairs.map(_.referenceLandmark),"Test"))
        val registeredMesh = registration(model, alignedTargetMesh, faceMask, alignedLandmarkPairs)

        // we realign the registered mesh with the target.
        val registeredMeshOrigSpace = registeredMesh.transform(alignmentTransform.inverse)
        dataProvider.registration.saveMesh(id,expression, registeredMeshOrigSpace)
      }
    }
  }

  private def correspondingLandmarkPairs(referenceLandmarks: Seq[Landmark[_3D]], targetLandmarks: Seq[Landmark[_3D]]): Seq[LandmarkPair] = {

    referenceLandmarks
      .map(refLm => (refLm, targetLandmarks.find(targetLm => targetLm.id == refLm.id)))
      .filter(lmTuple => lmTuple._2.nonEmpty)
      .map(lmTuple => LandmarkPair(lmTuple._1, lmTuple._2.get))
  }


}

object Registration {

  def main(args: Array[String]): Unit = {

    implicit val rng = Random(1024l)
    scalismo.initialize()
    Registration(BU3DDataProvider).run()

  }
}
