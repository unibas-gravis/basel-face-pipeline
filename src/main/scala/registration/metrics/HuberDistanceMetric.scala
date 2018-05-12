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

package registration.metrics

import scalismo.common.Domain
import scalismo.geometry.{Dim, NDSpace, Point}
import scalismo.image.{DifferentiableScalarImage, ScalarImage}
import scalismo.numerics.Sampler
import scalismo.registration.{ImageMetric, Transformation}


// use from scalismo
//case class HuberDistanceMetric[D <: Dim: NDSpace](sampler: Sampler[D]) extends ImageMetric[D] {
//
//
//  override val ndSpace = implicitly[NDSpace[D]]
//
//  def value(fixedImage: ScalarImage[D], movingImage: ScalarImage[D], transform: Transformation[D]) = {
//    val warpedImage = fixedImage.compose(transform)
//
//
//    def rhoHuber(v : Float ) : Float = {
//      val k = 1.345
//      if (v < k)
//        (v * v / 2f) / (1 + v * v)
//      else
//        (k * ( Math.abs(v) - k / 2 )).toFloat
//    }
//    integrator.integrateScalar((warpedImage - movingImage).andThen(rhoHuber _)) / integrator.sampler.volumeOfSampleRegion
//  }
//
//
//  def takeDerivativeWRTToTransform(fixedImage: DifferentiableScalarImage[D], movingImage: ScalarImage[D], transform: Transformation[D]) = {
//
//    def psiHuber(v : Float) : Float = {
//      val k = 1.345
//      if (v < k) v else (k * Math.signum(v)).toFloat
//    }
//
//    val movingGradientImage = fixedImage.differentiate
//    val warpedImage = fixedImage.compose(transform)
//    val dDMovingImage = (warpedImage - movingImage).andThen(psiHuber _) * (1.0 / sampler.volumeOfSampleRegion)
//
//    val fullMetricGradient = (x: Point[D]) => {
//      val domain = Domain.intersection(warpedImage.domain, dDMovingImage.domain)
//      if (domain.isDefinedAt(x))
//        Some(movingGradientImage(transform(x)).toFloatBreezeVector * dDMovingImage(x))
//      else None
//    }
//
//    fullMetricGradient
//  }
//}