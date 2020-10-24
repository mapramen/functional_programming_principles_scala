package observatory

import com.sksamuel.scrimage.{Image, RGBColor}
import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val radius: Double = 6371.0
    val p: Double = 6.0

    def distance(a: Location, b: Location): Double = {
      val phi1: Double = a.lat.toRadians
      val phi2: Double = b.lat.toRadians
      val delLambda: Double = a.lon.toRadians - b.lon.toRadians

      radius * acos(sin(phi1) * sin(phi2) + cos(phi1) * cos(phi2) * cos(delLambda))
    }

    val weightedTemperatures = {
      val parTemperatures = temperatures.par
      
      val exactTemperatures = parTemperatures
        .filter(_._1 == location)

      if(!exactTemperatures.isEmpty){
        exactTemperatures.map(x => (x._2.toDouble, 1.0))
      }
      else{
        val distanceAndTemperatures = parTemperatures
          .map(x => (distance(x._1, location), x._2))

        val nearByTemperatures = distanceAndTemperatures
          .filter(_._1 < 1)

        if(!nearByTemperatures.isEmpty){
          nearByTemperatures.map(x => (x._2.toDouble, 1.0))
        }
        else{
          distanceAndTemperatures
            .map(x => (pow(x._1, -p), x._2))
            .map(x => (x._1 * x._2, x._1))
        }
      }
    }
    
    val (weightedTemperaturesSum, weightSum) 
      = weightedTemperatures.reduce((a, b) => (a._1 + b._1, a._2 + b._2))

    weightedTemperaturesSum / weightSum
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val sortedPoints = points.toList.sortBy(-_._1)
    getColor(sortedPoints, value)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val locations = {
      for(i <- -90.until(90, 1);
        j <- -180.until(180, 1)
      ) yield Location(-i, j)
    }

    visualize(locations, temperatures, colors, 360, 180, 255)
  }

  def visualize(
    locations: Iterable[Location],
    temperatures: Iterable[(Location, Temperature)], 
    colors: Iterable[(Temperature, Color)],
    width: Int,
    height: Int,
    transparency: Int): Image =
  {
    val sortedColors = colors.toList.sortBy(-_._1)
    
    val pixels = locations.par
      .map(location => getColor(sortedColors, predictTemperature(temperatures, location)))
      .map({ case Color(red, green, blue) => RGBColor(red, green, blue, transparency).toPixel})
      .toArray

    Image(width, height, pixels)
  }

  def getColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    def interpolate(x1: Temperature, y1: Int, x2: Temperature, y2: Int, x: Temperature): Int = {
      (y1 + ((y2 - y1) / (x2 - x1)) * (x - x1)).round.toInt
    }

    def interpolateColor(leftPoint: (Temperature, Color), rightPoint: (Temperature, Color), temperature: Temperature): Color = {
      if(leftPoint._1 == temperature || leftPoint == rightPoint){
        leftPoint._2
      }
      else{
        val (x1, y1) = leftPoint
        val (x2, y2) = rightPoint
        Color(
          interpolate(x1, y1.red, x2, y2.red, temperature),
          interpolate(x1, y1.green, x2, y2.green, temperature),
          interpolate(x1, y1.blue, x2, y2.blue, temperature)
        )
      }
    }

    def getColor(pointList: Iterable[(Temperature, Color)], lastPoint: (Temperature, Color), temperature: Temperature): Color = pointList match {
      case Nil => lastPoint._2
      case currentPoint :: remainingPoints => {
        if(temperature >= currentPoint._1){
          interpolateColor(currentPoint, lastPoint, temperature)
        }
        else{
          getColor(remainingPoints, currentPoint, temperature)
        }
      }
    }

    getColor(points, points.head, value)
  }
}