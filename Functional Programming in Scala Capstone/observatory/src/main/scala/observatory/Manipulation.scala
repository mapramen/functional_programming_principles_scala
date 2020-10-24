package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {
  val gridLocations = for{
    lat <- -89 to 90;
    lon <- -180 to 179
  } yield GridLocation(lat, lon)

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val gridLocationToTemperatureMap = gridLocations.par
      .map(gridLocation => (gridLocation, Visualization.predictTemperature(temperatures, gridLocation.toLocation)))
      .toList
      .toMap

    gridLocationToTemperatureMap
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val n = temperaturess.size
    val gridTemperatures = temperaturess.map(makeGrid)

    def avgTemperature(gridLocation: GridLocation): Temperature = {
      gridTemperatures.map(gridTemperature => gridTemperature(gridLocation)).sum / n
    }

    val gridLocationToTemperatureMap = gridLocations.par
    .map(gridLocation => (gridLocation, avgTemperature(gridLocation)))
    .toList
    .toMap

    gridLocationToTemperatureMap
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val current: GridLocation => Temperature = makeGrid(temperatures)

    val gridLocationToTemperatureMap = gridLocations
    .map(gridLocation => (gridLocation, current(gridLocation) - normals(gridLocation)))
    .toMap

    gridLocationToTemperatureMap
  }
}