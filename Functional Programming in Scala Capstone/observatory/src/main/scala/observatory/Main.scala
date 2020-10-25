package observatory

object Main extends App {
  val yearlyTemperatures = (1975 to 2015).par
    .map(year => (year, Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(year, "/stations.csv", s"/${year}.csv"))))
    .toArray

  val normalTemperature = Manipulation.average(yearlyTemperatures.filter(_._1 <= 1990).map(_._2))

  def generateImage(year: Year, tile: Tile, temperatures: Iterable[(Location, Temperature)]) = {
    import java.io._

    val image = Interaction.tile(temperatures, Interaction2.temperatureColors, tile)
    val file = new File(s"target/temperatures/${year}/${tile.zoom}/${tile.x}-${tile.y}.png")
    file.getParentFile().mkdirs()
    image.output(file)
  }

  Interaction.generateTiles(
    yearlyTemperatures.filter(_._1 > 1990),
    generateImage
  )
}
