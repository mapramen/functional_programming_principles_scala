package observatory

object Main extends App {
  val temperatures = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv"))
  val colors = List(
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0)),
    (0.0, Color(0, 255, 255)),
    (12.0, Color(255, 255, 0)),
    (32.0, Color(255, 0, 0)),
    (60.0, Color(255, 255, 255))
  )

  def generateImage(year: Year, tile: Tile, temperatures: Iterable[(Location, Temperature)]) = {
    import java.io._

    val image = Interaction.tile(temperatures, colors, tile)
    val file = new File(s"target/temperatures/${year}/${tile.zoom}/${tile.x}-${tile.y}.png")
    file.getParentFile().mkdirs()
    image.output(file)
  }

  Interaction.generateTiles(
    List((2015, temperatures)),
    generateImage
  )
}
