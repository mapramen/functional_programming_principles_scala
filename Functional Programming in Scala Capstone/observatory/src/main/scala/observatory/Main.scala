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

  val myImage = Visualization.visualize(temperatures, colors)

  myImage.output(new java.io.File("target/some-image-4.png"))
}
