package observatory

import java.time.LocalDate

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {
  import org.apache.log4j.{Level, Logger}
  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Capstone")
      .master("local")
      .getOrCreate()

  import spark.implicits._
  
  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    import scala.io.Source

    def getTokenLines(path: String, tokenCount: Int): Seq[Array[String]] = {
      Source.fromInputStream(getClass.getResourceAsStream(path), "utf-8").getLines()
        .map(line => line.split(','))
        .filter(tokens => tokens.size == tokenCount)
        .map(tokens => tokens.map(_.trim))
        .filter(tokens => !tokens.exists(_.size == 0))
        .toSeq
    }
    
    val stations = getTokenLines(stationsFile, 4)
      .map(x => (x(0).toInt, x(1).toInt, x(2).toDouble, x(3).toDouble))

    val temperatures = getTokenLines(temperaturesFile, 5)
      .map(x => (x(0).toInt, x(1).toInt, x(2).toInt, x(3).toInt, x(4).toDouble))

    val stationsDF = spark.sparkContext.parallelize(stations)
      .toDF("stn", "wban", "latitude", "longitude")

    val temperaturesDF = spark.sparkContext.parallelize(temperatures)
      .toDF("stn", "wban", "month", "day", "temperature")
      .where($"temperature" =!= 9999.9d)
      .withColumn("temperature", lit(5) * ($"temperature" - 32) / 9)

    val joinDF = stationsDF.join(temperaturesDF, List("stn", "wban"))
      .select($"latitude", $"longitude", $"month", $"day", $"temperature")

    joinDF.as[DailyTemperatureStats]
      .collect()
      .map((x: DailyTemperatureStats) => (LocalDate.of(year, x.month, x.day), Location(x.latitude, x.longitude), x.temperature))
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    import org.apache.spark.sql.expressions.scalalang.typed

    val locationAndTemperatures = records.map(x => (x._2, x._3))

    val avgTemperatures = spark.sparkContext.parallelize(locationAndTemperatures.toSeq).toDS()
      .groupByKey(x => x._1)
      .agg(typed.avg(_._2))

    avgTemperatures.collect()
  }
}

case class DailyTemperatureStats(
  latitude: Double,
  longitude: Double,
  month: Int,
  day: Int,
  temperature: Temperature
)