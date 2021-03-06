package observatory

import scala.math._

/**
  * Introduced in Week 1. Represents a location on the globe.
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double) {
  def gridCompatibleLocation = Location(
    max(-89.0, min(90.0, lat)),
    max(-180.0, min(179.0, lon))
  )

  def boundingGridLocations = {
    val Location(gridLat, gridLon) = this.gridCompatibleLocation

    Array(
      GridLocation(gridLat.ceil.toInt, gridLon.floor.toInt),
      GridLocation(gridLat.floor.toInt, gridLon.floor.toInt),
      GridLocation(gridLat.ceil.toInt, gridLon.ceil.toInt),
      GridLocation(gridLat.floor.toInt, gridLon.ceil.toInt)
    )
  }

  def cellPoint = {
    val Location(gridLat, gridLon) = this.gridCompatibleLocation
    CellPoint(gridLon - gridLon.floor, gridLat.ceil - gridLat)
  }
}

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  * @param x X coordinate of the tile
  * @param y Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int) {
  def toLocation = Location(
    toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1 << zoom))))), 
    x.toDouble / (1 << zoom) * 360.0 - 180.0
  )

  def subTiles(zoomLevel: Int): Iterable[Tile] = {
    val n = (1 << zoomLevel)
    for{
      j <- 0 until n;
      i <- 0 until n
    } yield Tile(n * x + i, n * y + j, zoom + zoomLevel)
  }
}

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int) {
  def toLocation = Location(lat, lon)
}

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double)

/**
  * Introduced in Week 2. Represents an RGB color.
  * @param red Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int)

