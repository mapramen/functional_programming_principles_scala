package observatory

import com.sksamuel.scrimage.{Image}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    val CellPoint(x, y) = point
    (1 - x) * ((1 - y) * d00 + y * d01) + x * ((1 - y) * d10 + y * d11)
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val sortedColors = colors.toList.sortBy(-_._1)

    def getColor(subTile: Tile): Color = {
      val location = subTile.toLocation
      val boundingGridLocations = location.boundingGridLocations

      val temperature = bilinearInterpolation(
        location.cellPoint,
        grid(boundingGridLocations(0)),
        grid(boundingGridLocations(1)),
        grid(boundingGridLocations(2)),
        grid(boundingGridLocations(3))
      )

      Visualization.getColor(sortedColors, temperature)
    }

    val pixelColors = tile.subTiles(8).par.map(getColor(_)).toArray
    Visualization.getImage(pixelColors, 256, 256, 255)
  }

}
