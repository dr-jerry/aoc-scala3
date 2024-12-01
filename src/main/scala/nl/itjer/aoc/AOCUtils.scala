package nl.itjer.aoc
import sttp.client4.quick.*
import sttp.client4.Response
import io.Source
import sttp.model.Header

class AOCUtils {
  def getFromAOC(year: Int, day: Int): String = 
    val cookieContents = Source
      .fromInputStream(this.getClass().getResourceAsStream("/sessionID"))
      .getLines().toList(0)
    val response: Response[String] = quickRequest.withHeaders(List(new Header("cookie", cookieContents)))
        .get(uri"https://adventofcode.com/$year/day/$day/input")
        .send()

    println(s"the response '${response.body}'")
    return response.body
  

}
