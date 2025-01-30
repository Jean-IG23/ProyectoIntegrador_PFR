# ProyectoIntegrador_PFR
Repositorio del proyecto integrador - Programación funcional y reactica
# Tablas de Datos

### Columnas, Tipos, Propósito y Observaciones

| **Nombre de Columna**         | **Tipo**       | **Propósito y Observaciones**                                                                                |
|-------------------------------|---------------|-------------------------------------------------------------------------------------------------------------|
| `adult`                       | `bool`        | Indica si la película es solo para adultos.                                                                |
| `belongs_to_collection`       | `object`      | Representa la colección a la que pertenece la película, si aplica.                                         |
| `budget`                      | `int64`       | Presupuesto asignado para la producción de la película.                                                    |
| `genres`                      | `object`      | Lista de géneros asociados con la película.                                                                |
| `homepage`                    | `object`      | Página web oficial de la película.                                                                         |
| `id`                          | `int64`       | Identificador único para cada película (clave primaria).                                                   |
| `imdb_id`                     | `object`      | Identificador único de la película en IMDb.                                                                |
| `original_language`           | `object`      | Idioma original de la película (código ISO 639-1).                                                         |
| `original_title`              | `object`      | Título original de la película.                                                                            |
| `overview`                    | `object`      | Resumen o sinopsis de la película.                                                                         |
| `popularity`                  | `float64`     | Medida de popularidad basada en diversos factores como interacciones y búsquedas.                          |
| `poster_path`                 | `object`      | Ruta del póster oficial de la película.                                                                    |
| `production_companies`        | `object`      | Lista de empresas que participaron en la producción de la película.                                        |
| `production_countries`        | `object`      | Países donde se produjo la película.                                                                       |
| `release_date`                | `object`      | Fecha de estreno de la película.                                                                           |
| `revenue`                     | `int64`       | Ingresos generados por la película.                                                                        |
| `runtime`                     | `int64`       | Duración de la película en minutos.                                                                        |
| `spoken_languages`            | `object`      | Idiomas hablados en la película.                                                                           |
| `status`                      | `object`      | Estado de la película (por ejemplo, lanzada, postproducción, etc.).                                         |
| `tagline`                     | `object`      | Frase o eslogan asociado con la película.                                                                  |
| `title`                       | `object`      | Título de la película.                                                                                     |
| `video`                       | `bool`        | Indica si el registro es de un video (generalmente para trailers).                                         |
| `vote_average`                | `float64`     | Promedio de votos recibidos por la película.                                                              |
| `vote_count`                  | `int64`       | Número total de votos recibidos por la película.                                                          |
| `keywords`                    | `object`      | Palabras clave asociadas con la película.                                                                 |
| `cast`                        | `object`      | Lista de actores que participaron en la película.                                                         |
| `crew`                        | `object`      | Lista de miembros del equipo técnico que trabajaron en la película.                                       |
| `ratings`                     | `object`      | Calificaciones detalladas recibidas por la película.                                                      |

```scala
import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._
import java.io.File

// Definición de la case class
case class Peliculas(
                      adult: Boolean,
                      belongs_to_collection: String,
                      budget: Int,
                      genres: String,
                      homepage: String,
                      id: Int,
                      imdb_id: String,
                      original_language: String,
                      original_title: String,
                      overview: String,
                      popularity: Float,
                      poster_path: String,
                      production_companies: String,
                      production_countries: String,
                      release_date: String,
                      revenue: Int,
                      runtime: Int,
                      spoken_languages: String,
                      status: String,
                      tagline: String,
                      title: String,
                      video: Boolean,
                      vote_average: Float,
                      vote_count: Int,
                      keywords: String,
                      cast: String,
                      crew: String,
                      ratings: String
                    )

object PeliculasStats extends App {
  val filePath = "data/pi_movies_small.csv"

  // Leer el archivo CSV con ';' como delimitador
  val dataSource = new File(filePath).readCsv[List, Peliculas](rfc.withHeader.withCellSeparator(';'))

  // Filtrar las filas exitosas y extraer solo las películas válidas
  val peliculas = dataSource.collect { case Right(pelicula) => pelicula }

  // Función para calcular estadísticas descriptivas básicas de una lista de números
  def calcularEstadisticas(datos: Seq[Double], nombreColumna: String): Unit = {
    if (datos.isEmpty) {
      println(s"No hay datos para la columna '$nombreColumna'")
    } else {
      val count = datos.size
      val sum = datos.sum
      val mean = sum / count
      val minVal = datos.min
      val maxVal = datos.max
      val variance = datos.map(x => math.pow(x - mean, 2)).sum / count
      val stddev = math.sqrt(variance)
      println(s"--- Estadísticas para '$nombreColumna' ---")
      println(f"Conteo: $count")
      println(f"Media: $mean%.2f")
      println(f"Mínimo: $minVal%.2f")
      println(f"Máximo: $maxVal%.2f")
      println(f"Desviación Estándar: $stddev%.2f\n")
    }
  }

  // Extraer columnas numéricas y calcular estadísticas
  val budgets = peliculas.map(_.budget.toDouble).filter(_ >= 0)
  val popularities = peliculas.map(_.popularity.toDouble).filter(_ >= 0)
  val revenues = peliculas.map(_.revenue.toDouble).filter(_ >= 0)
  val runtimes = peliculas.map(_.runtime.toDouble).filter(_ >= 0)
  val voteAverages = peliculas.map(_.vote_average.toDouble).filter(_ >= 0)
  val voteCounts = peliculas.map(_.vote_count.toDouble).filter(_ >= 0)

  // Calcular estadísticas para cada columna numérica
  calcularEstadisticas(budgets, "Budget")
  calcularEstadisticas(popularities, "Popularity")
  calcularEstadisticas(revenues, "Revenue")
  calcularEstadisticas(runtimes, "Runtime")
  calcularEstadisticas(voteAverages, "Vote Average")
  calcularEstadisticas(voteCounts, "Vote Count")
}

```
### Estadísticas para 'Budget'
- **Conteo:** 99  
- **Media:** 3,588,282.83  
- **Mínimo:** 0.00  
- **Máximo:** 130,000,000.00  
- **Desviación Estándar:** 18,723,357.91  

### Estadísticas para 'Popularity'
- **Conteo:** 99  
- **Media:** 2.40  
- **Mínimo:** 0.00  
- **Máximo:** 26.88  
- **Desviación Estándar:** 5.00  

### Estadísticas para 'Revenue'
- **Conteo:** 99  
- **Media:** 16,625,218.92  
- **Mínimo:** 0.00  
- **Máximo:** 847,423,452.00  
- **Desviación Estándar:** 100,131,385.84  

### Estadísticas para 'Runtime'
- **Conteo:** 99  
- **Media:** 99.17  
- **Mínimo:** 0.00  
- **Máximo:** 360.00  
- **Desviación Estándar:** 43.71  

### Estadísticas para 'Vote Average'
- **Conteo:** 99  
- **Media:** 5.43  
- **Mínimo:** 0.00  
- **Máximo:** 9.50  
- **Desviación Estándar:** 2.37  

### Estadísticas para 'Vote Count'
- **Conteo:** 99  
- **Media:** 257.89  
- **Mínimo:** 0.00  
- **Máximo:** 6,656.00  
- **Desviación Estándar:** 1,034.90  

### 📝 **Código**

```scala
// Función para analizar estadísticas de la columna 'title'
def analizarTitulos(titulos: Seq[String]): Unit = {
  if (titulos.isEmpty) {
    println("No hay títulos disponibles para analizar.")
  } else {
    val totalTitulos = titulos.size
    val titulosUnicos = titulos.distinct.size

    val tituloMasLargo = titulos.maxBy(_.length)
    val longitudPromedio = titulos.map(_.length).sum.toDouble / totalTitulos

    println(s"--- Análisis de la columna 'title' ---")
    println(s"Número total de títulos: $totalTitulos")
    println(s"Número de títulos únicos: $titulosUnicos")
    println(s"Título más largo: $tituloMasLargo")
    println(f"Longitud promedio de los títulos: $longitudPromedio%.2f\n")

    val topTitulosFrecuentes = titulos
      .groupBy(identity)
      .view.mapValues(_.size)
      .toSeq
      .sortBy(-_._2)
      .take(5)

    println("Frecuencia de los 5 títulos más comunes:")
    topTitulosFrecuentes.foreach { case (title, count) =>
      println(f"$title%-50s $count")
    }
    println()
  }
}

// Analizar la columna 'title' de las películas
val titulos = peliculas.map(_.title)
analizarTitulos(titulos)

// Función para analizar estadísticas de los idiomas
def analizarIdiomas(idiomas: Seq[String]): Unit = {
  if (idiomas.isEmpty) {
    println("No hay idiomas disponibles para analizar.")
  } else {
    val totalIdiomas = idiomas.size
    val idiomasUnicos = idiomas.distinct.size

    println(s"--- Análisis de la columna 'original_language' ---")
    println(s"Número total de idiomas: $totalIdiomas")
    println(s"Número de idiomas únicos: $idiomasUnicos")

    val topIdiomasFrecuentes = idiomas
      .groupBy(identity)
      .view.mapValues(_.size)
      .toSeq
      .sortBy(-_._2)
      .take(5)

    println("Frecuencia de los 5 idiomas más comunes:")
    topIdiomasFrecuentes.foreach { case (lang, count) =>
      println(f"$lang%-10s $count")
    }
    println()
  }
}

// Analizar la columna 'original_language' de las películas
val idiomas = peliculas.map(_.original_language)
analizarIdiomas(idiomas)

```

### 📊 **Resultados del Análisis**

---

#### Análisis de la columna 'title'

| Métrica                         | Valor                                                   |
|---------------------------------|---------------------------------------------------------|
| **Número total de títulos**     | 99                                                      |
| **Número de títulos únicos**    | 98                                                      |
| **Título más largo**            | *Lock, Stock and Two Smoking Barrels*                  |
| **Longitud promedio de títulos**| 72.00                                                   |

**Frecuencia de los 5 títulos más comunes:**

| Título                                         | Frecuencia |
|------------------------------------------------|------------|
| Unicorn City                                   | 2          |
| Unguarded                                      | 1          |
| Eddie: The Sleepwalking Cannibal               | 1          |
| Follow Me: The Yoni Netanyahu Story            | 1          |
| Quints                                         | 1          |

---

#### Análisis de la columna 'original_language'

| Métrica                          | Valor                                                  |
|----------------------------------|--------------------------------------------------------|
| **Número total de idiomas**      | 99                                                     |
| **Número de idiomas únicos**     | 14                                                     |

**Frecuencia de los 5 idiomas más comunes:**

| Idioma | Frecuencia |
|--------|------------|
| en     | 75         |
| fr     | 7          |
| da     | 3          |
| it     | 2          |
| es     | 2          |

---

 ### Consultar sobre librería play-json (trabajo json en scala) y hacer: Usar cualquier JSON pequeño para aprender play-json, Usar en algunas columnas JSON para obtener datos.



```scala
import play.api.libs.json._

object Main extends App {
  // -----------------------------------
  // PARTE 1: JSON PEQUEÑO
  // -----------------------------------

  // JSON de ejemplo pequeño como String
  val simpleJsonString: String =
    """{
      |  "id": 1,
      |  "name": "John Doe",
      |  "age": 30,
      |  "email": "john.doe@example.com"
      |}""".stripMargin

  println("---- Parte 1: JSON Pequeño ----")
  println(s"JSON Original:\n$simpleJsonString")

  // Convertir el JSON a un objeto JsValue
  val simpleJson: JsValue = Json.parse(simpleJsonString)

  // Acceder a valores específicos
  val id = (simpleJson \ "id").as[Int]        // Obtiene el valor de la clave "id"
  val name = (simpleJson \ "name").as[String] // Obtiene el valor de la clave "name"

  println(s"ID: $id")
  println(s"Nombre: $name")

  // Modificar el JSON (agregar una clave nueva)
  val updatedJson = simpleJson.as[JsObject] + ("status" -> JsString("active"))
  println(s"JSON Modificado:\n${Json.prettyPrint(updatedJson)}")

  // -----------------------------------
  // PARTE 2: JSON COMPLEJO (COLUMNAS)
  // -----------------------------------

  // JSON complejo con varias columnas y estructuras anidadas
  val complexJsonString: String =
    """{
      |  "id": 2,
      |  "name": "Jane Doe",
      |  "age": 25,
      |  "contact": {
      |    "email": "jane.doe@example.com",
      |    "phone": "123-456-7890"
      |  },
      |  "address": {
      |    "city": "Springfield",
      |    "zip": "12345"
      |  }
      |}""".stripMargin

  println("\n---- Parte 2: JSON Complejo ----")
  println(s"JSON Complejo:\n$complexJsonString")

  // Convertir el JSON complejo a un objeto JsValue
  val complexJson: JsValue = Json.parse(complexJsonString)

  // Acceder a valores específicos (columnas)
  val email = (complexJson \ "contact" \ "email").as[String]
  val city = (complexJson \ "address" \ "city").as[String]

  println(s"Email: $email")
  println(s"Ciudad: $city")

  // Extraer varias columnas a la vez (como un mapa)
  val extractedColumns: Map[String, String] = Map(
    "name" -> (complexJson \ "name").as[String],
    "email" -> (complexJson \ "contact" \ "email").as[String],
    "city" -> (complexJson \ "address" \ "city").as[String]
  )

  println("\nColumnas extraídas:")
  extractedColumns.foreach { case (key, value) => println(s"$key: $value") }
}
```

<img width="866" alt="image" src="https://github.com/user-attachments/assets/6e164807-102c-4904-b200-3973d7c32145" />


<img width="854" alt="image" src="https://github.com/user-attachments/assets/ae70cf5f-d622-4431-af75-9814b6385968" />
