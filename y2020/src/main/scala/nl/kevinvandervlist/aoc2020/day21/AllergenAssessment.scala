package nl.kevinvandervlist.aoc2020.day21

import nl.kevinvandervlist.aoc.Resolver
import nl.kevinvandervlist.aoc2020.day21.Food.{Allergen, Ingredient}

object AllergenAssessment {
  def one(in: List[String]): Int =
    safe(in.map(parse)).size

  def two(in: List[String]): String = {
    val food = in.map(parse)
    val unsafe = allergens(food, safe(food).toSet)
    unsafe.toList.sortBy(_._1).map(_._2).mkString(",")
  }

  private def parse(line: String): Food = {
    val parts = line.split(Array(' ', '(', ')', ',')).filterNot(_.isEmpty)
    val ingredients = parts.takeWhile(_ != "contains")
    Food(ingredients.toSet, parts.drop(ingredients.length + 1).toSet)
  }

  private def safe(food: List[Food]): List[Ingredient] = {
    val maybe = maybeAllergens(food)
    val maybeIngredients: Set[String] = maybe.values.flatten.toSet
    food
      .iterator
      .flatMap(_.ingredients)
      .filterNot(maybeIngredients.contains)
      .toList
  }

  private def maybeAllergens(food: List[Food]): Map[Allergen, Set[Ingredient]] =
    food.foldLeft(Map.empty[String, Set[String]]) {
      case (state, f) => f.allergen.foldLeft(state) {
        case (acc, a) if acc.contains(a) => acc + (a -> acc(a).intersect(f.ingredients))
        case (acc, a) => acc + (a -> f.ingredients)
      }
    }

  private def allergens(food: List[Food], safe: Set[Ingredient]): Map[Ingredient, Allergen] = {
    val lookup: Map[Allergen, Set[Ingredient]] = food
      .iterator
      .map(f => {
        f.copy(ingredients = f.ingredients.diff(safe))
      })
      .flatMap(f => f.allergen.map(a => a -> f.ingredients))
      .toList
      .groupBy(_._1)
      .map { case (allergen, lst) =>
        val all: Seq[Set[Ingredient]] = lst.map(_._2)
        val maybe = all.tail.foldLeft(all.head) {
          case (acc, r) => acc.intersect(r)
        }
        allergen -> maybe
      }

    Resolver.solve[Allergen, Ingredient](lookup).head
  }
}

private object Food {
  type Ingredient = String
  type Allergen = String
}

private case class Food(ingredients: Set[Ingredient], allergen: Set[Allergen])