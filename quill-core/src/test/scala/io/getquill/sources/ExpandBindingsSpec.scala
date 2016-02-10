package io.getquill.sources

import io.getquill.Spec

class ExpandBindingsSpec extends Spec {

  "tes" in {
    val query = "SELECT ? FROM Person p WHERE p.id IN (?) AND p.age > ?"
    println(ExpandBindings(query, List(1, 11, 12)))
  }

}
