
val map = Map(1 -> 2, 2 -> 3)
val k1 = map.keySet
val k2 = map.keySet
k1 eq k2
(k1 zip k2).map(pair => pair._1 eq pair._2)