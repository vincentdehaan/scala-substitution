val ys = init(xs)
= init(List(1, 2, 3, 4, 5))
= List(1, 2, 3, 4, 5) match {
    case Nil => throw new Exception("Nil has no init")
    case x :: Nil => Nil
    case x :: xs => x :: init(xs)
}

List(1, 2) match {
    case 1 :: List(2) => 1 :: init(List(2))
}

    init(List(2)) =
    List(2) match {
        case 2 :: Nil => Nil
    }
    = Nil

= 1 :: Nil
= List(1)