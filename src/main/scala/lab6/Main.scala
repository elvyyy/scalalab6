package lab6

object Main {
  type Row = List[Int]
  type Matrix = List[Row]

  def transpose(matrix: Matrix): Matrix =
    if (matrix.isEmpty) Nil else matrix.map(_.head) :: transpose(matrix.map(_.tail))
}
