
class Point
  var x: F64
  var y: F64

  new create(x': F64, y': F64) =>
    x = x'
    y = y'

  fun string(): String =>
    "Point{ x: "+x.string( where prec = 16)+", y: "+y.string( where prec = 16)+"}"

  fun ref add(p: Point val) =>
    x = x + p.x
    y = y + p.y

  fun sub(p: Point val): Point => Point(x - p.x, y - p.y)

  fun div(d: F64): Point => Point(x / d , y / d)
    
  fun _sq(v: F64): F64 => v*v

  fun modulus(): F64 => (_sq(x) + _sq(y)).sqrt()
