use "collections"

actor KmeansActor
  var _env: Env
  var _parent: Main
  var _xs: Array[Point val] val
  let _n: U64
  var _iters: U64
  var _printResult: Bool
  var _centActs: Array[CentActor tag] val = recover Array[CentActor tag] end

  let pas: Array[PointActor] = Array[PointActor]()

  var _count: U64 = 0

  var _centroids: Array[Point val] = Array[Point val]()

  new create(env: Env, xs: Array[Point val] val, n: U64, iters: U64, parent: Main, printResult: Bool = false) =>
    _env = env
    _parent = parent
    _xs = xs
    _n = n
    _iters = iters
    _printResult = printResult

    var tmp = recover Array[CentActor tag](n) end

    for i in Range(0, n) do
      try
        _centroids.push(xs(i))
        tmp.push(CentActor(env, xs, this))
      end
    end

    _centActs = consume tmp

    var i: U64 = 0
    for p in xs.values() do
      pas.push(PointActor(env, p, i, _centActs))
      i = i + 1
    end

  be step() =>
    var cent_tmp = recover Array[Point val]() end

    for c in _centroids.values() do
      cent_tmp.push(c)
    end

    let cents: Array[Point val] val = consume cent_tmp

    _centroids.clear()

    for p in pas.values() do
      p.updateClosest(cents)
    end

  be assignNewCentroids() =>
    for c in _centActs.values() do
      c.assign()
    end
    
  be setCentroid(p: Point iso) =>
    _centroids.push(consume p)
    if (_centroids.size() >= _centActs.size()) then
      _iters = _iters - 1
      if _iters > 0 then
        step()
      else
        _parent.printAndQuit()
        if _printResult then
          _env.out.print("Final centroids are:")
          for c in _centroids.values() do
            _env.out.print(c.string())
          end
        end
      end
    end

  be inc() =>
    _count = _count + 1
    if _count >= _xs.size() then
      _count = 0
      assignNewCentroids()
    end    

actor CentActor
  var _env: Env
  var _parent: KmeansActor
  var _xs: Array[Point val] val

  var _toAdd: Array[U64] = Array[U64]()

  new create(env: Env, xs: Array[Point val] val, parent: KmeansActor) =>
    _env = env
    _xs = xs
    _parent = parent

  be addPoint(i: U64) =>
    _toAdd.push(i)
    _parent.inc()

  be assign() =>
    var tmp = recover Point(0,0) end
    for p in _toAdd.values() do
      try
        tmp + _xs(p)
      end
    end
    let div = _toAdd.size().f64()
    var x = recover Point(tmp.x / div, tmp.y / div) end
    _toAdd.clear()

    _parent.setCentroid(consume x)

actor PointActor
  var _env: Env
  var _point: Point val
  var _index: U64
  var _ca: Array[CentActor] val

  new create(env: Env, p: Point val, index: U64, ca: Array[CentActor] val) =>
    _env = env
    _point = p
    _index = index
    _ca = ca

  fun dist(p1: Point val, p2: Point val): F64 =>
    (p1 - p2).modulus()

  fun closest(x: Point val, choices: Array[Point val] val): U64 =>
    var tmp : U64 = -1
    var i : U64 = 0
    var min: F64 = U32.max_value().f64()
    for p in choices.values() do
      let tmp_dist = dist(p, x)
      if tmp_dist < min then
        min = tmp_dist
        tmp = i
      end
      i = i + 1
    end
    tmp    

  be updateClosest(centroids: Array[Point val] val) =>
    let clo = closest(_point, centroids)
    try
      _ca(clo).addPoint(_index)
    end
