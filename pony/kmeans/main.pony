use "time"
use "json"
use "files"

actor Main
  var _env: Env
  var xs: Array[Point val] val = recover Array[Point val] end

  let n: U64 = 10
  let iters: U64 = 15

  let iterations: U64 = 3
  var _i: U64 = 0

  var _initTime: U64 = 0

  new create(env: Env) =>
    _env = env

    let caps = recover val FileCaps.set(FileRead).set(FileStat) end

    var fileString = ""
    try
      with file = OpenFile(FilePath(env.root, "../points.json", caps)) as File do
        for line in file.lines() do
          fileString = fileString + line
        end
      end
    end


    let doc: JsonDoc = JsonDoc
    try
      doc.parse(fileString)

      let size = (doc.data as JsonArray).data.size()

      var tmp = recover Array[Point val](size) end

      for el in (doc.data as JsonArray).data.values() do
        let x = ((el as JsonArray).data(0) as F64)
        let y = ((el as JsonArray).data(1) as F64)
        let p = recover val Point(x, y) end
        tmp.push(p)
      end

      xs = consume tmp
    end

    _initTime = Time.millis()

    _i = 1    
    iteration()
    
  be iteration() =>
    let cents = KmeansActor(_env, xs, n, iters, this)

    cents.step()
   
  be printAndQuit() =>
    if _i >= iterations then
      let after = Time.millis()
        
      let t = (after - _initTime) / iterations
      _env.out.print("Average time is "+ t.string())
    else
      _i = _i + 1
      iteration()
    end