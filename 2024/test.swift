import Foundation

func stats(_ inp: [Int]) -> (min: Int, max: Int, sum: Int)? {
    if inp.isEmpty {
        return Optional.none
    }

    var min = inp[0]
    var max = inp[0]
    var sum = 0
    for v in inp {
        if v < min {
            min = v
        }
        if v > max {
            max = v
        }
        
        sum += v
    }

    return Optional.some((min, max, sum))
}

print(stats([]))
let st = stats([1,2,3,242,354,6565])
print(st)
print("Sum: \(st!.sum)")

func closure() -> (() -> (), () -> ()) {
    var v = 0
    func p() {
        print("Current value is: \(v)")
    }
    func inc() {
        v += 1
    }

    return (p, inc)
}

let (p, inc) = closure()
inc()
p()
inc()
p()

var numbers = [20, 19, 7, 12]
print(numbers)
// closure short form
numbers = numbers.map({ (number: Int) -> Int in
    let result = if number % 2 != 0 { 0 } else { 3 * number }
    return result
})
print(numbers)

class Foo {
    var variable: Int
    let constant: Int

    init(v v: Int, c c: Int) {
        print("Init Foo")
        self.variable = v
        self.constant = c
    }

    deinit {
        print("Deinit Foo")
    }
}

let f = Foo(v: 3, c: 1337)
print(f.variable)
print(f.constant)

// similar to Rust "enums"
enum Response {
    case success(String)
    case error(Int, String)

    func describe() -> String {
        switch self {
        case let .success(content):
            return "Success: '\(content)'"
        case let .error(httpCode, message):
            return "Error with code{\(httpCode)}: '\(message)'"
        }
    }
}

let success = Response.success("<html><body>Foo</body></html>")
let error = Response.error(404, "Not found")

print(success.describe())
print(error.describe())

protocol IFoo {
    var prefix: String { get }
    // mutating: for structures that are value types where methods normally don't
    //           modify the struct itself
    mutating func adjust(prefix pre: String)
    func describe() -> String
}

// NOTE: value type, will get copied when passed around
struct ImplFoo: IFoo {
    var prefix: String = "StructFoo: "
    var val: String
    init(_ val: String) { self.val = val }
    mutating func adjust(prefix pre: String) {
        self.prefix += pre
    }
    func describe() -> String { return "\(self.prefix)\(self.val)" }
}

var sf = ImplFoo("s")
sf.adjust(prefix: "STRUCT ")
print(sf.describe())

// add to existing type
extension Foo: IFoo {
    // extension can't contain stored properties, so use a getter instead
    // -> not trivially possible
    // -> and only solvable with ugly workarounds -> don't do it use a method instead
    var prefix: String {
        return "ClassFooExt: "
    }
    // NOTE: broken
    func adjust(prefix pre: String) {}
    func describe() -> String { return "\(self.prefix)v=\(self.variable), c=\(self.constant)" }
    // NOTE: doesn't work since the closures and prefix get re-created
    //       when called and no way (other than static) to store it
    // private func prefixHelper() -> (() -> (String), (String) -> ()) {
    //     var prefix: String = "ClassFooExt: "
    //     func setter(_ pre: String) {
    //         print("pre from \(prefix)")
    //         prefix += pre
    //         print("to \(prefix)")
    //     }
    //     // return just prefix here wouldn't work since it would not refer
    //     // to the same close over value, so we also need a getter
    //     func getter() -> String {
    //         print("Prefix is \(prefix)")
    //         return prefix
    //     }
    //     return (getter, setter)
    // }
}

let cf = Foo(v: 5, c: 420)
let cf2 = Foo(v: 7, c: 69)
cf.adjust(prefix: "CLASS ")
print(cf.describe())
print(cf2.describe())

let icf: any IFoo = cf
print(icf.describe())



