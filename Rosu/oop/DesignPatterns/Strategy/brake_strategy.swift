protocol BrakeBehavior {
    func brake()
}

class BrakeWithABS : BrakeBehavior {
    func brake() {
        print("Brake with ABS actionated")
    }
}

class Brake : BrakeBehavior {
    func brake() {
        print("Simple Brake action")
    }
}

protocol Car {
    var brakeBehavior: BrakeBehavior { get set }
    
    func applyBrake()
}

extension Car {
    func applyBrake() {
        brakeBehavior.brake()
    }
}

class SUV : Car {
    var brakeBehavior: BrakeBehavior = BrakeWithABS()
}

class Sedan: Car {
    var brakeBehavior: BrakeBehavior = Brake()
}

let carSedan: Car = Sedan()
var carSuv: Car = SUV()

carSedan.applyBrake()
carSuv.applyBrake()

carSuv.brakeBehavior = Brake()
carSuv.applyBrake()
