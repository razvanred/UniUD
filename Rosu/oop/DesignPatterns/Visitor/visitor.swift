import Foundation

protocol Visitable {
    func visit(visitor: Visitor) -> Double
}

struct Milk : Visitable {
    let price: Double

    func visit(visitor: Visitor) -> Double {
        return visitor.apply(milk: self)
    }
}

struct Tobacco : Visitable {
    let price: Double

    func visit(visitor: Visitor) -> Double {
        return visitor.apply(tobacco: self)
    }
}

struct Liquor : Visitable {
    let price: Double

    func visit(visitor: Visitor) -> Double {
        return visitor.apply(liquor: self)
    }
}

protocol Visitor {
    func apply(milk: Milk) -> Double
    func apply(tobacco: Tobacco) -> Double
    func apply(liquor: Liquor) -> Double
}

struct TaxVisitor : Visitor {

    func apply(milk: Milk) -> Double {
        return milk.price + milk.price * 0.10
    }

    func apply(tobacco: Tobacco) -> Double {
        return tobacco.price + tobacco.price * 0.50
    }

    func apply(liquor: Liquor) -> Double {
        return liquor.price + liquor.price * 0.40
    }
}

struct HolidayTaxVisitor : Visitor {

    func apply(milk: Milk) -> Double {
        return milk.price + milk.price * 0.05
    }

    func apply(tobacco: Tobacco) -> Double {
        return tobacco.price + tobacco.price * 0.60
    }

    func apply(liquor: Liquor) -> Double {
        return liquor.price + liquor.price * 0.30
    }
}

let visitor = TaxVisitor()
let holidayVisitor = HolidayTaxVisitor()

let milk = Milk(price: 1.45)
let cigarettes = Tobacco(price: 4.5)
let whisky = Liquor(price: 7.00)

print(String(format: "Milk cost: %.2f", milk.visit(visitor: visitor)))
print(String(format: "Cigarettes cost: %.2f", cigarettes.visit(visitor: visitor)))
print(String(format: "Whisky cost: %.2f", whisky.visit(visitor: visitor)))

print()

print(String(format: "Milk cost: %.2f", milk.visit(visitor: holidayVisitor)))
print(String(format: "Cigarettes cost: %.2f", cigarettes.visit(visitor: holidayVisitor)))
print(String(format: "Whisky cost: %.2f", whisky.visit(visitor: holidayVisitor)))
