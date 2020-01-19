import Foundation

protocol BillingStrategy {
    func calculatePrice(_ rawPrice: Int) -> Int
}

class HappyHourStrategy : BillingStrategy {
    func calculatePrice(_ rawPrice: Int) -> Int {
        return rawPrice / 2
    }
}

class NormalStrategy : BillingStrategy {
    func calculatePrice(_ rawPrice: Int) -> Int {
        return rawPrice
    }
}

class Customer {
    private var drinks = [Int]()
    var billingStrategy: BillingStrategy
    
    init(billingStrategy: BillingStrategy) {
        self.billingStrategy = billingStrategy
    }

    func printBill() {
        let sum = drinks.reduce(0, +)
        print(String(format: "Total due: %.2f", Double(sum) / 100.0))
        drinks.removeAll()
    }
    
    func addDrink(rawPrice: Int, quantity: Int = 1) {
        drinks.append(billingStrategy.calculatePrice(rawPrice * quantity))
    }
}

let customer = Customer(billingStrategy: NormalStrategy())
customer.addDrink(rawPrice: 4)
customer.addDrink(rawPrice: 4, quantity: 2)

customer.billingStrategy = HappyHourStrategy()
customer.addDrink(rawPrice: 10, quantity: 2)

customer.printBill()
