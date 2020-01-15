protocol Shape {
    func draw()
}

class Circle: Shape {
    func draw() {
        print("Shape: Circle")
    }
}

class Rectangle: Shape {
    func draw() {
        print("Shape: Rectangle")
    }
}

protocol ShapeDecorator: Shape {
    var decoratedShape: Shape { get }
}

extension ShapeDecorator {
    func draw() {
        decoratedShape.draw()
    }
}

class RedShapeDecorator: ShapeDecorator {
    let decoratedShape: Shape

    required init(shape: Shape) {
        decoratedShape = shape
    }

    func draw() {
        decoratedShape.draw()
        print("Border color: Red")
    }
}

let circle = Circle()
let redCircle = RedShapeDecorator(shape: Circle())
let redRectangle = RedShapeDecorator(shape: Rectangle())

print("Circle with normal borders")
circle.draw()

print("\nCircle with red borders")
redCircle.draw()

print("\nRectangle with red borders")
redRectangle.draw()
