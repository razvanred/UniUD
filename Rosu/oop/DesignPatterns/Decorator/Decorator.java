public class Decorator {

    public static void main(final String[] args) {
        final Shape circle = new Circle();

        final Shape redCircle = new RedShapeDecorator(new Circle());
        final Shape redRectangle = new RedShapeDecorator(new Rectangle());

        System.out.println("Circle with normal border");
        circle.draw();

        System.out.println("\nCircle of red border");
        redCircle.draw();

        System.out.println("\nRectangle of red border");
        redRectangle.draw();
    }

    interface Shape {
        void draw();
    }

    static abstract class ShapeDecorator implements Shape {
        protected Shape decoratedShape;

        public ShapeDecorator(final Shape decoratedShape) {
            this.decoratedShape = decoratedShape;
        }

        @Override
        public void draw() {
            decoratedShape.draw();
        }
    }

    static class RedShapeDecorator extends ShapeDecorator {
        public RedShapeDecorator(final Shape decoratedShape) {
            super(decoratedShape);
        }

        @Override
        public void draw() {
            super.draw();
            setRedBorder(decoratedShape);
        }

        private void setRedBorder(final Shape decoratedShape) {
            System.out.println("Border color: Red");
        }
    }

    static class Rectangle implements Shape {
        @Override
        public void draw() {
            System.out.println("Shape: Rectangle");
        }
    }

    static class Circle implements Shape {
        @Override
        public void draw() {
            System.out.println("Shape: Circle");
        }
    }
}