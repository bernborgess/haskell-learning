
// 2. Here's a sum type defined in Haskell:
/*
data Shape = Circle Float
           | Rect Float Float
*/
// When we want to define a function like `area`
// that acts on a `Shape`, we do it by pattern
// matching on the two constructors:
/*
 * area :: Shape -> Float
 * area (Circle r) = pi * r * r
 * area (Rect d h) = d * h
 */
// Implement `Shape` in C++ or Java as an interface
// and create two classes: `Circle` and `Rect`.
// Implement `area` as a virtual function.

interface Shape {
  public float area();
}

class Circle implements Shape {
  private float _pi = 3.1415f;
  private float _r;

  public Circle(float r) {
    _r = r;
  }

  public float area() {
    return _pi * _r * _r;
  }

  // ! Ex3
  public float circ() {
    return 2.0f * _pi * _r;
  }
}

class Rect implements Shape {
  private float _d;
  private float _h;

  public Rect(float d, float h) {
    _d = d;
    _h = h;
  }

  public float area() {
    return _d * _h;
  }

  // ! Ex3
  public float circ() {
    return 2.0f * (_d + _h);
  }
}

// ? Ex4
class Square implements Shape {
  private float _s;

  public Square(float s) {
    _s = s;
  }

  public float area() {
    return _s * _s;
  }

  public float circ() {
    return 4.0f * _s;
  }
}

public class Ex2 {
  public static void main(String[] args) {
    Circle c = new Circle(1.0f);
    System.out.printf("R = %f\n", c.area());
  }
}
// ! 3 Continuing with the previous example: We can easily
// add a new function `circ` that calculates the circumference
// of a `Shape`. We can do it without touching the definition of
// `Shape`:
/*
 * circ :: Shape -> Float
 * circ (Circle r) = 2.0 * pi * r
 * circ (Rect d h) = 2.0 * (d + h)
 */
// ! Add `circ` to you C++ or Java implementation. What parts of
// the original code did you have to touch?

// ? 4 Continuing further: Add a new shape, `Square`, to `Shape`
// ? and make all the necessary updates. What code did you have
// to touch in Haskell vs. C++ or Java?
