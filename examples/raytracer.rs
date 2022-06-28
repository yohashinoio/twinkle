// WIP

[[nomangle]]
declare func printf(fmt: ^i8, ...) -> i32;

[[nomangle]]
declare func sqrt(x: f64) -> f64;

struct Vec {
  Vec(x_: f64, y_: f64, z_: f64)
  {
    x = x_;
    y = y_;
    z = z_;
  }

  func length_squared() -> f64
  {
    return x * x + y * y + z * z;
  }

  func length() -> f64
  {
    return sqrt(length_squared());
  }

  func div(b: f64) -> Vec
  {
    return Vec{x / b, y / b, z / b};
  }

private:
  let x: f64;
  let y: f64;
  let z: f64;
}

struct Ray {
  Ray(org_: ^Vec, dir_: ^Vec)
  {
    org = org_^;
    dir = dir_^;
  }

private:
  let org: Vec;
  let dir: Vec;
}

func normalize(v: ^Vec) -> Vec
{
  // return *v / (*v).length();
}

func createCamera() -> Ray
{
  let a = Vec{50.0, 52.0, 295.6};
  let b = Vec{0.0, -0.042612, -1.0};
  let c = normalize(&b);

  return Ray{&a, &c};
}

func main() -> i32
{
  let width = 640;
  let height = 480;

  let camera = createCamera();
}
