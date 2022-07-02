// WIP

// Copyright (c) 2022 Hiramoto Ittou

// Copyright (c) 2014 hole
// This software is released under the MIT License (http://kagamin.net/hole/license.txt).
// A part of this software is based on smallpt (http://www.kevinbeason.com/smallpt/) and
// released under the MIT License (http://kagamin.net/hole/smallpt-license.txt).

declare struct FILE;

[[nomangle]]
declare func printf(format: ^i8, ...) -> i32;

[[nomangle]]
declare func fprintf(stream: ^FILE, format: ^i8, ...) -> i32;

[[nomangle]]
declare func fdopen(fd: i32, mode: ^i8) -> ^FILE;

[[nomangle]]
declare func fclose(stream: ^FILE) -> i32;

[[nomangle]]
declare func rand() -> i32;

[[nomangle]]
declare func srand(seed: u32);

[[nomangle]]
declare func sqrt(x: f64) -> f64;

[[nomangle]]
declare func malloc(size: u64) -> ^void;

[[nomangle]]
declare func free(ptr: ^void);

struct HeapArray {
  HeapArray(size: u64)
  {
    p = malloc(size);
  }

  ~HeapArray()
  {
    free(p);
  }

  func get() -> ^void
  {
    return p;
  }

private:
  let p: ^void;
}

struct File {
  File(fd: i32, mode: ^i8)
  {
    fp = fdopen(fd, mode);
  }

  ~File()
  {
    if (fp)
      fclose(fp);
  }

  func stream() -> ^FILE
  {
    return fp;
  }

private:
  let fp: ^FILE;
}

struct Vec {
  Vec()
  {
    x = 0.;
    y = 0.;
    z = 0.;
  }

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

  func add(b: &Vec) -> Vec
  {
    return Vec{x + b.x, y + b.y, z + b.z};
  }

  func mul(b: f64) -> Vec
  {
    return Vec{x * b, y * b, z * b};
  }

  func div(b: f64) -> Vec
  {
    return Vec{x / b, y / b, z / b};
  }

  let x: f64;
  let y: f64;
  let z: f64;
}

typedef Color = Vec;

func cross(v1: &Vec, v2: &Vec) -> Vec
{
  return
    Vec
    { (v1.y * v2.z) - (v1.z * v2.y)
    , (v1.z * v2.x) - (v1.x * v2.z)
    , (v1.x * v2.y) - (v1.y * v2.x)};
}

struct Ray {
  Ray(org_: &Vec, dir_: &Vec)
  {
    org = org_;
    dir = dir_;
  }

  let org: Vec;
  let dir: Vec;
}

func radiance(ray: &Ray, depth: i32) -> Color
{
}

func normalize(v: &Vec) -> Vec
{
  return v.div(v.length());
}

func createCamera() -> Ray
{
  let a = Vec{50.0, 52.0, 295.6};
  let b = Vec{0.0, -0.042612, -1.0};
  let c = normalize(ref b);

  return Ray{ref a, ref c};
}

func main() -> i32
{
  let width = 640;
  let height = 480;

  let image_heap = HeapArray{width * height * sizeof Color{}};
  let mut image = image_heap.get() as ^Color;

  let camera = createCamera();

  let cx = Vec{width as f64 * 0.5135 / height as f64,
               0.0,
               0.0};

  let tmp = cross(ref cx, ref camera.dir);
  let tmp = normalize(ref tmp);
  let cy = tmp.mul(0.5135);

  let stderr = File{2, "w"};

  for (let mut y = 0; y < height; ++y) {
    fprintf
    ( stderr.stream()
    , "Rendering %.4f%%"
    , 100.0 * y as f64 / (height - 1) as f64
    );

    srand(y * y * y);

    for (let mut x = 0; x < width; ++x) {
      let image_idx = y * width + x;
      image[image_idx] = Color{0.0, 0.0, 0.0};

      for (let mut sy = 0; sy < 2; ++sy) {
        for (let mut sx = 0; sx < 2; ++sx) {
          let dx = sx as f64 / 2.0;
          let dy = sy as f64 / 2.0;

          let tmp1
            = cx.mul(((sx as f64 + 0.5 + dx) / 2.0 + x as f64) / width as f64 - 0.5);

          let tmp2
            = cy.mul(((sy as f64 + 0.5 + dy) / 2.0 + y as f64) / height as f64 - 0.5);
          let tmp2 = tmp2.add(ref camera.dir);

          let dir = tmp1.add(ref tmp2);

          let tmp1 = dir.mul(130.0);
          let tmp1 = camera.org.add(ref tmp1);
          let tmp2 = normalize(ref dir);
          let tmp = Ray{ref tmp1, ref tmp2};
          let tmp = radiance(ref tmp, 0);

          image[image_idx] = (image[image_idx]).add(ref tmp);
        }
      }
    }

    fprintf(stderr.stream(), "\r");
  }

  fprintf(stderr.stream(), "\n");
}
