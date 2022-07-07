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

  func sub(b: &Vec) -> Vec
  {
    return Vec{x - b.x, y - b.y, z - b.z};
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

func dot(v1: &Vec, v2: &Vec) -> f64
{
	return v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
}

struct Sphere {
  Sphere(radius_: f64,
         // FIXME
         position_: Vec, // Copy
         emission_: Color, // Copy
         color_: Color, // Copy
         type_arg: i32)
  {
    radius = radius_;
    position = position_;
    emission = emission_;
    color = color_;
    type_ = type_arg;
  }

  func intersect(ray: &Ray) -> f64
  {
    let eps = 1e-6;

		let o_p = position.sub(ref ray.org); // Error

		let b = dot(ref o_p, ref ray.dir);
    let det = b * b - dot(ref o_p, ref o_p) + radius * radius;

		if (0.0 <= det) {
			let sqrt_det = sqrt(det);
			let t1 = b - sqrt_det;
      let t2 = b + sqrt_det;

			if (t1 > eps)
        return t1;
			else if (t2 > eps)
        return t2;
		}

		return 0.0;
	}

  let radius: f64;
  let position: Vec;
  let emission: Color;
  let color: Color;
  let type_: i32;
}

func intersect_scene(spheres: ^Sphere,
                     spheres_size: i32,
                     ray: &Ray,
                     t: mut &f64,
                     id: mut &i32) -> bool
{
  let inf = 1e20;

  t = inf;
  id = -1;

  for (let mut i = 0; i < spheres_size; ++i) {
    let d = spheres[i].intersect(ref ray);

    if (0.0 < d && d < t) {
      t = d;
      id = i;
    }
  }

  return t < inf;
}

func radiance(ray: &Ray, depth: i32) -> Color
{
  let background_color = Color{0.0, 0.0, 0.0};

  let DIFFUSE = 0;
  let SPECULAR = 1;
  let REFRACTION = 2;

  let spheres = [
	  Sphere{ 1e5, Vec{1e5 + 1., 40.8, 81.6},   Color{}, Color{0.75, 0.25, 0.25},    DIFFUSE},
	  Sphere{ 1e5, Vec{-1e5 + 99., 40.8, 81.6}, Color{}, Color{0.25, 0.25, 0.75},    DIFFUSE},
	  Sphere{ 1e5, Vec{50., 40.8, 1e5},         Color{}, Color{0.75, 0.75, 0.75},    DIFFUSE},
	  Sphere{ 1e5, Vec{50., 40.8, -1e5 + 170.}, Color{}, Color{},                    DIFFUSE},
	  Sphere{ 1e5, Vec{50., 1e5, 81.6},         Color{}, Color{0.75, 0.75, 0.75},    DIFFUSE},
	  Sphere{ 1e5, Vec{50., -1e5 + 81.6, 81.6}, Color{}, Color{0.75, 0.75, 0.75},    DIFFUSE},
	  Sphere{16.5, Vec{27., 16.5, 47.},         Color{}, Color{1., 1., 1.}.mul(.99), SPECULAR},
	  Sphere{16.5, Vec{73., 16.5, 78.},         Color{}, Color{1., 1., 1.}.mul(.99), REFRACTION}
  ];

  let spheres_size = (sizeof spheres / sizeof spheres[0]) as i32;

  let mut t: f64;
  let mut id: i32;
  if (!intersect_scene(&spheres[0], spheres_size, ref ray, ref t, ref id))
    return background_color;

  printf("%f %d\n", t, id);
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

          image[image_idx] = image[image_idx].add(ref tmp);
        }
      }
    }

    fprintf(stderr.stream(), "\r");
  }

  fprintf(stderr.stream(), "\n");
}
