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
declare func fopen(filename: ^i8, mode: ^i8) -> ^FILE;

[[nomangle]]
declare func fclose(stream: ^FILE) -> i32;

[[nomangle]]
declare func rand() -> i32;

[[nomangle]]
declare func srand(seed: u32);

[[nomangle]]
declare func sqrt(x: f64) -> f64;

[[nomangle]]
declare func pow(x: f64, y: f64) -> f64;

[[nomangle]]
declare func frexp(x: f64, exp: ^i32) -> f64;

[[nomangle]]
declare func malloc(size: u64) -> ^void;

[[nomangle]]
declare func free(ptr: ^void);

[[nomangle]]
declare func exit(status: i32);

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

  File(filename: ^i8, mode: ^i8)
  {
    fp = fopen(filename, mode);
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
         ref_type_: i32)
  {
    radius = radius_;
    position = position_;
    emission = emission_;
    color = color_;
    ref_type = ref_type_;
  }

  func intersect(ray: &Ray) -> f64
  {
    let eps = 1e-6;

		let o_p = position.sub(ref ray.org);

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
  let ref_type: i32;
}

func normalize(v: &Vec) -> Vec
{
  return v.div(v.length());
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

func max(a: f64, b: f64) -> f64
{
  if (a < b)
    return b;
  else
    return a;
}

func min(a: i32, b: i32) -> i32
{
  if (a < b)
    return a;
  else
    return b;
}

func multiply(v1: &Vec, v2: &Vec) -> Vec
{
	return Vec{v1.x * v2.x, v1.y * v2.y, v1.z * v2.z};
}

func radiance(ray: &Ray, depth: i32) -> Color
{
  let max_depth = 5;
  let background_color = Color{0.0, 0.0, 0.0};

  let DIFFUSE = 0;
  let SPECULAR = 1;
  let REFRACTION = 2;

  let light_pos = Vec{50.0, 75.0, 81.6};
  let light_color = Color{256., 256., 256.};

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

  let obj = ref spheres[id];

  let tmp = ray.dir.mul(t);
  let hitpoint = ray.org.add(ref tmp);

  let tmp = hitpoint.sub(ref obj.position);
  let normal = normalize(ref tmp);

  let mut orienting_normal: Vec;
  if (dot(ref normal, ref ray.dir) < 0.0)
    orienting_normal = normal;
  else
    orienting_normal = normal.mul(-1.0);

  if (max_depth < depth)
    return Color{};

  if (obj.ref_type == DIFFUSE) {
    let ldir = light_pos.sub(ref hitpoint);

    let len = ldir.length();

    let tmp = ldir.div(len);
    let tmp = Ray{ref hitpoint, ref tmp};
    let mut t_: f64;
    let mut id_: i32;
    intersect_scene(&spheres[0], spheres_size, ref tmp, ref t_, ref id_);

    if (len <= t_) {
      let tmp1 = ldir.div(len);
      let tmp = multiply(ref light_color, ref obj.color);
      let tmp = tmp.mul(max(0.0, dot(ref orienting_normal, ref tmp1)));
      let tmp = tmp.div(len * len);
      return obj.emission.add(ref tmp);
    }
    else
      return Color{};
  }
  else if (obj.ref_type == SPECULAR) {
    let tmp = normal.mul(2.0);
    let tmp = tmp.mul(dot(ref normal, ref ray.dir));
    let tmp = tmp.sub(ref ray.dir);
    let tmp = Ray{ref hitpoint, ref tmp};
    let tmp
      = radiance(ref tmp, depth + 1);

    let tmp = multiply(ref obj.color, ref tmp);

    return obj.emission.add(ref tmp);
  }
  else if (obj.ref_type == REFRACTION) {
    let tmp = ray.dir.sub(ref normal);
    let tmp = tmp.mul(2.);
    let tmp = tmp.mul(dot(ref normal, ref ray.dir));
    let reflection_ray = Ray{ref hitpoint, ref tmp};

    let into = 0.0 < dot(ref normal, ref orienting_normal);

    let nc = 1.0;
    let nt = 1.5;
    let mut nnt: f64;
    if (into)
      nnt = nc / nt;
    else
      nnt = nt / nc;
    let ddn = dot(ref ray.dir, ref orienting_normal);
    let cos2t = 1.0 - nnt * nnt * (1.0 - ddn * ddn);

    if (cos2t < 0.0) {
      let tmp = radiance(ref reflection_ray, depth + 1);
      let tmp = multiply(ref obj.color, ref tmp);
      return obj.emission.add(ref tmp);
    }

    let tmp = ray.dir.mul(nnt);
    let tmp = tmp.sub(ref normal);

    let mut tmp1: f64;
    if (into)
      tmp1 = 1.0;
    else
      tmp1 = -1.0;

    let tmp = tmp.mul(tmp1);
    let tmp = tmp.mul(ddn * nnt + sqrt(cos2t));

    let tdir = normalize(ref tmp);

    let a = nt - nc;
    let b = nt + nc;
		let r0 = (a * a) / (b * b);
		let mut c: f64;
    if (into)
      c = 1.0 - -ddn;
    else
      c = 1.0 - dot(ref tdir, ref normal);
		let re = r0 + (1.0 - r0) * pow(c, 5.0);
		let tr = 1.0 - re;

    {
      let tmp = radiance(ref reflection_ray, depth + 1);
      let tmp = tmp.mul(re);
      let ray_tmp = Ray{ref hitpoint, ref tdir};
      let tmp1 = radiance(ref ray_tmp, depth + 1);
      let tmp = tmp.add(ref tmp1);
      let tmp = tmp.mul(tr);

      let tmp = multiply(ref obj.color, ref tmp);
      return obj.emission.add(ref tmp);
    }
  }
}

func createCamera() -> Ray
{
  let a = Vec{50.0, 52.0, 295.6};
  let b = Vec{0.0, -0.042612, -1.0};
  let c = normalize(ref b);

  return Ray{ref a, ref c};
}

struct HDRPixel {
  HDRPixel(r_: u8,
           g_: u8,
           b_: u8,
           e_: u8)
  {
    r = r_;
    g = g_;
    b = b_;
    e = e_;
  }

  HDRPixel()
  {
    r = 0 as u8;
    g = 0 as u8;
    b = 0 as u8;
    e = 0 as u8;
  }

  func get(idx: i32) -> u8
  {
    if (idx == 0)
      return r;
    if (idx == 2)
      return g;
    if (idx == 3)
      return b;
    if (idx == 4)
      return e;

    return 0 as u8;
  }

  let r: u8;
  let g: u8;
  let b: u8;
  let e: u8;
}

func get_hdr_pixel(color: &Color) -> HDRPixel
{
	let d = max(color.x, max(color.y, color.z));
	if (d <= 1e-32)
		return HDRPixel{};

	let mut e: i32;
	let m = frexp(d, &e);

	let d = m * 256.0 / d;
	return HDRPixel{
    (color.x * d) as u8,
    (color.y * d) as u8,
    (color.z * d) as u8,
    (e + 128) as u8
  };
}

struct HDRPixelArray {
  HDRPixelArray()
  {
  }

  func push_back(pix: &HDRPixel)
  {
  }

  func at(idx: u64) -> HDRPixel
  {
    return p[idx];
  }

  let p: ^HDRPixel;
  let size: u64;
}

func save_as_hdr(filename: ^i8, image: ^Color, width: i32, height: i32)
{
  let file = File{filename, "wb"};
  let fp = file.stream();

  if (fp) {
    // let ret = 0x0a as u8;
	  // fprintf(fp, "#?RADIANCE%c", ret);
	  // fprintf(fp, "# Made with 100%% pure HDR Shop%c", ret);
	  // fprintf(fp, "FORMAT=32-bit_rle_rgbe%c", ret);
	  // fprintf(fp, "EXPOSURE=1.0000000000000%c%c", ret, ret);

    // fprintf(fp, "-Y %d +X %d%c", height, width, ret);
	  // for (let mut i = height - 1; 0 <= i; --i) {
	  //   std::vector<HDRPixel> line;
	  //   for (let mut j = 0; j < width; ++j) {
	  //     HDRPixel p = get_hdr_pixel(image[j + i * width]);
	  //     line.push_back(p);
	  //   }

	  //   fprintf(fp, "%c%c", 0x02, 0x02);
	  //   fprintf(fp, "%c%c", (width >> 8) & 0xFF, width & 0xFF);

	  //   for (let mut i = 0; i < 4; ++i) {
	  //     for (let mut cursor = 0; cursor < width;) {
	  //       let cursor_move = min(127, width - cursor);
	  //       fprintf(fp, "%c", cursor_move);
	  //       for (int j = cursor;  j < cursor + cursor_move; j ++)
	  //         fprintf(fp, "%c", line[j].get(i));
	  //       cursor += cursor_move;
	  //     }
	  //   }
	  // }
  }
  else {
    let stderr = File{2, "w"};
    fprintf(stderr.stream(), "Failed to open file!\n");
    exit(1);
  }
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

          let tmp0 = dir.mul(130.0);
          let tmp1 = camera.org.add(ref tmp0);
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

  for (let mut i = 0; i < 10; ++i) {
    printf("%f\n", image[i].length_squared());
  }

  save_as_hdr("image.hdr", image, width, height);
}
