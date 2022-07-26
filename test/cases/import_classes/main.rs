import "./sample.rs";

func main() -> i32
{
  let n: i32;

  let sam = sample{&n, 58};

  sam.set();

  return n;
}
