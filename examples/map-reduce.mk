let map = fn(f, arr) {
  if (len(arr) == 0) {
    []
  } else {
    let h = head(arr);
    cons(f(h), map(f, tail(arr)));
  }
};


let reduce = fn(f, init, arr) {
  if (len(arr) == 0) {
    init
  } else {
    let newInit = f(init, head(arr));
    reduce(f, newInit, tail(arr));
  }
};

let double = fn(x) {
  2 * x
};

let add = fn(x, y) {
  x + y
};

let mapped = map(double, [1, 2, 3, 4]);
print(mapped);

let sum = fn(arr) {
  reduce(add, 0, arr);
};

let summed = sum([1, 2, 3, 4, 5]);
print(summed);
