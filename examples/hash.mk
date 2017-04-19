let people = [{"name": "Alice", "age": 24}, {"name": "Anna", "age": 28}];

print(people[0]["name"]);
print(people[1]["age"]);
print(people[0]["age"] + people[1]["age"]);

let getName = fn(person) { person["name"]; };
print(getName(people[0]));
print(getName(people[1]));


let map = fn(f, arr) {
  if (len(arr) == 0) {
    []
  } else {
    let h = head(arr);
    cons(f(h), map(f, tail(arr)));
  }
};
print(map(getName, people));
