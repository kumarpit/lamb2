# lamb(iguous)2

<img width="322" alt="image" src="https://github.com/user-attachments/assets/1e9fa0bb-fd97-446b-a6b7-4b299387e714" />
<br/>
<br/>

This is another approach to implementing the `amb` operator in JS -- using macros! Now, JS does not have macros, but there are projects that try to fill this hole (eg. [Sweet.js](https://www.sweetjs.org)). I decided to just use a simple Racket preprocessing script.

## Usage

You can now use `amb` as another way to declare variables. Note that when using `amb` to declare a variable, the value it is assigned must be an iterable. Let's see a few examples. Say you want to pick a number from the array `[0, 1, 2, 3, 4]` and one from `[5, 6, 7, 8, 9]` such that they add up to `8`. Here is how you can do so using `amb`:
```javascript
function sumToEight(arr1, arr2) {
    amb x = arr1;
    amb y = arr2;
    assert(x + y == 8);

    return [x, y];
}

console.log(sumToEight([0, 1, 2, 3, 4], [5, 6, 7, 8, 9]));
```

After the pre-processing step, this exapnds to the following javascript code:
```javascript
function sumToEight(arr1, arr2) {
    return amb(arr1, (x) => {
        return amb(arr2, (y) => {
            assert(x + y == 8);

            return [x, y];
        })
    })
}

console.log(sumToEight([0, 1, 2, 3, 4], [5, 6, 7, 8, 9]));
```

Notice the call to the `amb` and `assert` functions - they ceom from the following definitions that are added as part of the pre-processing step:
```javascript
const AmbError = Symbol('AmbError');

function amb(iterable, closure) {
    for (const n of iterable) {
        try {
            return closure(n);
        } catch (e) {
            if (e == AmbError) {
                continue;
            }
        }
    }
    throw AmbError;
}

function fail() {
    throw AmbError;
}

function assert(pred) {
    if (!pred) {
        fail();
    }
}
```

For a more elaborate example, consider the following program that solves the map coloring problem described [here](https://www.metalevel.at/prolog/optimization).
```javascript
const colors = ["red", "green", "blue", "yellow"];
const adjacencyList = {
    "a": ["b", "c", "d", "f"],
    "b": ["a", "c", "d"],
    "c": ["a", "b", "d", "e"],
    "d": ["a", "b", "c", "e", "f"],
    "e": ["c", "d", "f"],
    "f": ["a", "d", "e"]
}

function solveMapColoring() {
    amb a = colors;
    amb b = colors;
    amb c = colors;
    amb d = colors;
    amb e = colors;
    amb f = colors;

    const assignment = { a, b, c, d, e, f };
    for (const [node, neighbors] of Object.entries(adjacencyList)) {
        for (const neighbor of neighbors) {
            assert(assignment[node] !== assignment[neighbor]);
        }
    }

    return assignment;
}

solveMapColoring();
```

This gets expanded to:
```javascript
const colors = ["red", "green", "blue", "yellow"];
const adjacencyList = {
    "a": ["b", "c", "d", "f"],
    "b": ["a", "c", "d"],
    "c": ["a", "b", "d", "e"],
    "d": ["a", "b", "c", "e", "f"],
    "e": ["c", "d", "f"],
    "f": ["a", "d", "e"]
}

function solveMapColoring() {
    return amb(colors, (a) => {
        return amb(colors, (b) => {
            return amb(colors, (c) => {
                return amb(colors, (d) => {
                    return amb(colors, (e) => {
                        return amb(colors, (f) => {

                            const assignment = { a, b, c, d, e, f };
                            for (const [node, neighbors] of Object.entries(adjacencyList)) {
                                for (const neighbor of neighbors) {
                                    assert(assignment[node] !== assignment[neighbor]);
                                }
                            }

                            return assignment;
                        })
                    })
                })
            })
        })
    })
}
```

Finally, here is a solution to the 8-Queens problem!

```javascript
const N = 8;
let choices = [...Array(N)].map((_, j) => j);

function solve8Queens() {
    amb col0 = choices;
    amb col1 = choices;
    amb col2 = choices;
    amb col3 = choices;
    amb col4 = choices;
    amb col5 = choices;
    amb col6 = choices;
    amb col7 = choices;

    const positions = { col0, col1, col2, col3, col4, col5, col6, col7 };
    assert(
        ![...Array(N)].some((_, i) =>
            [...Array(i)].some((_, j) => // only check columns < i
                positions["col" + i] === positions["col" + j] || // same row?
                Math.abs(i - j) === Math.abs(positions["col" + i] - positions["col" + j]) // same diagonal?
            )
        )
    )

    return positions;
}
```

And here is what this gets expanded to:
```javascript
const N = 8;
let choices = [...Array(N)].map((_, j) => j);

function solve8Queens() {
    return amb(choices, (col0) => {
        return amb(choices, (col1) => {
            return amb(choices, (col2) => {
                return amb(choices, (col3) => {
                    return amb(choices, (col4) => {
                        return amb(choices, (col5) => {
                            return amb(choices, (col6) => {
                                return amb(choices, (col7) => {

                                    const positions = { col0, col1, col2, col3, col4, col5, col6, col7 };
                                    assert(
                                        ![...Array(N)].some((_, i) =>
                                            [...Array(i)].some((_, j) => // only check columns < i
                                                positions["col" + i] === positions["col" + j] || // same row?
                                                Math.abs(i - j) === Math.abs(positions["col" + i] - positions["col" + j]) // same diagonal?
                                            )
                                        )
                                    )

                                    return positions;
                                })
                            })
                        })
                    })
                })
            })
        })
    })
}
```
