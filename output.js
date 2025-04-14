///////////////////////////////////////////////////////////////////////////
//
// A simple implementation of the `ambiguous` operator for Javascript
//
//////////////////////////////////////////////////////////////////////////////

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
/////////////////////////////////////////////////////////////////////////////
//
// End of implementation
//
/////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////
//
// Simple example where we want to find all pairs of numbers from the given lists that sum to 7 
//
//////////////////////////////////////////////////////////////////////////////////////////////////

function sumToEight(arr1, arr2) {
    return amb(arr1, (x) => {
        return amb(arr2, (y) => {
            assert(x + y == 8);

            return [x, y];
        })
    })
}

console.log(sumToEight([0, 1, 2, 3, 4], [5, 6, 7, 8, 9]));


//////////////////////////////////////////////////////////////////////////////////////////////////
//
// Solving the graph coloring problem with 4 colors 
// Problem taken from https://www.metalevel.at/prolog/optimization
//
//////////////////////////////////////////////////////////////////////////////////////////////////

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

console.log(solveMapColoring())


//////////////////////////////////////////////////////////////////////////////////////////////////
//
// Solving the 8 Queens problem
//
//////////////////////////////////////////////////////////////////////////////////////////////////

let choices = [...Array(8)].map((_, j) => j + 1);

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

console.log(solve8Queens());
