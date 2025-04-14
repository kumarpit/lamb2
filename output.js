// --- amb helpers ---
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
// --- end helpers ---

//////////////////////////////////////////////////////////////////////////////////////////////////
//
// Simple example where we want to find all pairs of numbers from the given lists that sum to 7 
//
//////////////////////////////////////////////////////////////////////////////////////////////////

function sumToEight(arr1, arr2) {
    amb(arr1, (x) => {
        amb(arr2, (y) => {
            if (x + y == 8) {
                console.log([x, y]);
            }
            // fail();
        })
    })
}

sumToEight([0, 1, 2, 3, 4], [5, 6, 7, 8, 9]);


//////////////////////////////////////////////////////////////////////////////////////////////////
//
// Solving the graph coloring problem with 4 colors 
// Problem taken from https://www.metalevel.at/prolog/optimization
//
//////////////////////////////////////////////////////////////////////////////////////////////////

colors = ["red", "green", "blue", "yellow"];
let adjacencyList = {
    "a": ["b", "c", "d", "f"],
    "b": ["a", "c", "d"],
    "c": ["a", "b", "d", "e"],
    "d": ["a", "b", "c", "e", "f"],
    "e": ["c", "d", "f"],
    "f": ["a", "d", "e"]
}

// TODO: would be nice to have a macro that takes a list of variables and exapnds to something
// like this
amb(colors, (a) => {
    amb(colors, (b) => {
        amb(colors, (c) => {
            amb(colors, (d) => {
                amb(colors, (e) => {
                    amb(colors, (f) => {

                        const assignment = { a, b, c, d, e, f };
                        for (const [node, neighbors] of Object.entries(adjacencyList)) {
                            for (const neighbor of neighbors) {
                                assert(assignment[node] !== assignment[neighbor]);
                            }
                        }
                        console.log(assignment)
                    })
                })
            })
        })
    })
})
