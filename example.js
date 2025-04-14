//////////////////////////////////////////////////////////////////////////////////////////////////
//
// Simple example where we want to find all pairs of numbers from the given lists that sum to 7 
//
//////////////////////////////////////////////////////////////////////////////////////////////////

function sumToEight(arr1, arr2) {
    amb x = arr1;
    amb y = arr2;

    if (x + y == 8) {
        console.log([x, y]);
    }
    // fail();
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

console.log(solveMapColoring())


//////////////////////////////////////////////////////////////////////////////////////////////////
//
// Solving the 8 Queens problem
//
//////////////////////////////////////////////////////////////////////////////////////////////////

let choices = [...Array(8)].map((_, j) => j + 1);

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

console.log(solve8Queens());
