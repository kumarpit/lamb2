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
    if (!(pred())) {
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
            fail();
        })
    })
}

sumToEight([0, 1, 2, 3, 4], [5, 6, 7, 8, 9]);

