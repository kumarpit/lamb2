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

amb([false, true], (x) => {
    if (x) console.log("amb succeeded");
    else {
        console.log("amb failed");
        fail();
    }

    amb([false, true], (x) => {
        if (x) console.log("amb succeeded");
        else {
            console.log("amb failed");
            fail();
        }
    })
})
