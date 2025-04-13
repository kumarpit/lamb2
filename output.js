// --- amb helpers ---
const AmbError = Symbol('AmbError');

function amb(iterable, closure) {
  let it = iterable[Symbol.iterator]();
  for (const n of it) {
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

amb("hello", (x) => {

    console.log(x);
    fail();

})
