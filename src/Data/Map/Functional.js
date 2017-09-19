exports.fromPartialImpl = function (unsafePartial) {
  return function (partialFn) {
    return function (Nothing) {
      return function (Just) {
        return function (key) {
          try {
            return Just(unsafePartial(partialFn)(key));
          } catch (_) {
            return Nothing;
          }
        }
      }
    }
  }
}
