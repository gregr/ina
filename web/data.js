'use strict';

var string_interner = {'': true}; // ensure hash table mode
delete string_interner[''];
function string_interned(str) {
  string_interner[str] = true;
  var interned = Object.keys(string_interner)[0];
  delete string_interner[interned];
  return interned;
}

function array_insert(xs, ix, val) {
  var rhs = xs.length;
  var ys = []; ys.length = rhs + 1;
  for (; ix < rhs; --rhs) { ys[rhs] = xs[rhs - 1]; }
  ys[ix] = val;
  while (--ix >= 0) { ys[ix] = xs[ix]; }
  return ys;
}
function array_replace(xs, ix, val) {
  var ys = xs.slice();
  ys[ix] = val;
  return ys;
}
function array_remove(xs, ix) {
  var len = xs.length - 1;
  var ys = []; ys.length = len;
  for (var j = 0; j < ix; ++j) { ys[j] = xs[j]; }
  for (; j < len; ++j) { ys[j] = xs[j + 1]; }
  return ys;
}
function array_extend(xs, ys) {
  var ix = xs.length;
  var len = ix + ys.length;
  xs.length = len;
  for (var j = 0; ix < len; ++ix, ++j) { xs[ix] = ys[j]; }
}

function range(start, end) {
  var xs = []; xs.length = end - start;
  for (var ix = 0; ix + start < end; ++ix) {
    xs[ix] = ix + start;
  }
  return xs;
}
function sorted(xs) {
  var ys = xs.slice();
  ys.sort();
  return ys;
}
function sorted_by(cmp, xs) {
  var ys = xs.slice();
  ys.sort(cmp);
  return ys;
}
function bisect(xs, key, start, end) {
  while (start < end) {
    var mid = start + ((end - start) >> 1);
    var found = xs[mid];
    if      (key < found) { end = mid; }
    else if (key > found) { start = mid + 1; }
    else                  { return mid; }
  }
  return start;
}
function bisect_by(compare, xs, key, start, end) {
  while (start < end) {
    var mid = start + ((end - start) >> 1);
    var found = xs[mid];
    var cmp = compare(key, found);
    if      (cmp < 0) { end = mid; }
    else if (cmp > 0) { start = mid + 1; }
    else              { return mid; }
  }
  return start;
}
function unique(compare, xs) {
  var dup_count = 0, len = xs.length;
  for (var i = 0, prev = undefined; i < len; ++i) {
    var current = xs[i];
    if (compare(current, prev) === 0) { ++dup_count; }
    prev = current;
  }
  if (dup_count === 0) { return xs; }
  var ys = []; ys.length = len - dup_count;
  for (var i = 0, j = 0, prev = undefined; i < len; ++i) {
    var current = xs[i];
    if (compare(xs[i], prev) !== 0) { ys[j] = current; ++j; }
    prev = current;
  }
  return ys;
}
function unique_insert(compare, xs, value) {
  var len = xs.length;
  var ix = bisect_by(compare, xs, value, 0, len);
  if (ix < len && compare(xs[ix], value) === 0) { return xs; }
  return array_insert(xs, ix, value);
}

function compare_boolean_asc(b0, b1) { return (b0|0) - (b1|0); }
function compare_boolean_desc(b0, b1) { return (b1|0) - (b0|0); }
function compare_number_asc(n0, n1) { return n0 - n1; }
function compare_number_desc(n0, n1) { return n1 - n0; }
function compare_text_asc(t0, t1) { return t0 === t1 ? 0 : t0 < t1 ? -1 : 1; }
function compare_text_desc(t0, t1) { return t1 === t0 ? 0 : t1 < t0 ? -1 : 1; }
// TODO: compare_pair_asc, compare_set_asc, compare_poly_asc

function is_boolean(term) { return typeof term === 'boolean'; }
function is_number(term) { return typeof term === 'number'; }
function is_text(term) { return typeof term === 'string'; }
// TODO: is_nil, is_pair, is_set

// TODO: set

// TODO: read
