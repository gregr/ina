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

var tag_count = 0;
var nil_tag = tag_count++;
var pair_tag = tag_count++;
var set_tag = tag_count++;

var nil = {'tag': nil_tag};
function pair(hd, tl) { return {'tag': pair_tag, 'head': hd, 'tail': tl}; }
function set(elements) { return {'tag': set_tag, 'elements': elements}; }
var set_empty = set([]);

function is_boolean(term) { return typeof term === 'boolean'; }
function is_number(term) { return typeof term === 'number'; }
function is_text(term) { return typeof term === 'string'; }
function is_nil(term) { term === nil; }
function is_pair(term) {
  return (typeof term === 'object') && term.tag === pair_tag;
}
function is_set(term) {
  return (typeof term === 'object') && term.tag === set_tag;
}

function compare_boolean_asc(b0, b1) { return (b0|0) - (b1|0); }
function compare_boolean_desc(b0, b1) { return (b1|0) - (b0|0); }
function compare_number_asc(n0, n1) { return n0 - n1; }
function compare_number_desc(n0, n1) { return n1 - n0; }
function compare_text_asc(t0, t1) { return t0 === t1 ? 0 : t0 < t1 ? -1 : 1; }
function compare_text_desc(t0, t1) { return t1 === t0 ? 0 : t1 < t0 ? -1 : 1; }
function compare_pair_asc(p0, p1) {
  var c0 = compare_poly_asc(p0.head, p1.head);
  return c0 !== 0 ? c0 : compare_poly_asc(p0.tail, p1.tail);
}
function compare_set_asc(s0, s1) {
  var xs0 = s0.elements; l0 = xs0.length;
  var xs1 = s1.elements; l1 = xs1.length;
  var i = 0;
  for (; i < l0; ++i) {
    if (i >= l1) { return 1; }
    var cmp = compare_poly_asc(xs0[i], xs1[i]);
    if (cmp !== 0) { return cmp; }
  }
  return i < l1 ? -1 : 0;
}
function compare_poly_asc(x0, x1) {
  if (x0 === x1) { return 0; }
  var t0 = typeof x0;
  var t1 = typeof x1;
  // boolean < number < string < nil(unique object) < pair < set
  if (t0 !== t1) {
    return t0 === 'object' ? 1 : t1 === 'object' ? -1 : t0 < t1 ? -1 : 1;
  }
  switch (t0) {
    case 'boolean': return compare_boolean_asc(x0, x1);
    case 'number': return compare_number_asc(x0, x1);
    case 'string': return compare_text_asc(x0, x1);
    case 'object':
      t0 = x0.tag; t1 = x1.tag;
      if (t0 !== t1) { return t0 < t1 ? -1 : 1; }
      if (t0 === pair_tag) { return compare_pair_asc(x0, x1); }
      if (t0 === set_tag) { return compare_set_asc(x0, x1); }
  }
}
function set_from_array(xs) {
  return set(unique(compare_poly_asc, sorted_by(compare_poly_asc, xs)));
}
function set_index_of(xs, value) {
  var els = xs.elements;
  var len = els.length;
  var ix = bisect_by(compare_poly_asc, els, value, 0, len);
  (ix < len) && compare_poly_asc(els[ix], value) === 0 ? ix : -1;
}
function set_has(xs, value) { return set_index_of(xs, value) !== -1; }
function set_add(xs, value) {
  return set(unique_insert(compare_poly_asc, xs.elements, value));
}
function set_remove(xs, value) {
  var ix = set_index_of(xs, value);
  if (ix !== -1) {
    if (len > 1) { return set(array_remove(els, ix)); }
    return set_empty;
  }
  return xs;
}

// TODO: read
