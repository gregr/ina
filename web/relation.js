'use strict'
/*

* relation definition
** extensional
*** defined by data
** intensional
*** defined by computation
*** potentially infinite modes
*** decidable modes may be precomputed
*** any mode may be memoized

* relation properties
** decidable
*** finite number of answers
*** may require tabling/memoization to terminate when calculating fixed points
** semi-deterministic
*** 0 or 1 answer

* negation
** allowed for any decidable goal

* constraints
** should be idempotent, unlike CHR
** lattice-based (a bit ad-hoc at the moment)
*** types (possibly other predicates?)
*** intervals
*** finite domains
*** (in)equalities
*** computations (e.g. arithmetic: a + b = c)
*** negated versions of the above

* stratification and analyses?

* higher level sugar
** multi-site definitions/assignments
*** definitions may be distributed across several source locations
** monotonic/merging assignment
*** merge operations must be commutative, associative, idempotent (CRDTs)
*** generalizes relation definition?
** perturbance-based assignment
*** optionally deferred
*** optionally pinned
**** pinned values are not changeable by future perturbances until unpinned
** conditionals
*** optionally multi-arm
*** optionally event/signal-based
** arithmetic (in)equalities
*** could be generalized to other structures/computations
**** e.g. (append a b) == (append c d)
** aggregation
** higher-order functional orchestration and communication

* terms
** atoms
*** boolean, exact, inexact (later), symbol
*** text is pseudo-atomic?
** compounds
*** tuple
**** unordered (record)
**** ordered (array)
*** set
**** monomorphic vs. polymorphic sets
****   boolean (ideal: two bits)
****   number  (ideal: intervals)
****   text    (ideal: some kind of trie/index?)
****   symbol  (ideal: also intervals?)
****   tuple ordered
****   tuple unordered
****   set (nested, so mono vs. poly again)
****   polymorphic
*** extensional relations are sets of unordered tuples

*/

function range(start, end) {
  var xs = []; xs.length = end - start;
  for (var ix = 0; ix + start < end; ++ix) {
    xs[ix] = ix + start;
  }
  return xs;
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
function array_insert_remove_right(xs, start, end, ix, val) {
  var lhs = start, rhs = end - 1;
  var ys = []; ys.length = end - start;
  var j = 0;
  for (; lhs < ix; ++j, ++lhs) { ys[j] = xs[lhs]; }
  ys[j] = val;
  for (++j; lhs < rhs; ++j, ++lhs) { ys[j] = xs[lhs]; }
  return ys;
}
function array_insert_remove_left(xs, start, end, ix, val) {
  var lhs = start, rhs = end - 1;
  var ys = []; ys.length = end - start;
  var j = 0;
  for (; ++lhs < ix; ++j) { ys[j] = xs[lhs]; }
  ys[j] = val;
  for (++j; lhs <= rhs; ++j, ++lhs) { ys[j] = xs[lhs]; }
  return ys;
}
function array_merge(xs, ys) { return xs.concat(ys); }
function array_merge_mid(xs, mid, ys) { return xs.concat([mid]).concat(ys); }
function array_extend(xs, ys) {
  var ix = xs.length;
  var len = ix + ys.length;
  xs.length = len;
  for (var j = 0; ix < len; ++ix, ++j) { xs[ix] = ys[j]; }
}

var BTREE_BLOCK_SIZE_FULL = 8;
var BTREE_BLOCK_SIZE_HALF = BTREE_BLOCK_SIZE_FULL/2;
function btree_branch(keys, values, children) {
  return {'keys': keys, 'values': values, 'children': children};
}
function btree_leaf(keys, values) { return btree_branch(keys, values, null); }
function btree_step(path, bt, index) { path.push({'index': index, 'tree': bt}); }
var btree_empty = btree_leaf([], []);

function btree_get(bt, key) {
  while (true) {
    var keys = bt.keys;
    var klen = keys.length;
    var ix = bisect(keys, key, 0, klen);
    if (ix < klen && keys[ix] === key) { return bt.values[ix]; }
    var children = bt.children;
    if (!children) { return undefined; }
    bt = bt.children[ix];
  }
}

function btree_path_update(path, bt) {
  for (var ip = path.length - 1; ip >= 0; --ip) {
    var pseg = path[ip];
    var ix = pseg.index;
    var old = pseg.tree;
    var children = old.children.slice();
    children[ix] = bt;
    bt = btree_branch(old.keys, old.values, children);
  }
  return bt;
}

function btree_put(bt, key, value) {
  var original = bt;
  var path = [];
  while (true) {
    var keys = bt.keys;
    var klen = keys.length;
    var ix = bisect(keys, key, 0, klen);
    var children = bt.children;
    if (ix < klen && keys[ix] === key) {
      values = bt.values;
      if (values[ix] === value) { return original; }
      bt = btree_branch(keys, array_replace(values, ix, value), children);
      return btree_path_update(path, bt);
    }
    btree_step(path, bt, ix);
    if (!children) { return btree_path_insert(path, key, value, null, null); }
    bt = children[ix];
  }
}

function btree_path_insert(path, key, value, left, right) {
  for (var ip = path.length - 1; ip >= 0; --ip) {
    var pseg = path[ip];
    var ix = pseg.index;
    var old = pseg.tree;
    var keys = old.keys;
    var values = old.values;
    var children = old.children;
    var klen = keys.length;
    if (klen < BTREE_BLOCK_SIZE_FULL) {
      keys = array_insert(keys, ix, key);
      values = array_insert(values, ix, value);
      if (children) {
        children = array_insert(children, ix, left);
        children[ix + 1] = right;
      }
      path.length = ip;
      return btree_path_update(path, btree_branch(keys, values, children));
    } else {
      var kl, kr, vl, vr, chl = null, chr = null;
      if (ix < BTREE_BLOCK_SIZE_HALF) {
        kr = keys.slice(BTREE_BLOCK_SIZE_HALF, BTREE_BLOCK_SIZE_FULL);
        kl = array_insert_remove_right(keys, 0, BTREE_BLOCK_SIZE_HALF, ix, key);
        key = keys[BTREE_BLOCK_SIZE_HALF - 1];
        vr = values.slice(BTREE_BLOCK_SIZE_HALF, BTREE_BLOCK_SIZE_FULL);
        vl = array_insert_remove_right(values, 0, BTREE_BLOCK_SIZE_HALF, ix, value);
        value = keys[BTREE_BLOCK_SIZE_HALF - 1];
        if (children) {
          chr = children.slice(BTREE_BLOCK_SIZE_HALF, BTREE_BLOCK_SIZE_FULL + 1);
          chl = array_insert_remove_right(children, 0, BTREE_BLOCK_SIZE_HALF + 1, ix, left);
          chl[ix + 1] = right;
        }
      } else if (ix > BTREE_BLOCK_SIZE_HALF) {
        kl = keys.slice(0, BTREE_BLOCK_SIZE_HALF);
        kr = array_insert_remove_left(keys, BTREE_BLOCK_SIZE_HALF, BTREE_BLOCK_SIZE_FULL, ix, key);
        key = keys[BTREE_BLOCK_SIZE_HALF];
        vl = values.slice(0, BTREE_BLOCK_SIZE_HALF);
        vr = array_insert_remove_left(values, BTREE_BLOCK_SIZE_HALF, BTREE_BLOCK_SIZE_FULL, ix, value);
        value = keys[BTREE_BLOCK_SIZE_HALF];
        if (children) {
          chl = children.slice(0, BTREE_BLOCK_SIZE_HALF + 1);
          chr = array_insert_remove_left(children, BTREE_BLOCK_SIZE_HALF, BTREE_BLOCK_SIZE_FULL + 1, ix, left);
          chr[ix - BTREE_BLOCK_SIZE_HALF] = right;
        }
      } else {
        kl = keys.slice(0, BTREE_BLOCK_SIZE_HALF);
        kr = keys.slice(BTREE_BLOCK_SIZE_HALF, BTREE_BLOCK_SIZE_FULL);
        vl = values.slice(0, BTREE_BLOCK_SIZE_HALF);
        vr = values.slice(BTREE_BLOCK_SIZE_HALF, BTREE_BLOCK_SIZE_FULL);
        if (children) {
          chl = children.slice(0, BTREE_BLOCK_SIZE_HALF + 1);
          chr = children.slice(BTREE_BLOCK_SIZE_HALF, BTREE_BLOCK_SIZE_FULL + 1);
          chl[BTREE_BLOCK_SIZE_HALF] = left;
          chr[0] = right;
        }
      }
      left = btree_branch(kl, vl, chl);
      right = btree_branch(kr, vr, chr);
    }
  }
  return btree_branch([key], [value], [left, right]);
}

function btree_remove(bt, key) {
}

//function btree_join(bt0, bt1) {
//}
//function btree_meet(bt0, bt1) {
//}
//function btree_to_list(bt) {
//}

function btree_from_list(xs) {
  var bt = btree_empty;
  for (var ix = 0, len = xs.length; ix < len; ++ix) {
    key = xs[ix];
    bt = btree_put(bt, key, key);
  }
  return bt;
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
function compare_asc(n0, n1) { return n0 - n1; }
function compare_desc(n0, n1) { return n1 - n0; }

var string_interner = {'': true}; // ensure hash table mode
delete string_interner[''];
function string_interned(str) {
  string_interner[str] = true;
  var interned = Object.keys(symbols_interned)[0];
  delete string_interner[interned];
  return interned;
}

function is_boolean(term) { return typeof term === 'boolean'; }
function is_number(term)  { return typeof term === 'number'; }
function is_text(term)    { return typeof term === 'string'; }
function is_tuple(term)   { return typeof term === 'object'; }

function tagged(tag, payload) { return [tag, payload]; }
function tagged_tag(tg)       { return tg[0]; }
function tagged_payload(tg)   { return tg[1]; }

var tag_count = 0;
var boolean_tag = tag_count++;
var number_tag = tag_count++;
var text_tag = tag_count++;
var symbol_tag = tag_count++;
var set_tag = tag_count++;
var tuple_ordered_tag = tag_count++;
var tuple_unordered_tag = tag_count++;

var symbol_table = {};
var symbol_name = [];
function symbol_new(name) {
  var index = symbol_name.length;
  symbol_name.push(name);
  return tagged(symbol_tag, index);
}
function symbol(name) {
  name = string_interned(name);
  var sym = symbol_table[name];
  if (sym === undefined) {
    sym = symbol_new(name);
    symbol_table[str] = sym;
  }
  return sym;
}

function tuple_from_obj(obj) {
  var tuple = {};
  for (var name in obj) {
    tuple[symbol(name)[1]] = obj[name];
  }
  return tuple;
}
function tuple_insert(tup, key, value) {
  var tnew = {};
  for (var k in tup) {
    tnew[k] = tup[k];
  }
  // TODO: check for existing key and unify
  tnew[key] = value;
  return tnew;
}
function tuple_remove(tup, key) {
  var tnew = {};
  for (var k in tup) {
    if (k !== key) tnew[k] = tup[k];
  }
  return tnew;
}
function tuple_meet(t0, t1) {
  var tnew = {};
  for (var k in t0) {
    tnew[k] = t0[k];
  }
  for (var k in t1) {
    // TODO: check for existing key and unify
    tnew[k] = t1[k];
  }
  return tnew;
}

function is_symbol(term) { return tagged_tag(term) === symbol_tag; }
function is_set(term)    { return tagged_tag(term) === set_tag; }

var set_boolean_empty = 0;
function set_boolean_has(set, bool) { return (set & (1 << bool)) > 0; }
function set_boolean_add(set, bool) { return set | (1 << bool); }
function set_boolean_join(s0, s1)   { return s0 | s1; }
function set_boolean_meet(s0, s1)   { return s0 & s1; }

// https://www.npmjs.com/package/v8-natives
// node --allow-natives-syntax
// %HasFastProperties(obj)
// %GetOptimizationStatus(fn)
// %OptimizeFunctionOnNextCall(fn)
