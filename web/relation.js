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

var BTREE_BLOCK_SIZE_FULL = 8;
var BTREE_BLOCK_SIZE_HALF = BTREE_BLOCK_SIZE_FULL/2;
var btree_empty = {'depth': 0, 'data': null};

function btree_get(bt, key) {
  var depth = bt.depth;
  var data = bt.data;
  if (!depth) return undefined;
  do {
    var keys = data.keys;
    var ix = bisect(keys, key, 0, keys.length);
    if (keys[ix] === key) { return data.values[ix]; }
    if (!--depth) return undefined;
    data = data.children[ix];
  } while (true);
}

function btree_update(bt, path, data) {
  for (var ip = path.length - 1; ip >= 0; --ip) {
    var pseg = path[ip];
    var ix = pseg.index;
    var data_old = pseg.data;
    var children = data_old.children.slice();
    children[ix] = data;
    data = {'keys': data_old.keys
           ,'values': data_old.values
           ,'children': children};
  }
  return {'depth': bt.depth, 'data': data};
}

function btree_put(bt, key, value) {
  var path = [];
  var depth = bt.depth;
  var data = bt.data;
  if (!depth) {
    return {'depth': 1, 'data': {'keys': [key], 'values': [value]}};
  }
  do {
    var keys = data.keys;
    var ix = bisect(keys, key, 0, keys.length);
    if (keys[ix] === key) {
      values = data.values;
      if (values[ix] === value) return bt;
      values = values.slice();
      values[ix] = value;
      data = {'keys': keys, 'values': values, 'children': data.children};
      return btree_update(bt, path, data);
    }
    if (!--depth) {
      var klen = keys.length;
      if (klen < BTREE_BLOCK_SIZE_FULL) {
        keys = keys.slice();     ++keys.length;
        values = values.slice(); ++values.length;
        for (var j = klen; ix < j; --j) {
          keys[j] = keys[j - 1];
          values[j] = values[j - 1];
        }
        keys[ix] = key;
        values[ix] = value;
        return btree_update(bt, path, {'keys': keys, 'values': values});
      } else {
        var kl = keys.slice(0, BTREE_BLOCK_SIZE_HALF);
        var kr = keys.slice(BTREE_BLOCK_SIZE_HALF, BTREE_BLOCK_SIZE_FULL);
        var vl = values.slice(0, BTREE_BLOCK_SIZE_HALF);
        var vr = values.slice(BTREE_BLOCK_SIZE_HALF, BTREE_BLOCK_SIZE_FULL);
        var ckey, cvalue;
        if (ix < BTREE_BLOCK_SIZE_HALF) {
          var j = BTREE_BLOCK_SIZE_HALF - 1;
          ckey = kl[j];
          cvalue = vl[j];
          for (; ix < j; --j) {
            kl[j] = kl[j - 1];
            vl[j] = vl[j - 1];
          }
          kl[ix] = key;
          vl[ix] = value;
        } else if (ix > BTREE_BLOCK_SIZE_HALF) {
          ix -= BTREE_BLOCK_SIZE_HALF + 1;
          var j = 0;
          ckey = kr[j];
          cvalue = vr[j];
          for (; j < ix; ++j) {
            kr[j] = kr[j + 1];
            vr[j] = vr[j + 1];
          }
          kr[ix] = key;
          vr[ix] = value;
        } else {
          ckey = key;
          cvalue = value;
        }
        var left = {'keys': kl, 'values': vl};
        var right = {'keys': kr, 'values': vr};
        return btree_update_put(bt, path, ckey, cvalue, left, right);
      }
    }
    path.push({'index': ix, 'data': data});
    data = data.children[ix];
  } while (true);
}

function btree_update_put(bt, path, key, value, left, right) {
  for (var ip = path.length - 1; ip >= 0; --ip) {
    var pseg = path[ip];
    var ix = pseg.index;
    var data_old = pseg.data;
    var keys = data_old.keys;
    var values = data_old.values;
    var children = data_old.children;
    var klen = keys.length;

    if (klen < BTREE_BLOCK_SIZE_FULL) {
      keys = keys.slice();         ++keys.length;
      values = values.slice();     ++values.length;
      children = children.slice(); ++children.length;
      for (var j = klen; ix < j; --j) {
        keys[j] = keys[j - 1];
        values[j] = values[j - 1];
        children[j + 1] = children[j];
      }
      keys[ix] = key;
      values[ix] = value;
      children[ix] = left;
      children[ix + 1] = right;
      data = {'keys': keys, 'values': values, 'children': children};
      path.length = ip;
      return btree_update(bt, path, data);
    } else {
      var kl = keys.slice(0, BTREE_BLOCK_SIZE_HALF);
      var kr = keys.slice(BTREE_BLOCK_SIZE_HALF, BTREE_BLOCK_SIZE_FULL);
      var vl = values.slice(0, BTREE_BLOCK_SIZE_HALF);
      var vr = values.slice(BTREE_BLOCK_SIZE_HALF, BTREE_BLOCK_SIZE_FULL);
      var chl = children.slice(0, BTREE_BLOCK_SIZE_HALF + 1);
      var chr = children.slice(BTREE_BLOCK_SIZE_HALF, BTREE_BLOCK_SIZE_FULL + 1);
      var ckey, cvalue;
      if (ix < BTREE_BLOCK_SIZE_HALF) {
        var j = BTREE_BLOCK_SIZE_HALF - 1;
        ckey = kl[j];
        cvalue = vl[j];
        for (; ix < j; --j) {
          kl[j] = kl[j - 1];
          vl[j] = vl[j - 1];
          chl[j + 1] = chl[j];
        }
        kl[ix] = key;
        vl[ix] = value;
        chl[ix] = left;
        chl[ix + 1] = right;
      } else if (ix > BTREE_BLOCK_SIZE_HALF) {
        ix -= BTREE_BLOCK_SIZE_HALF + 1;
        var j = 0;
        ckey = kr[j];
        cvalue = vr[j];
        for (; j < ix; ++j) {
          kr[j] = kr[j + 1];
          vr[j] = vr[j + 1];
          chr[j] = chr[j + 1];
        }
        kr[ix] = key;
        vr[ix] = value;
        chr[ix] = left;
        chr[ix + 1] = right;
      } else {
        ckey = key;
        cvalue = value;
        chl[BTREE_BLOCK_SIZE_HALF] = left;
        chr[0] = right;
      }
      key = ckey;
      value = cvalue;
      left = {'keys': kl, 'values': vl, 'children': cl};
      right = {'keys': kr, 'values': vr, 'children': cr};
    }
  }
  return {'depth': bt.depth + 1
         ,'data': {'keys': [key]
                  ,'values': [value]
                  ,'children': [left, right]}};
}

function btree_remove(bt, key) {
}

//function btree_join(bt0, bt1) {
//}
//function btree_meet(bt0, bt1) {
//}
//function btree_to_list(bt) {
//}


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
