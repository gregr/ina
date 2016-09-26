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
        value = values[BTREE_BLOCK_SIZE_HALF - 1];
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
        value = values[BTREE_BLOCK_SIZE_HALF];
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

function btree_put(bt, key, value) {
  var original = bt;
  var path = [];
  while (true) {
    var keys = bt.keys;
    var klen = keys.length;
    var ix = bisect(keys, key, 0, klen);
    var children = bt.children;
    if (ix < klen && keys[ix] === key) {
      var values = bt.values;
      if (values[ix] === value) { return original; }
      bt = btree_branch(keys, array_replace(values, ix, value), children);
      return btree_path_update(path, bt);
    }
    btree_step(path, bt, ix);
    if (!children) { return btree_path_insert(path, key, value, null, null); }
    bt = children[ix];
  }
}

function btree_merge_siblings(pbt, left, il, right, ir) {
  var keys = array_merge_mid(left.keys, pbt.keys[il], right.keys);
  var values = array_merge_mid(left.values, pbt.values[il], right.values);
  var children = left.children;
  if (children) { children = array_merge(left.children, right.children); }
  var bt = btree_branch(keys, values, children);
  keys = array_remove(pbt.keys, il);
  values = array_remove(pbt.values, il);
  children = array_remove(pbt.children, ir);
  children[il] = bt;
  if (keys.length === 0) return bt;
  return btree_branch(keys, values, children);
}

function btree_leftmost(path_out, bt) {
  var children = bt.children;
  while (children) {
    btree_step(path_out, bt, 0);
    bt = children[0];
    children = bt.children;
  }
  return bt;
}
function btree_rightmost(path_out, bt) {
  var children = bt.children;
  while (children) {
    var ix = children.length - 1;
    btree_step(path_out, bt, ix);
    bt = children[ix];
    children = bt.children;
  }
  return bt;
}

function btree_rotate_leaf(path, bt, ix, ix_child, leaf_path, leaf, leaf_ix) {
  var keys = array_replace(bt.keys, ix, leaf.keys[leaf_ix]);
  var values = array_replace(bt.values, ix, leaf.values[leaf_ix]);
  btree_step(path, btree_branch(keys, values, bt.children), ix_child);
  array_extend(path, leaf_path);
  return btree_path_remove(path, leaf, leaf_ix);
}
function btree_rotate_sibling(path, bt, ix, cix, pix, pbt, p_ix, sib, sib_ix, sib_cix, sib_pix) {
  var children = bt.children, schildren = sib.children;
  if (children) {
    children = array_insert(children, cix, schildren[sib_cix]);
    schildren = array_remove(schildren, sib_cix);
  }
  bt = btree_branch(array_insert(bt.keys, ix, pbt.keys[p_ix])
                   ,array_insert(bt.values, ix, pbt.values[p_ix])
                   ,children);
  var pkeys = array_replace(pbt.keys, p_ix, sib.keys[sib_ix]);
  var skeys = array_remove(sib.keys, sib_ix);
  var pvalues = array_replace(pbt.values, p_ix, sib.values[sib_ix]);
  var svalues = array_remove(sib.values, sib_ix);
  sib = btree_branch(skeys, svalues, schildren);
  var siblings = array_replace(pbt.children, sib_pix, sib);
  siblings[pix] = bt;
  return btree_path_update(path, btree_branch(pkeys, pvalues, siblings));
}

function btree_path_remove(path, bt, ix) {
  var keys = array_remove(bt.keys, ix);
  var values = array_remove(bt.values, ix);
  var children = bt.children;
  if (children) { children = array_remove(children, ix); }
  bt = btree_branch(keys, values, children);
  return btree_path_balance(path, bt);
}
function btree_path_balance(path, bt) {
  for (var ip = path.length - 1; ip >= 0; --ip) {
    var klen = bt.keys.length;
    if (klen >= BTREE_BLOCK_SIZE_HALF || path.length === 0) {
      path.length = ip + 1;
      return btree_path_update(path, bt);
    } else {
      var pseg = path[ip]; path.length = ip;
      var pix = pseg.index;
      var pbt = pseg.tree;
      var siblings = pbt.children;
      var siblen = siblings.length;
      if (pix > 0 && siblings[pix - 1].keys.length > BTREE_BLOCK_SIZE_HALF) {
        var six = pix - 1;
        var sib = siblings[six];
        var sklen = sib.keys.length;
        return btree_rotate_sibling(
            path, bt, 0, 0, pix, pbt, six, sib, sklen - 1, sklen, six);
      }
      if (pix < siblen - 1) {
        sib = siblings[pix + 1];
        if (sib.keys.length > BTREE_BLOCK_SIZE_HALF) {
          return btree_rotate_sibling(
              path, bt, klen, klen + 1, pix, pbt, pix, sib, 0, 0, pix + 1);
        } else { bt = btree_merge_siblings(pbt, bt, pix, pbt.children[pix + 1], pix + 1); }
      } else { bt = btree_merge_siblings(pbt, pbt.children[pix - 1], pix - 1, bt, pix); }
    }
  }
  return bt;
}

function btree_remove(bt, key) {
  var original = bt;
  var path = [];
  while (true) {
    var children = bt.children;
    var keys = bt.keys;
    var klen = keys.length;
    var ix = bisect(keys, key, 0, klen);
    if (ix < klen && keys[ix] === key) {
      if (children) {
        var lpath = [];
        var leaf = btree_rightmost(lpath, children[ix]);
        var lklen = leaf.keys.length;
        if (lklen > BTREE_BLOCK_SIZE_HALF) {
          return btree_rotate_leaf(path, bt, ix, ix, lpath, leaf, lklen - 1);
        } else {
          lpath = [];
          leaf = btree_leftmost(lpath, children[ix + 1]);
          return btree_rotate_leaf(path, bt, ix, ix + 1, lpath, leaf, 0);
        }
      } else { return btree_path_remove(path, bt, ix); }
    }
    if (!children) { return original; }
    btree_step(path, bt, ix);
    bt = children[ix];
  }
}

function btree_join(join, bt0, bt1) {
  var ks0 = btree_keys_to_list(bt0);
  var ks1 = btree_keys_to_list(bt1);
  var result = btree_empty;
  var k0, l0 = ks0.length, i0 = 0, k1, l1 = ks1.length, i1 = 0;
  if (i0 < l0) {
    k0 = ks0[i0];
    if (i1 < l1) {
      k1 = ks1[i1];
      while (true) {
        if (k0 < k1) {
          result = btree_put(result, k0, btree_get(bt0, k0));
          ++i0; if (i0 === l0) break; k0 = ks0[i0];
        } else if (k1 < k0) {
          result = btree_put(result, k1, btree_get(bt1, k1));
          ++i1; if (i1 === l1) break; k1 = ks1[i1];
        } else {
          var value = join(btree_get(bt0, k0), btree_get(bt1, k1));
          if (value !== undefined) { result = btree_put(result, k0, value); }
          ++i0; ++i1; if (i0 === l0 || i1 === l1) break;
          k0 = ks0[i0]; k1 = ks1[i1];
        }
      }
    }
  }
  for (; i0 < l0; ++i0) {
    k0 = ks0[i0];
    result = btree_put(result, k0, btree_get(bt0, k0));
  }
  for (; i1 < l1; ++i1) {
    k1 = ks1[i1];
    result = btree_put(result, k1, btree_get(bt1, k1));
  }
  return result;
}

function btree_meet(meet, bt0, bt1) {
  var ks0 = btree_keys_to_list(bt0);
  var ks1 = btree_keys_to_list(bt1);
  var result = btree_empty;
  var k0, l0 = ks0.length, i0 = 0, k1, l1 = ks1.length, i1 = 0;
  if (i0 < l0) {
    k0 = ks0[i0];
    if (i1 < l1) {
      k1 = ks1[i1];
      while (true) {
        if      (k0 < k1) { ++i0; if (i0 === l0) break; k0 = ks0[i0]; }
        else if (k1 < k0) { ++i1; if (i1 === l1) break; k1 = ks1[i1]; }
        else {
          var value = meet(btree_get(bt0, k0), btree_get(bt1, k1));
          if (value !== undefined) { result = btree_put(result, k0, value); }
          ++i0; ++i1; if (i0 === l0 || i1 === l1) break;
          k0 = ks0[i0]; k1 = ks1[i1];
        }
      }
    }
  }
  return result;
}

function btree_keys_to_list(bt) {
  var path = [], result = [];
  while (true) {
    var children = bt.children;
    var keys = bt.keys;
    if (children) {
      for (var j = children.length - 1; j; --j) {
        path.push({'key': keys[j - 1], 'rhs': children[j]});
      }
      bt = children[j];
    } else {
      for (var i = 0, len = keys.length; i < len; ++i) {
        result.push(keys[i]);
      }
      if (path.length === 0) { return result; }
      var seg = path.pop();
      result.push(seg.key);
      bt = seg.rhs;
    }
  }
}

function btree_from_list(xs) {
  var bt = btree_empty;
  for (var ix = 0, len = xs.length; ix < len; ++ix) {
    var key = xs[ix];
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

function compare_boolean_asc(b0, b1) { return +b0 - +b1; }
function compare_boolean_desc(b0, b1) { return +b1 - +b0; }
function compare_number_asc(n0, n1) { return n0 - n1; }
function compare_number_desc(n0, n1) { return n1 - n0; }
function compare_symbol_asc(s0, s1) { return s0.index - s1.index; }
function compare_symbol_desc(s0, s1) { return s1.index - s0.index; }
function compare_tuple_asc(t0, t1) {
  var ks0 = t0.keys; var l0 = ks0.length;
  var ks1 = t1.keys; var l1 = ks1.length;
  var i = 0;
  for (; i < l0; ++i) {
    if (i >= l1) { return 1; }
    var k0 = ks0[i];
    var k1 = ks1[i];
    if (k0 !== k1) { return k0 < k1 ? -1 : 1; }
    var cmp = compare_poly_asc(t0.assoc[k0], t1.assoc[k0]);
    if (cmp !== 0) { return cmp; }
  }
  return i < l1 ? -1 : 0;
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
  // coincidentally, this line works for now
  if (t0 !== t1) { return t0 < t1 ? -1 : 1; }
  switch (t0) {
    case 'boolean': return compare_boolean_asc(x0, x1);
    case 'number': return compare_number_asc(x0, x1);
    case 'string': return x0.localeCompare(x1);
    case 'object':
      t0 = x0.tag; t1 = x1.tag;
      // coincidentally, this line also works for now
      if (t0 !== t1) { return t0 < t1 ? -1 : 1; }
      if (t0 === symbol_tag) { return compare_symbol_asc(x0, x1); }
      if (t0 === tuple_tag) { return compare_tuple_asc(x0, x1); }
      if (t0 === set_tag) { return compare_set_asc(x0, x1); }
  }
}

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

var tag_count = 0;
var boolean_tag = tag_count++;
var number_tag = tag_count++;
var text_tag = tag_count++;
var symbol_tag = tag_count++;
var tuple_tag = tag_count++;
var set_tag = tag_count++;

function is_symbol(term) {
  return (typeof term === 'object') && term.tag === symbol_tag;
}
function is_tuple(term) {
  return (typeof term === 'object') && term.tag === tuple_tag;
}
function is_set(term) {
  return (typeof term === 'object') && term.tag === set_tag;
}

var symbol_table = {};
var symbol_name = [];
function symbol_new(name) {
  var index = symbol_name.length;
  symbol_name.push(name);
  return {'tag': symbol_tag, 'index': index};
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

function tuple(keys, assoc) {
  return {'tag': tuple_tag, 'keys': keys, 'assoc': assoc};
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

function set(elements) { return {'tag': set_tag, 'elements': elements}; }
var set_empty = set([]);

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
