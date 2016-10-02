'use strict';
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

function compare_symbol_asc(s0, s1) { return s0.index - s1.index; }
function compare_symbol_desc(s0, s1) { return s1.index - s0.index; }
function compare_tuple_asc(t0, t1) {
  var ks0 = Object.keys(t0.assoc); var l0 = ks0.length;
  var ks1 = Object.keys(t1.assoc); var l1 = ks1.length;
  ks0.sort(compare_number_asc); ks1.sort(compare_number_asc);
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
    case 'string': return compare_text_asc(x0, x1);
    case 'object':
      t0 = x0.tag; t1 = x1.tag;
      // coincidentally, this line also works for now
      if (t0 !== t1) { return t0 < t1 ? -1 : 1; }
      if (t0 === symbol_tag) { return compare_symbol_asc(x0, x1); }
      if (t0 === tuple_tag) { return compare_tuple_asc(x0, x1); }
      if (t0 === set_tag) { return compare_set_asc(x0, x1); }
  }
}

function is_tuple(term)   { return typeof term === 'object'; }

var symbol_tag = tag_count++;
var tuple_tag = tag_count++;

function is_symbol(term) {
  return (typeof term === 'object') && term.tag === symbol_tag;
}
function is_tuple(term) {
  return (typeof term === 'object') && term.tag === tuple_tag;
}

var symbol_table = {};
var symbol_name = [];
function symbol_new(name) {
  var index = symbol_name.length;
  symbol_name.push(name);
  return {'tag': symbol_tag, 'name': name, 'index': index};
}
function symbol(name) {
  name = string_interned(name);
  var sym = symbol_table[name];
  if (sym === undefined) {
    sym = symbol_new(name);
    symbol_table[name] = sym;
  }
  return sym;
}

function tuple(assoc) {
  return {'tag': tuple_tag, 'assoc': assoc};
}
var tuple_empty = tuple([]);
function tuple_from(obj, arr) {
  var assoc = arr.slice();
  for (var name in obj) {
    var key = -symbol(name).index - 1;
    assoc[key] = obj[name];
  }
  return tuple(assoc);
}
function tuple_get(tup, key) {
  if (!is_number(key)) { key = -key.index - 1; } return tuple.assoc[key];
}
function tuple_put(tup, key, value) {
  var assoc, old = tup.assoc;
  if (is_number(key)) {  // assumes key <= old.length
    if (key < old.length) { assoc = array_replace(old, key, value); }
    else { assoc = array_insert(old, key, value); }
    for (var k in old) { if (k < 0) { assoc[k] = old[k]; } }
  } else {
    assoc = []; assoc.length = old.length;
    for (var k in old) { assoc[k] = old[k]; }
    assoc[-key.index - 1] = value;
  }
  return tuple(assoc);
}
function tuple_remove(tup, key) {
  var assoc, old = tup.assoc;
  if (is_number(key)) {
    if (key >= old.length) { return tup; }
    assoc = array_remove(old, key);
    for (var k in old) { if (k < 0) { assoc[k] = old[k]; } }
  } else {
    key = -key.index - 1;
    if (key in old) {
      assoc = []; assoc.length = old.length;
      for (var k in old) { if (k !== key) { assoc[k] = old[k]; } }
    } else { return tup; }
  }
  return tuple(assoc);
}
// TODO: fix
//function tuple_meet(t0, t1) {
  //var a0 = t0.assoc, k0 = t0.keys;
  //var a1 = t1.assoc, k1 = t1.keys;
  //var keys = unique(compare_number_asc
                   //,sorted_by(compare_number_asc, k0.concat(k1)));
  //var assoc = {};
  //for (var k in a0) {
    //assoc[k] = a0[k];
  //}
  //for (var k in t1) {
    //// TODO: check for existing key and unify
    //assoc[k] = a1[k];
  //}
  //return tuple(keys, assoc);
//}

function set_from_list(xs) {
  return set(unique(compare_poly_asc, sorted_by(compare_poly_asc, xs)));
}
function set_has(xs, value) {
  var els = xs.elements;
  var len = els.length;
  var ix = bisect_by(compare_poly_asc, els, value, 0, len);
  return (ix < len) && compare_poly_asc(els[ix], value) === 0;
}
function set_insert(xs, value) {
  return set(unique_insert(compare_poly_asc, xs.elements, value));
}

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

function stream(source, start) { return {'source': source, 'start': start}; }

var re_slash_text = /\\[^u]|\\u[0-9a-fA-F]{4}/g;
function decode_text_slash(es) {
  switch (es[1]) {
    case '0': return '\0';
    case 'b': return '\b';
    case 't': return '\t';
    case 'n': return '\n';
    case 'r': return '\r';
    case 'v': return '\v';
    case 'f': return '\f';
    case 'u': return eval('"' + es + '"');
    default: return es[1];
  }
}
function decode_text(src, start, end) {
  return src.slice(start, end).replace(re_slash_text, decode_text_slash);
}
function decode_int(src, start, end) { return eval(src.slice(start, end)); }
function decode_float(src, start, end) { return eval(src.slice(start, end)); }

function cc_digit_decimal(cc) { return cc >= 48 && cc <= 57; }
function cc_digit_hexadecimal(cc) {
  return cc_digit_decimal(cc) ||
    (cc >= 65 && cc <= 70) || (cc >= 97 && cc <= 102);
  }
function cc_digit_binary(cc) { return cc === 48 || cc === 49; }
function cc_alphaUpper(cc) { return cc >= 65 && cc <= 90; }
function cc_alphaLower(cc) { return cc >= 97 && cc <= 122; }
function cc_alpha(cc) { return cc_alphaLower(cc) || cc_alphaUpper(cc); }
function cc_hspace(cc) {
  return cc === 32 || cc === 9 || cc === 160 || cc >= 0x2000 && cc <= 0x200A;
}
function cc_vspace(cc) {
  return cc >= 10 && cc <= 13 || cc === 133 || cc === 0x2028 || cc === 0x02029;
}
function cc_space(cc) { return cc_hspace(cc) || cc_vspace(cc); }

function token_boundary(src, i) {
  if (i >= src.length) { return i; }
  var ch = src[i];
  switch (ch) {
    case '(': case '[': case '{':
    case ')': case ']': case '}':
    case '`': case '"': case "'":
      return i;
    default:
      if (cc_space(src.charCodeAt(i))) { return i; }
      else { return undefined; }
  }
}
function token_space(src, i) {
  for (var len = src.length; i < len; ++i) {
    if (!cc_space(src.charCodeAt(i))) { break; }
  }
  return i;
}
function token_text(delim, src, i) {
  for (var len = src.length; i < len; ++i) {
    if (src[i] === delim) { return i; }
    else if (src[i] === '\\') { ++i; }
  }
  return undefined;
}

// ~!@$%^&*-=+|\<>/?
//case ',': case '.': case ';': case ':':
function token_symbol(src, i) {
  for (var len = src.length; i < len; ++i) {
    if (src[i] === '\\') { ++i; }
    else {
      var j = token_boundary(src, i);
      if (j !== undefined) { return j; }
    }
  }
  return i;
}
function token_float(src, i) {
  var len = src.length, needs_digits = true;
  if (i < len) {
    var ch = src[i]; if (ch === '-') { ++i; }
    for (; i < len; ++i) {
      if (!cc_digit_decimal(src.charCodeAt(i))) { break; }
      needs_digits = false;
    }
    if (i === len) { return needs_digits ? undefined : i; }
    ch = src[i];
    if (ch === '.') {
      ++i;
      for (; i < len; ++i) {
        if (!cc_digit_decimal(src.charCodeAt(i))) { break; }
        needs_digits = false;
      }
    }
    if (i === len) { return needs_digits ? undefined : i; }
    switch (src[i]) {
      case 'e': case 'E':
        ++i; needs_digits = true; if (i === len) { return undefined; }
        ch = src[i]; if (ch === '-') { ++i; }
        for (; i < len; ++i) {
          if (!cc_digit_decimal(src.charCodeAt(i))) { break; }
          needs_digits = false;
        }
    }
  }
  if (needs_digits) { return undefined; }
  return token_boundary(src, i);
}
function token_int(src, i) {
  var start = i, len = src.length;
  if (i < len) {
    var ch = src[i];
    if (ch === '-') {
      ++start; ++i; if (i === len) { return undefined; } ch = src[i];
    }
    if (ch === '0') {
      ++i; if (i === len) { return i; } ch = src[i];
      switch (ch) {
        case 'x':
          start += 2; ++i;
          for (; i < len; ++i) {
            if (!cc_digit_hexadecimal(src.charCodeAt(i))) { break; }
          }
          break;
        case 'b':
          start += 2; ++i;
          for (; i < len; ++i) {
            if (!cc_digit_binary(src.charCodeAt(i))) { break; }
          }
          break;
      }
    }
  }
  for (; i < len; ++i) {
    if (!cc_digit_decimal(src.charCodeAt(i))) { break; }
  }
  if (i === start) { return undefined; }
  return token_boundary(src, i);
}

function result_boundary(result, ss, i) {
  i = token_boundary(ss.source, i);
  if (i === undefined) { ss.start = i; return result; }
  else { return undefined; }
}

function read_special(ss) {
  var src = ss.source, i = ss.start;
  var len = src.length;
  if (i >= len) { return undefined; }
  var result, ch = src[i];
  switch (ch) {
    case 't': { return result_boundary(true, ss, ++i); }
    case 'f': { return result_boundary(false, ss, ++i); }
    // TODO: comments and other specials
    default: return undefined;
  }
}

function array_to_list(xs) {
  var result = nil;
  for (var i = xs.length - 1; i >= 0; --i) { result = pair(xs[i], result); }
  return result;
}
var symbol_bracket_tuple = symbol('[]');
function construct_tuple(xs) {
  return pair(symbol_bracket_tuple, array_to_list(xs));
}
var symbol_bracket_set = symbol('{}');
function construct_set(xs) {
  return pair(symbol_bracket_set, array_to_list(xs));
}

var bracket_struct_attr = {'(': [')', array_to_list]
                          ,'[': [']', construct_tuple]
                          ,'{': ['}', construct_set]};
function read_structure(bracket, ss) {
  var src = ss.source, struct_attr = bracket_struct_attr[bracket];
  var len = src.length, delim = struct_attr[0], construct = struct_attr[1];
  var elements = [];
  while (ss.start < len) {
    var element = read(ss);
    if (element !== undefined) { elements.push(element); }
    else { break; }
  }
  var i = ss.start;
  if (i < len && src[i] === delim) {
    ss.start = ++i; return construct(elements);
  }
  return undefined;
}

function read(ss) {
  var src = ss.source, i = ss.start, len = src.length;
  i = token_space(src, i);
  ss.start = i;
  if (i < len) {
    var ch = src[i];
    switch (ch) {
      case '(': case '[': case '{':
        ss.start = ++i; return read_structure(ch, ss);
      case ')': case ']': case '}': return undefined;
      case '`': case '"': case "'":
        var j = token_text(ch, src, ++i);
        if (j === undefined) { return undefined; }
        var text = decode_text(src, i, j);
        ss.start = j + 1;
        if (ch === '`') { return symbol(text); }
        else { return text; }
      case '#': ss.start = ++i; return read_special(ss);
      default:
        var j = token_float(src, i);
        var k = token_int(src, i);
        var l = token_symbol(src, i);
        if (j !== undefined && (k === undefined || j > k) &&
            (l === undefined || j >= l)) {
          ss.start = j; return decode_float(src, i, j);
        } else if (k !== undefined && (l === undefined || k >= l)) {
          ss.start = k; return decode_int(src, i, k);
        } else if (l !== undefined) {
          ss.start = l; return symbol(decode_text(src, i, l));
        }
    }
  }
  return undefined;
}
