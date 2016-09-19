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
