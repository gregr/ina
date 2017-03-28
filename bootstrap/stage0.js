'use strict';

// TODO: read(/annotate), write

function is_nil(datum) { return datum === null; }
function is_closure(datum) { return typeof datum === 'function'; }
function is_boolean(datum) { return typeof datum === 'boolean'; }
function is_text(datum) { return typeof datum === 'string'; }
function is_number(datum) { return typeof datum === 'number'; }
function is_int32(datum) {
  return (typeof datum === 'number') && (datum === (datum | 0));
}
function is_pair(datum) {
  return (typeof datum === 'object') && (datum !== null);
}

function env_extend(env, datum) { return [datum, env]; }
function env_ref(env, idx) {
  for (; idx > 0; --idx) { env = env[1]; }
  return env[0];
}
function env_find(env, found) {
  var index = 0;
  for (; env !== null; ++index) {
    if (found(env[0])) { return index; }
    env = env[1];
  }
  return undefined;
}
function env_find_name(env, name) {
  var index = env_find(env, function(b){return (name === b[0]);});
  if (index === undefined) { throw ['unbound variable', name]; }
  return index;
}
function env_lookup(env, name) {
  var index = env_find_name(env, name);
  return env_ref(env, index)[1];
}

// TODO: Expect annotated syntax.
function syntax_list_of_length(len, xs, err) {
  var result = [];
  for (; len > 0; --len) {
    if (!is_pair(xs)) { throw err; }
    result.push(xs[0]);
    xs = xs[1];
  }
  if (!is_nil(xs)) { throw err; }
  return result;
}
function syntax_list(xs, err) {
  var result = [];
  while (is_pair(xs)) {
    result.push(xs[0]);
    xs = xs[1];
  }
  if (!is_nil(xs)) { throw err; }
  return result;
}
function array_to_list(xs) {
  var i = xs.length - 1;
  var result = null;
  for (; i >= 0; --i) { result = [xs[i], result]; }
  return result;
}
function array_map(f, xs) {
  var i = xs.length - 1;
  var result = [];
  result.length = xs.length;
  for (; i >= 0; --i) { result[i] = f(xs[i]); }
  return result;
}

function evaluate(stx) {
  // TODO: It may be better to pass these registers around in an explicit
  // context rather than capturing them in closures.  It may also be more
  // efficient to represent continuations as tagged data structures.
  var result, env = null, ks = [function(){return null;}];

  // TODO: Might as well inline this everywhere.
  function unwind() { return ks.pop(); }

  function denote_literal(datum) {
    return function() { result = datum; return unwind(); };
  }
  function denote_var(senv, name) {
    var index = env_find_name(senv, name);
    if (env_ref(senv, index)[1] !== false) { throw ['invalid syntax', name]; }
    return function() { result = env_ref(env, index); return unwind(); };
  }

  function build_app(senv, dp, as) {
    if (is_nil(as)) { return dp; }
    var da = denote(senv, as[0]);
    var dapp = function(){
      var kenv = env;
      ks.push(function() {
        var proc = result;
        if (!is_closure(proc)) { throw ['cannot apply non-procedure', proc]; }
        ks.push(function() { return proc(result); });
        env = kenv;
        return da;
      });
      return dp;
    };
    return build_app(senv, dapp, as[1]);
  }
  function denote_app(senv, stx) {
    var err = ['invalid application', stx];
    if (!is_pair(stx)) { throw err; }
    syntax_list(stx[1], err);
    return build_app(senv, denote(senv, stx[0]), stx[1]);
  }

  function denote(senv, stx) {
    if (is_nil(stx) || is_boolean(stx) || is_number(stx)) {
      return denote_literal(stx);
    } else if (is_text(stx)) {
      return denote_var(senv, stx);
    } else if (is_pair(stx)) {
      if (is_text(stx[0])) {
        var special = env_lookup(senv, stx[0]);
        if (typeof special === 'function') { return special(senv, stx[1]); }
      }
      return denote_app(senv, stx);
    } else { throw ['unknown syntax', stx]; }
  }

  function build_lambda(senv, params, body) {
    if (is_nil(params)) { return denote(senv, body); }
    senv = env_extend(senv, [params[0], false]);
    var dl = build_lambda(senv, params[1], body);
    return function() {
      var clo_env = env;
      result = function(arg) {
        env = env_extend(clo_env, arg);
        return dl;
      };
      return unwind();
    };
  }
  function denote_lambda(senv, stx) {
    var err = ['invalid lambda', stx];
    stx = syntax_list_of_length(2, stx, err);
    var params = stx[0], body = stx[1];
    if (!is_pair(params)) { throw err; }
    array_map(function(p){ if (!is_text(p)) { throw err; } },
              syntax_list(params, err));
    return build_lambda(senv, params, body);
  }
  function denote_if(senv, stx) {
    stx = syntax_list_of_length(3, stx, ['invalid if', stx]);
    var dc = denote(senv, stx[0]);
    var dt = denote(senv, stx[1]);
    var df = denote(senv, stx[2]);
    return function() {
      var kenv = env;
      ks.push(function(){
        env = kenv;
        if (result === false) { return df; } else { return dt; }
      });
      return dc;
    };
  }
  function denote_quote(senv, stx) {
    stx = syntax_list_of_length(1, stx, ['invalid quote', stx]);
    return denote_literal(stx[0]);
  }
  function denote_qq(senv, stx) {
    // TODO:
  }
  function denote_let(senv, stx) {
    var err = ['invalid let', stx];
    stx = syntax_list_of_length(2, stx, err);
    var bindings = array_map(function(b) {
      return syntax_list_of_length(2, b, err);
    }, syntax_list(stx[0], err));
    var params = array_map(function(b) {
      if (!is_text(b[0])) { throw err; } return b[0];
    }, bindings);
    var args = array_map(function(b) { return b[1]; }, bindings);
    var body = stx[1];
    var dp = build_lambda(senv, array_to_list(params), body);
    return build_app(senv, dp, array_to_list(args));
  }

  var operatives = [
    ['let', denote_let],
    ['quasiquote', denote_qq],
    ['quote', denote_quote],
    ['if', denote_if],
    ['lambda', denote_lambda]];

  function native_procedure_huh(datum) {
    result = is_closure(datum); return unwind();
  }
  function native_boolean_huh(datum) {
    result = is_boolean(datum); return unwind();
  }
  function native_pair_huh(datum) { result = is_pair(datum); return unwind(); }
  function native_nil_huh(datum) { result = is_nil(datum); return unwind(); }
  function native_text_huh(datum) { result = is_text(datum); return unwind(); }
  function native_number_huh(datum) { result = is_number(datum); return unwind(); }
  function native_int32_huh(datum) { result = is_int32(datum); return unwind(); }
  function make_eq(name, is_x) {
    return function(x0) {
      if (!is_x(x0)) { throw ["invalid argument to '"+name+"'", x0]; }
      result = function(x1) {
        if (!is_x(x1)) { throw ["invalid argument to '"+name+"'", x1]; }
        result = (x0 === x1);
        return unwind();
      };
      return unwind();
    };
  }
  var native_text_eq = make_eq('text=?', is_text);
  var native_number_eq = make_eq('number=?', is_number);
  var native_int32_eq = make_eq('int32=?', is_int32);
  function make_lt(name, is_x) {
    return function(x0) {
      if (!is_x(x0)) { throw ["invalid argument to '"+name+"'", x0]; }
      result = function(x1) {
        if (!is_x(x1)) { throw ["invalid argument to '"+name+"'", x1]; }
        result = (x0 < x1);
        return unwind();
      };
      return unwind();
    };
  }
  var native_text_lt = make_lt('text<?', is_text);
  var native_number_lt = make_lt('number<?', is_number);
  var native_int32_lt = make_lt('int32<?', is_int32);
  function native_pair(h) {
    result = function(t) { result = [h, t]; return unwind(); };
    return unwind();
  }
  function native_pair_head(p) {
    if (!is_pair(p)) { throw ['cannot take head of non-pair', p]; }
    result = p[0];
    return unwind();
  }
  function native_pair_tail(p) {
    if (!is_pair(p)) { throw ['cannot take tail of non-pair', p]; }
    result = p[1];
    return unwind();
  }

  function native_text_concat(t0) {
    if (!is_text(t0)) { throw ['cannot text-concat non-text', t0]; }
    result = function(t1) {
      if (!is_text(t1)) { throw ['cannot text-concat non-text', t1]; }
      result = t0.concat(t1);
      return unwind();
    };
    return unwind();
  }
  function native_text_to_list(txt) {
    if (!is_text(t0)) { throw ['cannot text->list non-text', txt]; }
    var i = txt.length - 1;
    var answer = null;
    for (; i >= 0; --i) { answer = [txt.charAt(i), answer]; }
    result = answer;
    return unwind();
  }
  // NOTE: codePointAt and String.fromCodePoint aren't supported in ES5
  function native_text_to_codes(txt) {
    if (!is_text(t0)) { throw ['cannot text->codes non-text', txt]; }
    var i = txt.length - 1;
    var answer = null;
    for (; i >= 0; --i) { answer = [txt.charCodeAt(i), answer]; }
    result = answer;
    return unwind();
  }
  function native_text_from_codes(cs) {
    var err = ['invalid text codes', cs];
    cs = syntax_list(cs, err);
    var i = cs.length - 1;
    for (; i >= 0; --i) { if (!is_int32(cs[i])) { throw err; } }
    result = String.fromCharCode.apply(this, cs);
    return unwind();
  }

  function native_nadd(n0) {
    if (!is_number(n0)) { throw ['cannot add non-number', n0]; }
    result = function(n1) {
      if (!is_number(n1)) { throw ['cannot add non-number', n1]; }
      result = n0 + n1; return unwind();
    };
    return unwind();
  }
  function native_nsub(n0) {
    if (!is_number(n0)) { throw ['cannot subtract non-number', n0]; }
    result = function(n1) {
      if (!is_number(n1)) { throw ['cannot subtract non-number', n1]; }
      result = n0 - n1; return unwind();
    };
    return unwind();
  }
  function native_nmul(n0) {
    if (!is_number(n0)) { throw ['cannot multiply non-number', n0]; }
    result = function(n1) {
      if (!is_number(n1)) { throw ['cannot multiply non-number', n1]; }
      result = n0 * n1; return unwind();
    };
    return unwind();
  }
  function native_ndiv(n0) {
    if (!is_number(n0)) { throw ['cannot divide non-number', n0]; }
    result = function(n1) {
      if (!is_number(n1)) { throw ['cannot divide non-number', n1]; }
      result = n0 / n1; return unwind();
    };
    return unwind();
  }
  function native_nmod(n0) {
    if (!is_number(n0)) { throw ['cannot modulo non-number', n0]; }
    result = function(n1) {
      if (!is_number(n1)) { throw ['cannot modulo non-number', n1]; }
      result = n0 % n1; return unwind();
    };
    return unwind();
  }

  // TODO: More text and/or numerical operations?
  var applicatives = [
    ['procedure?', native_procedure_huh],
    ['pair?', native_pair_huh],
    ['nil?', native_nil_huh],
    ['boolean?', native_boolean_huh],
    ['text?', native_text_huh],
    ['number?', native_number_huh],
    ['int32?', native_int32_huh],
    ['text=?', native_text_eq],
    ['text<?', native_text_lt],
    ['number=?', native_number_eq],
    ['number<?', native_number_lt],
    ['int32=?', native_int32_eq],
    ['int32<?', native_int32_lt],
    ['pair', native_pair],
    ['head', native_pair_head],
    ['tail', native_pair_tail],
    ['text-concat', native_text_concat],
    ['text->list', native_text_to_list],
    ['text->codes', native_text_to_codes],
    ['text<-codes', native_text_from_codes],
    ['+', native_nadd],
    ['-', native_nsub],
    ['*', native_nmul],
    ['/', native_ndiv],
    ['%', native_nmod]];

  var initial_senv =
    array_to_list(
      array_map(function(binding){return [binding[0],false];},
                applicatives).concat(operatives));

  var initial_env =
    array_to_list(
      array_map(function(binding){return binding[1];}, applicatives).concat(
        array_map(function(binding){return binding[1];}, operatives)));

  env = initial_env;
  var k = denote(initial_senv, stx);
  while (k !== null) { k = k(); }
  return result;
}

tests = [
  evaluate(null),
  evaluate(true),
  evaluate(4),
  evaluate([['lambda', [['x', ['y', null]],
             ['x', null]]],
            [5, [6, null]]]),
  evaluate([['lambda', [['x', ['y', null]],
             [['pair', ['y', ['x', null]]], null]]],
            [5, [6, null]]]),
  evaluate(['if', [['head', [['quote', [[true, false], null]], null]],
             [['quote', ['yes', null]],
             [['quote', ['no', null]], null]]]]),
  evaluate(['if', [['tail', [['quote', [[true, false], null]], null]],
             [['quote', ['yes', null]],
             [['quote', ['no', null]], null]]]]),
  evaluate(['let', [[['x', [8, null]], null],
            ['x', null]]]),
  evaluate(['let', [[['x', [9, null]], null],
            [['let', [[['x', [20, null]], null],
              ['x', null]]], null]]]),
  evaluate(['let', [[['x', [9, null]], null],
            [['let', [[['y', [20, null]], null],
              ['x', null]]], null]]])
  ]
