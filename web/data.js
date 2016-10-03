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

function dotted_list_from_array(xs, tail) {
  var result = tail;
  for (var i = xs.length - 1; i >= 0; --i) { result = pair(xs[i], result); }
  return result;
}
function list_from_array(xs) { return dotted_list_from_array(xs, nil); }

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

var re_slash_text = /\\[^u]|\\u[0-9a-fA-F]{4}/g;
function decode_text_slash(es) {
  switch (es[1]) {
    case '0': return '\0';
    case 'b': return '\b';
    case 't': return '\t';
    case 'n': return '\n';
    case 'v': return '\v';
    case 'f': return '\f';
    case 'r': return '\r';
    case 'u': return eval('"' + es + '"');
    default: return es[1];
  }
}
function decode_text(src, start, end) {
  return src.slice(start, end).replace(re_slash_text, decode_text_slash);
}
// TODO: drop leading zeroes to avoid octal encoding surprises
function decode_number(src, start, end) { return eval(src.slice(start, end)); }

function cc_digit_decimal(cc) { return cc >= 48 && cc <= 57; }
function cc_digit_hexadecimal(cc) {
  return cc_digit_decimal(cc) ||
    (cc >= 65 && cc <= 70) || (cc >= 97 && cc <= 102);
  }
function cc_digit_binary(cc) { return cc === 48 || cc === 49; }
function cc_hspace(cc) {
  return cc === 32 || cc === 9 || cc === 160 || cc >= 0x2000 && cc <= 0x200A;
}
function cc_vspace(cc) {
  return cc >= 10 && cc <= 13 || cc === 133 || cc === 0x2028 || cc === 0x02029;
}
function cc_space(cc) { return cc_hspace(cc) || cc_vspace(cc); }
function ch_boundary(ch) {
  switch (ch) {
    case '(': case '[': case '{': case ')': case ']': case '}': case '"':
    case "'": case '`': case ',': case ';': return true;
    default: return cc_space(ch.charCodeAt(0));
  }
}

function stream(source) {
  return {'src': source, 'pos': 0, 'line': 0, 'col': 0, 'msg': ''};
}
function stream_copy(old) {
  var ss = stream(old.source);
  ss.pos = old.pos;
  ss.line = old.line;
  ss.col = old.col;
  return ss;
}
function stream_unexpected(ss, str) { ss.msg = 'unexpected `'+str+'`'; }
function stream_expected(ss, str) { ss.msg = 'expected `'+str+'`'; }

function read_number(ss) {
  var src = ss.src, i = ss.pos, len = src.length, needs_digits = true;
  var decode = function() {
    if (needs_digits || (i < len && !ch_boundary(src.charAt(i)))) {
      ss.msg = 'invalid number'; return;
    }
    var number = decode_number(src, ss.pos, i);
    ss.col += i - ss.pos;
    ss.pos = i;
    return number;
  }
  var ch = src.charAt(i);
  if (ch === '-' || ch === '+') { ch = src.charAt(++i); }
  if (ch === '0') {
    needs_digits = false; if (++i === len) { return decode(); }
    switch (src.charAt(i)) {
      case 'x':
        needs_digits = true;
        for (++i; i < len; ++i) {
          if (!cc_digit_hexadecimal(src.charCodeAt(i))) { break; }
          needs_digits = false;
        }
        return decode();
      case 'b':
        needs_digits = true;
        for (++i; i < len; ++i) {
          if (!cc_digit_binary(src.charCodeAt(i))) { break; }
          needs_digits = false;
        }
        return decode();
    }
  }
  for (; i < len; ++i) {
    if (!cc_digit_decimal(src.charCodeAt(i))) { break; }
    needs_digits = false;
  }
  if (i === len) { return decode(); }
  if (src.charAt(i) === '.') {
    for (++i; i < len; ++i) {
      if (!cc_digit_decimal(src.charCodeAt(i))) { break; }
      needs_digits = false;
    }
  }
  if (i === len) { return decode(); }
  switch (src.charAt(i)) {
    case 'e': case 'E':
      needs_digits = true; if (++i === len) { return decode(); }
      ch = src.charAt(i); if (ch === '-' || ch === '+') { ++i; }
      for (; i < len; ++i) {
        if (!cc_digit_decimal(src.charCodeAt(i))) { break; }
        needs_digits = false;
      }
  }
  return decode();
}

function token_numeric(src, i, len) {
  var ch = src.charAt(i);
  return (cc_digit_decimal(src.charCodeAt(i)) ||
          ((ch === '-' || ch === '+') && i+1 < len &&
           (cc_digit_decimal(src.charCodeAt(i+1)) ||
            src.charAt(i+1) === '.')));
}

function token_dot(src, i, len) {
  return (src.charAt(i) === '.' &&
          (i+1 >= len || ch_boundary(src.charAt(i+1))));
}

function read_sequence(ss) {
  var len = ss.src.length, elements = [], element;
  while (ss.pos < len && (element = read(ss)) !== undefined) {
    elements.push(element);
  }
  return elements;
}

function read(ss) {
  var src = ss.src, i = ss.pos, len = src.length;
  while (true) {
    for (; i < len; ++i) {
      var cc = src.charCodeAt(i);
      if (cc_vspace(cc)) { ++ss.line; ss.col = 0; }
      else if (cc_hspace(cc)) { ++ss.col; }
      else { break; }
    }
    ss.pos = i;
    if (i < len) {
      var ch = src.charAt(i);
      switch (ch) {
        case '(': case '[': case '{':
          var delim, prefix;
          switch (ch) {
            case '(': delim = ')'; break;
            case '[': delim = ']'; prefix = '[]'; break;
            case '{': delim = '}'; prefix = '{}'; break;
          }
          ++ss.pos; ++ss.col;
          var elements = read_sequence(ss);
          i = ss.pos;
          var tail = nil;
          if (token_dot(src, i, len)) {
            ++i; ++ss.pos; ++ss.col;
            tail = read(ss);
            if (read(ss) !== undefined) {
              ss.msg = 'dotted sequence must have exactly one trailing element';
              return undefined;
            }
          }
          i = ss.pos;
          if (i < len && src.charAt(i) === delim) {
            ++ss.pos; ++ss.col;
            elements = dotted_list_from_array(elements, tail);
            if (prefix !== undefined) { elements = pair(prefix, elements); }
            return elements;
          }
          return stream_expected(ss, delim);
        case ')': case ']': case '}': return stream_unexpected(ss, ch);
        case '"':
          for (++i; i < len; ++i, ++ss.col) {
            if (src.charAt(i) === ch) { break; }
            else if (src.charAt(i) === '\\') {
              if (++i < len) {
                if (src.charAt(i) === '\n') { ++ss.line; ss.col = 0; }
                else { ++ss.col; }
              }
            }
          }
          if (i >= len) { ss.pos = i; return stream_unexpected(ss, 'EOF'); }
          var text = decode_text(src, ss.pos, i); ss.pos = i + 1; return text;
        case "'": case '`': case ',':
          ++ss.pos; ++ss.col;
          var datum = read(ss);
          if (datum !== undefined) {
            var prefix;
            switch (ch) {
              case "'": prefix = 'quote'; break;
              case '`': prefix = 'quasiquote'; break;
              case ',': prefix = 'unquote'; break;
            }
            return list_from_array([prefix, datum]);
          } else { ss.msg = 'invalid '+ch+''; return undefined; }
        case ';':
          for (; i < len; ++i) {
            if (cc_vspace(src.charCodeAt(i))) {
              ++ss.line; ss.col = 0; ++i; break;
            } else { ++ss.col; }
          }
          ss.pos = i;
          break;
        case '#':
          ch = src.charAt(++i);
          switch (ch) {
            case 't': case 'f':
              if (ch_boundary(src.charAt(++i))) {
                ss.pos = i; ss.col += 2; return ch === 't';
              }
            case '{':
              ss.pos += 2; ss.col += 2;
              var elements = read_sequence(ss);
              i = ss.pos;
              if (i < len && src.charAt(i) === '}') {
                ++ss.pos; ++ss.col; return set_from_array(elements);
              }
              return stream_expected(ss, '}');
          }
          ss.msg = 'invalid `#` syntax'; return undefined;
        default:
          if (token_numeric(src, i, len)) { return read_number(ss); }
          else if (token_dot(src, i, len)) {
            ss.msg = 'invalid `.`'; return undefined;
          } else {
            for (; i < len; ++i) {
              ch = src.charAt(i);
              if (ch_boundary(ch)) { break; }
              else if (ch === '\\') {
                if (++i < len) {
                  if (src.charAt(i) === '\n') { ++ss.line; ss.col = 0; }
                  else { ++ss.col; }
                } else { ss.pos = i; return stream_unexpected(ss, 'EOF'); }
              }
            }
            var text = decode_text(src, ss.pos, i); ss.pos = i; return text;
          }
      }
    }
  }
  return stream_unexpected(ss, 'EOF');
}
