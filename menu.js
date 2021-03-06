(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.aC.Z === region.aN.Z)
	{
		return 'on line ' + region.aC.Z;
	}
	return 'on lines ' + region.aC.Z + ' through ' + region.aN.Z;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bN,
		impl.cd,
		impl.b3,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		F: func(record.F),
		aD: record.aD,
		ax: record.ax
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.F;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.aD;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.ax) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bN,
		impl.cd,
		impl.b3,
		function(sendToApp, initialModel) {
			var view = impl.ci;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bN,
		impl.cd,
		impl.b3,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.aB && impl.aB(sendToApp)
			var view = impl.ci;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.aH);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.br) && (_VirtualDom_doc.title = title = doc.br);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.bV;
	var onUrlRequest = impl.bW;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		aB: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.bj === next.bj
							&& curr.aQ === next.aQ
							&& curr.bg.a === next.bg.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		bN: function(flags)
		{
			return A3(impl.bN, flags, _Browser_getUrl(), key);
		},
		ci: impl.ci,
		cd: impl.cd,
		b3: impl.b3
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { bK: 'hidden', bz: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { bK: 'mozHidden', bz: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { bK: 'msHidden', bz: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { bK: 'webkitHidden', bz: 'webkitvisibilitychange' }
		: { bK: 'hidden', bz: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		bn: _Browser_getScene(),
		cj: {
			c: _Browser_window.pageXOffset,
			d: _Browser_window.pageYOffset,
			cl: _Browser_doc.documentElement.clientWidth,
			bJ: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		cl: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		bJ: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			bn: {
				cl: node.scrollWidth,
				bJ: node.scrollHeight
			},
			cj: {
				c: node.scrollLeft,
				d: node.scrollTop,
				cl: node.clientWidth,
				bJ: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			bn: _Browser_getScene(),
			cj: {
				c: x,
				d: y,
				cl: _Browser_doc.documentElement.clientWidth,
				bJ: _Browser_doc.documentElement.clientHeight
			},
			bE: {
				c: x + rect.left,
				d: y + rect.top,
				cl: rect.width,
				bJ: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.h) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.j),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.j);
		} else {
			var treeLen = builder.h * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.k) : builder.k;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.h);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.j) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.j);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{k: nodeList, h: (len / $elm$core$Array$branchFactor) | 0, j: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {aP: fragment, aQ: host, be: path, bg: port_, bj: protocol, bk: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Menu$Menu = 0;
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Menu$init = function (flags) {
	return _Utils_Tuple2(
		{E: 0},
		$elm$core$Platform$Cmd$none);
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Menu$subscriptions = function (_v0) {
	return $elm$core$Platform$Sub$none;
};
var $author$project$Menu$mainMenuToString = function (m) {
	switch (m) {
		case 0:
			return 'Menu';
		case 1:
			return 'Playground';
		case 2:
			return 'History';
		default:
			return 'About';
	}
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Menu$togglePlayground = _Platform_outgoingPort('togglePlayground', $elm$json$Json$Encode$string);
var $author$project$Menu$update = F2(
	function (msg, model) {
		var menu = msg;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{E: menu}),
			$author$project$Menu$togglePlayground(
				$author$project$Menu$mainMenuToString(menu)));
	});
var $author$project$Menu$About = 3;
var $author$project$Menu$History = 2;
var $author$project$Menu$Playground = 1;
var $author$project$Menu$UpdateMenu = $elm$core$Basics$identity;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$html$Html$img = _VirtualDom_node('img');
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Menu$view = function (model) {
	var menu = _List_fromArray(
		[
			A2(
			$elm$html$Html$ul,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('menu')
				]),
			_List_fromArray(
				[
					(!(!model.E)) ? A2(
					$elm$html$Html$li,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(0)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Menu')
						])) : A2($elm$html$Html$span, _List_Nil, _List_Nil),
					(model.E !== 2) ? A2(
					$elm$html$Html$li,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(2)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('History')
						])) : A2($elm$html$Html$span, _List_Nil, _List_Nil),
					(model.E !== 3) ? A2(
					$elm$html$Html$li,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(3)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('About')
						])) : A2($elm$html$Html$span, _List_Nil, _List_Nil),
					(model.E !== 1) ? A2(
					$elm$html$Html$li,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(1)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Play')
						])) : A2($elm$html$Html$span, _List_Nil, _List_Nil)
				]))
		]);
	var _v0 = model.E;
	switch (_v0) {
		case 0:
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('main')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('row')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('item')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$h1,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('title')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Pixel Art')
											]))
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('item')
									]),
								menu)
							]))
					]));
		case 1:
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('content')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('play')
							]),
						menu),
						A2(
						$elm$html$Html$h2,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('play-help')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Press Q for daytime or E for nighttime')
							]))
					]));
		case 2:
			var history = A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('overlay-wrapper')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$id('scroll'),
								$elm$html$Html$Attributes$class('overlay')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Pixel art has been around for decades, nowadays its used as a type of “style” in terms of art, but at its roots, Pixel art started as a design paradigm for video games. There\'s 5 distinct generations of this craft, the first being the early years which is from roughly 1972-1983. In 1972, Pong was released, considered as one of the first arcade games ever made, given the limitations of the technology at the time. This resulted in very blocky assets, and a simple screen. (Griffiths)')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('https://ietp-web-cdn-eandt-cache.azureedge.net/4/0/a/e/0/a/40ae0a569ab647dbb08695b4671ca4fc0a0c4db3.jpg')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('(Allan Allcorn\'s Pong.)')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('However, many others decided to take a stab in the gaming industry.  During these years, consoles such as ColecoVision, and the Atari 2600 adopted this limiting style of content, however as the years progressed and technology approved, the creator could make more engaging games.')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('https://images.nintendolife.com/a8dd2fc89f198/the-fabled-colecovision-port-of-donkey-kong-in-all-its-glory.900x.jpg')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('(Shigeru Miyamoto\'s Donkey Kong)')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('https://www.retrogames.cz/games/017/A26_01.gif')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('(Toru Iwatani\'s Pac-Man.)')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('The next and probably one of the more popular eras would be the 8 bit era, from 1983-1987. Although still fairly limited in terms of specs, Developers became more confident in their ability and its clearly shown in this era; with more colors on screen; and more refined; recognizable characters; some consoles from this era included Nintendo Entertainment System (NES), Sega Master System, Game Boy. (Griffiths)')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('https://images-na.ssl-images-amazon.com/images/I/515yYsRyIBL._AC_SX522_.jpg')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('(Shigeru Miyamoto\'s Super Mario Bros.)')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('https://i.pinimg.com/originals/7a/17/36/7a17365db92b185ad1496fcaa842bea4.png')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('(Yu Suzuki\'s Outrun)')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('https://i.ytimg.com/vi/bOYoBkZV9RU/hqdefault.jpg')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('(Alexey Patjitnov\'s Tetris.)')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('The 16-bit era of pixel art was the era that home consoles rivaled Arcade machines, matching what the arcades were capable of; and more, with 16-bits; the player could be introduced with more defined objects, shaded to give it a more three-dimensional look. Better, more varied level design, some adopters of this style included the SNES (Super Nintendo Entertainment System), and the SEGA Genesis. (Griffiths)')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('https://miro.medium.com/max/630/1*BG38rz6ciNSZsQ_fLaJ02A.jpeg')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('(Shigeru Miyamoto\'s Super Mario World.)')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('https://www.retrogames.cz/games/117/Genesis_01.gif')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('(Yuji Naka and Natao Ohshima\'s Sonic the Hedgehog.)')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('The next era is the beginning of Pixel Art graphics downfall, 1993-2006. With consoles like the Nintendo 64, and the Original Playstation pushing three-dimensional graphics, pixel art no longer appealed to the eye as much, few developers refused to adopt this new graphics format till it was more efficient, but that would only lead said companies to their demise. Some consoles from this era included: Sega Dreamcast, and the Gameboy Advanced. (Griffiths)')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('https://i.ytimg.com/vi/YqYfRVgAipk/maxresdefault.jpg')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('(Yuji Naka\'s Sonic Adventure 2.)')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('https://pm1.narvii.com/7245/fa73334ef94a517cb2e24d1bfc7d4c322db0b21br1-960-640v2_uhq.jpg')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('(Junichi Masuda\'s Pokemon Ruby.)')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Finally, the Modern era; from 2006-to present. Pixel art doesn\'t even compete in the gaming market against the polygonal graphics of today\'s world, however that doesn\'t stop Pixel art from thriving on its own, its seated itself nicely alongside handheld consoles like the nintendo DS, or even more recently, the nintendo switch. Another home Pixel art has found itself in is art itself, Pixel art’s style is nostalgic to most; as it\'s reminiscent of the time of its golden age, from still images, to GIFs, Its developed its own community, people who specialize in making this type of art. (Griffiths)')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('https://sketchbook.tsukimori.co/wp-content/uploads/Toyoi-Yuuta-Japan-dailylife-03.gif')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('(Toyoi Yuuta\'s \'The Japaneese Daily Life\')')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('https://media.giphy.com/media/pVGsAWjzvXcZW4ZBTE/giphy.gif')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('(\'GrandMasterSubZero\'s Smoke in the R[A]in\')')
									]))
							]))
					]));
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('content')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('play')
							]),
						menu),
						history
					]));
		default:
			var about = A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('overlay-wrapper')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$id('scroll'),
								$elm$html$Html$Attributes$class('overlay')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('This Page has all the Assets I drew for the project, which included 2 animated sprites; and 5 still images, At the bottom ill explain a few functions in the code')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAfgAAAH4CAYAAACmKP9/AAAFeUlEQVR4nO3aIY6FMBiFUfZvkEiWgUQ+yVKQyI4tdS+ZaTO95yTX/wlpPsOyAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAPx/67qWeqPvAQB+gcADwIQEHgAmJPAQxIPPdF1XqTf6Hv5G+77v+37N+4eJeeCZBD6DwEMwDzyTwGcQeAjmgWcS+AwCDwABBB0AJiTwADAhgQeACQk8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwNe2bSv1Rt9DH/u+l3qj76GP8zxLvdH30Mfn8yn1Rt9DJwKfSeAzCXwmgQ8l8JkEPpPAZxL4UAKfSeAzCXwmgQ/RBv15ntcEf05t0FuCP6fjOEq9luDPqf2uLcGflMBnEvhMAp9J4EMJfCaBzyTwmQQ+lMBnEvhMAp9J4FmWxU92qQQ9k6BnEvRQAp9J4DMJfCaBDyXwmQQ+k8BnEvhQAp9J4DMJfCaBBwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAPjaeZ6l3uh76GPf91Jv9D30sa5rqTf6HvrYtq3UG30PnQh8JoHPJPCZBD6UwGcS+EwCn0ngQwl8JoHPJPCZBD7EcRylXkvw59QGvSX4c2qDft/3a4I/pzboz/O8JviTEvhMAp9J4DMJfCiBzyTwmQQ+k8CHEvhMAp9J4DMJPMuy+MkulaBnEvRMgh5K4DMJfCaBzyTwoQQ+k8BnEvhMAh9K4DMJfCaBzyTwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADAINd1lXqj7wEAfoHAA8CEBB4AJiTwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAANDVD+TwYggZo862AAAAAElFTkSuQmCC'),
										$elm$html$Html$Attributes$class('black')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('This is the Sprite-sheet for a single star, which has a total of 13 frames of animation')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/gif;base64,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AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'),
										$elm$html$Html$Attributes$class('black')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('(Final Star Animation)')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAgAAAAEACAYAAADFkM5nAAAO60lEQVR4nO3dTY4tNxmA4SR3DSwie0CMmCAxzIApQghlA1kCO2DELthEFGURDLIQGFLlNHa5bZd/vueRPLrdp09X1We/Vzq696uvAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA4PP+M3gB6zL/EJgNAOIy/xCYDQDiMv8QmA0A4jL/h3ODubrdv+9+/5vb+tMfv21a6eulP2/i7w2Y/3AEAFc2AIjL/AcjALiyAUBc5j8YAcCVDQDiMv+Hu13w1htaWunPm/h787GhA29D2I6/EMRi/oMRAFzZALgSALGY/2AEAFc2AK4EQCzmPxgBwJUNgCsBEIv5P9yrB74g2M7UDcCGMN3Q++9+Ls/8H04AkGMDiE0AxGb+DycAyLEBxCYAYjP/hxMA5NgAYhMAsZn/wy0VAIJguqaB/+Xnf7660vc78bqdYuqGb4OfzvwHIwC4sgHEJgBiM//BCACubACxCYDYzH8wAoArG0BsAiA283+4pgPfDT7e7Xr/9K+/39bs58HzMdzUABAE05n/wwkAcmwAsQmA2Mz/4QQAOTaA2ARAbOb/cAKAHBtAbAIgNvN/ODeYnOzzUXv/egdl+v3p+5143Vbx0TV5vFb70Ff6/H3wnunL/B9OAJBjA9ibAKCF+T+cACDHBrA3AUAL8384AUCODWBvAoAW5v9wbjA5XZ+PVPr66SoF4IPXO132+qX364cvX7Kr9P2z/0IgAF5n/g8nAMixAaxNAMS6328z/4cTAOTYANYmAGLd77eZ/8MJAHJsAGsTALHu99vM/+HcYK6qDhTPx+uy96N0f1oDwF8Ijmf+g3GDubIBrE0ACICRzH8wbjBXNoC1CQABMJL5D8YN5soGsDYBIABGMv/BNd1wN3g7t+v5/Tff3FbrAZF+fe2GUVoBPhTWNI+9PwRowz+O+edGAMRiA1ibALAfjGT+uREAsdgA1iYA7AcjmX9uBEAsNoC1CQD7wUjmP7jsQLrBx8ne39IGkP556fVKz0vtCvA8dD3w09cr3d/eQbBAANQ+Y6cz/9wIgFhsAGsTAAJgJPPPjQCIxQawNgEgAEYy/9wIgFhsAGsTAAJgJPMfTPaCbniDow9wrar7V3tAtD4vtSt9/UnXdKTsgdjh+lQFQfr1G/yFoOr9BjhQzH9wAiA2G8BeBIAA6Mn8BycAYrMB7EUACICezH9wAiA2G8BeBIAA6Mn8B1M14K03uPeAt77/9PsnXP/VdN0QW5+XDut0TRto+v21P2/DvxBkfx8HjPmPRgB4YK5sAHsRAAKgJ/MfjADwwFzZAPYiAARAT+Y/GAHggbmyAexFAAiAnsx/MNkbXlqlA3f0De79IZSXrvlOXt0Qalftz590Dd80eoPsusHXBkvrXwh6//wO13N15v9wAsADkmMD2IsAEAA9mf/DCQAPSI4NYC8CQAD0ZP4PJwA8IDk2gL0IAAHQk/k/zNAB6H2DSyv9+bXv3wNT1PS8PLi+Tc9P6fXd31+pDYKu8z37LwQDnt/TbTX/7l+ZAPCA1NhqAyi93w9+ZjQCQADU2Gr+3b8yAeABqbHVBlB6vx/8zGgEgACosdX8u39lAsADUmOrDaD0fj/4mdEIAAFQY6v5d//Kqm5o+vWlNTsASssD0qzp+am9/rX3/xPP8+my17P2QK790O3b81/6egd+M/O/OQFg4FvYAPYiAARAT+Z/cwLAwLewAexFAAiAnsz/5gSAgW9hA9iLABAAPZn/zVVdsAcDsXQAGPjhqp6n1g0g/fNPrNNVHdjpn3/92z9nV/r66Z9/+cMPTav08z5Y2d8/fb5K68Hrc2f+NyMABEBPNoC1CAAB8CbzvxkBIAB6sgGsRQAIgDeZ/80IAAHQkw1gLQJAALzJ/G+m94Dc1t++fHl1ld6PDeB1Vdc//fraAyf98wm/72xVB366eh/QtUHwwv10QLzL/C9OAAiAkWwA7xIAFdfnwaKN+V+cABAAI9kA3iUAKq7Pg0Ub8784ASAARrIBvEsAVFyfB4s25n8zrQNyv0G/++urq/b9feL3o032epcOjNLXf/Ca0WQDoPQP/bR+iG/DIOBd5n9xAsADNJINYCwBwMrM/+IEgAdoJBvAWAKAlZn/xQkAD9BINoCxBAArM/+HyR74tRtI6/pEEDBW08CXNoAPFndtAZ/cn9JqfX1BcBzzfzgBQI4NYC4BwEzm/3ACgBwbwFwCgJnM/+EEADk2gLkEADOZ/8N0PfBLB/bo1590DU9WdYDY8Jf3ZNOtuR8C4Gzm/3ACgBwbwFkEADXM/+EEADk2gLMIAGqY/8MJAHJsAGcRANQw/4fJDvzb/3DPgJ/ngWrTtKEb+HBu97P0nxWV/uGiB4uxzP/hBAA5NgBqCICzmP/DCQBybADUEABnMf+HEwDk2ACoIQDOYv43lx2g0oCW1oMBbnq/L7w/D2Be0/1w/Y83dF5r/7OjidfhVOZ/cwLAA9jCBkCOADib+d+cAPAAtrABkCMAzmb+NycAPIAtbADkCICzmf/NZG9Y64DuFgC1G0yH93+6rhuC67+9J5v6m4uxzP/iBIAHcCQbAFezD3zPy7vM/+IEgAdwJBsAV7MPfM/Lu8z/4gSAB3AkGwBXsw98z8u7zP/iZg9g1wBYYJHn+kNc5n8xsw9MARCL6w9xmf/FzD4wBUAsrj/EZf4XM/vAFACxuP4Ql/kHAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEL5z+AFACxIAABAQAIAAAISAAAQwO2A/u73vxm60p838fcGgNAEAAAEJAAAICABAMCHfCjsLNkD/09//HboEgQA+xAAZxEAADwiAM4iAAB4RACcRQAAn+ZAOFv2gPChsO3drvfoA18QwFkEwNkEwNkEAPBpAuBsAuBsAgD4NAFwNgFwNgEAPJY9EGwA2xt6f93/6Zru779//Mdt/fLzP7uun/7199tK3+/E6wZ8JQBOJwDOJgCATxMAZxMAZxMAwKcJgLMJgLMJAOAxB0IsU++3+z/c7fqlB256/dM/733gCwBYmwCIRQCcTQAAjwmAWATA2QQA8JgAiEUAnE0AAI85EM7WdH9HHwjpSt/vxOu2q2wA1N6P3s9H+v3p+5143SAkAXA2ARCLAAAeEwBnEwCxCADgMQFwNgEQiwAA/q+lDwQfEuqu6kNhbx/4AuBXProGuZX9/tYASJXeT+l+1r7eg98XqCAAYhEAexEAAgCGEQCxCIC9CAABAMMIgFgEwF4EgACAYZY+EARAd10PBB8Ka5Y94NL7U1ql1yu9/moB0OH3BTIEQCwCYC0CQADANAIgFgGwFgEgAGAaARCLAFiLABAAMI0DIZau9/vtA+HlazVC1QFXez9++PIlu0o/v3R/3g782u/3FwaoIwBiEQBzCQABAMsQALEIgLkEgACAZQiAWATAXAJAAMAyHAhny15/93u4putdOuDSVQqA77/55rbS91d6P+nX167SAZ2+v9ZAEgSQ50A4mwCYSwAIAFiWA+FsAmAuASAAYFkOhLMJgLkEgACAaZoG1IGwndv1+Uuydv9Q2ITrWatpntLvr12lAOgdBLUrff3Sz69dAZ4vqCIAYhEAcwkAAQDLEACxCIC5BIAAgGUIgFgEwFwCQADAa6oO9NP+oZCXr/WKsverFADpn5der/eBdeD9rbp+//7xH7fV4XpkD9jWIGhdD16/6/W1n3A6ARCbAFiLABAA8BoBEJsAWIsAEADwGgEQmwBYiwAQADBM0wDUfkio9uelX1+7DGhR1f0oBUDvIKhd6etPuqYjZa/fC8/77fVq57/3Gv37Tbi+8CoBEJsA2MvsA0oA2E84iACITQDsZfYBJQDsJxxEAMQmAPYy+4ASAPYTNta0Aadfn34Ip/ShnNIAtx4AHQIimqrnobQBtn5IsMOK5u3rcXv92g8FbhAA2d/3hZ8HQwkAA30lAPYmAAQAPCYADPSVANibABAA8JgAMNBXAmBvAkAAwGNNB276/aVVu0HUvn5ptR5oA+/Dql4NgtJKnx/3bznZwB+9OvxDQBCKAHCA5AgAaggA2IgAcIDkCABqCADYiABwgOQIAGoIANhI1Qb/IACyr1870L0/BNghcKJpej4eHMhNgSjoltN0P1vXlz/8kF3p+3uw4GgCQADkCABqCADYiAAQADkCgBoCADYiAARAjgCghgCAjbUOSNWBn359umGkG/jb68HvG01VEKRfX3tAp89L6UNete/n5WsXwe36lg7k1Vf6+0y8rvAKASAAcgQAOQIANiYABECOACBHAMDGBIAAyBEA5AgA2Ej2AC4d2KVVer0H/3DHaou7qgO4NgAGPB+MdbveX//2z7e1wAFeFSgffA8cRQA4QFoIAK4EAGxEADhAWggArgQAbEQAOEBaCACuBAAsrOmATgc6fb3RA1/6+ZOuKf+TDYLSgV/7fKV/PuH35a5pP3hwf6vWgJ8PWxMAjCQAYhMAsDABwEgCIDYBAAsTAIwkAGITALCQqgO/9j/XmL0M7PKaNujS13/wmsxVdX873M/b19fuZ54nTicAmEkAxCIAYCECgJkEQCwCABYiAJhJAMQiAGAh2QEp/UMrrR/qq/1Qzws/j7G63t/S6034/chbOgAe/MNmcBQBYMDfJABiEwCwEAFgwN8kAGITALAQAWDA3yQAYhMAsLDaDTU70KX14PWzP08ALK/q+XD/jpfdXwQAzCUA6EkAcCUAYGECgJ4EAFcCABYmAOhJAHAlAOBgpYBoHajb99f+Z0UvvL/osge+YAuv6vkYcP+rgqDDz4NQBEBsAoAcAQAHEwCxCQByBAAcTADEJgDIEQDAY1UHfmml319a6c+feB120XS/PhFs7KXqAH7hQPZ8wcIEwF4EADkCAHhMAOxFAJAjAIDHBMBeBAA5AgB47Mkh8OaiTtcgEGjbWy0AgIXNPvBtOG0EAFcCAHhs9oFvw2kjALgSAMBjsw98G04bAcCVAAAISqDFJgAAghIAsQkAgKAEQGwCACAoARCbAAAA6oIg/fqJ7xsAaCAAACAgAQAAAQkAAMCHPAEgIgEAAAEJAAAISAAAAAAAAAAAAAAAAAAAAAAAsLz/AksZDb3mLmJyAAAAAElFTkSuQmCC'),
										$elm$html$Html$Attributes$class('black')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('This is the Players Character, the sheet consists of 7 frames of animation')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/gif;base64,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AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'),
										$elm$html$Html$Attributes$class('black')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('(Final Sprite Animation)')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('To make him go the other direction, I simply flipped the all the frames')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGAAAABgCAYAAADimHc4AAAAzElEQVR4nO3VsQ0CMRBE0Q0upwx0dbgH+qNSX3SUwIDmPcnJRJZ+sDMAAAAAAAAAAAAAAAAAAADwV473Y3u5J4AA3U8AAbqfAAJ0PwEE6H6znrPv91rn59m/swsgQPc+v/CJ5l0AAbp3NyC8CyBA9+4GhHcBBOje3YDwLoAA3bsbEN4FEKB7dwPCuwACdO9uQHgXQIDu3Q0I7wII0L27AeFdAAG6dzcgvAsgQPfuBoR3AQTo3t2A8C6AAN27GxDeBRCge3cDwrsAAnTvF8YvcGECEfxKAAAAAElFTkSuQmCC'),
										$elm$html$Html$Attributes$class('black')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('This is the Grass texture the player walks on')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGAAAABgCAYAAADimHc4AAABw0lEQVR4nO3avU0EMRBA4cnIKeAkEiiBkDKISSmBOmiANmjuSE4+B+tbA2s/7/o9aZIVSOx8aH90FzFGZ2jskgBwAgClRTx/P6R5+rpP0/p4TI4hAJwAcOnkey49Px4CCEAmAJz3ADgB4ATo1OobKbh0AUKA5gkAhD9ilo6XJir+UaBd/ikB4ASAwx8xa5b+24kdYQgAJwDQIZcugADVCQCHP3oKIACaAHDIPUCAawLACTBQTTE6L3o3S88TAE4AuE0eST9eT5vMka/1pQSAE2Cgaj/uO0fEZksvTeXfcagEgBNgB1EAdkkAOAEaVnOdFaBhAsAJAJdO+O3lLk0UYD7fH9Pki8uP10zpd0MAAXonwECtYpQWl/98zZQwQgAByAQYqEWM/yy9BiMESAkAJ8BALT6GdgaoeWM/bALACQBHAdS8pU+RAHACwCEAsy89TwA4AeC6Abj05QSAEwAuLcLPA5gEgBMAbvEe4Lci+iUAnABwi/cAvxvaLwHgBIATAE4AOAHgBIATAE4AOAHgBIATAKjb0m8ATI0hAJwADbt1ot2XPuO9QQA4AYCGXe4sGALACQC3et3f6ewmelECHHSa9gNPcAyztR+kAQAAAABJRU5ErkJggg=='),
										$elm$html$Html$Attributes$class('black')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('This is the Tree Sprite')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGAAAABgCAYAAADimHc4AAAA+klEQVR4nO3WQYrDMBQFQZ90Djx3mLMkm1kEgkGbpL/lKtDeeg1GxwEAAAAAAAB82OP3eBSnvvcYAsQECFSji/GvHlyAAaPfLsDpAH8/zblbDAFiAgTGjX63GALEBAhcZvRdYwgQEyBw+dGvHkOAmACxrUZfiFHv/UaAmACBbf/7CwFGxBBAAAEEEEAAAQQQQAABBBBAAAEEEECAfCgBBowlwIZHAAEEEGBGgGz0VwLEBBhEgJgAMQEG2SrGtKfnCgFiAsQu/zw9+34BBFgiwCCjY+w09BkBYgIMtXL5b556j6+rBxdgwOi3DvDK6DEBYgIAAAAAAADArp5JWBGJaBotAwAAAABJRU5ErkJggg=='),
										$elm$html$Html$Attributes$class('black')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('This is the Sun Sprite')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/png;base64,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'),
										$elm$html$Html$Attributes$class('black')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/png;base64,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'),
										$elm$html$Html$Attributes$class('black')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('These are the Sprites for the clouds')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGAAAABgCAYAAADimHc4AAABYUlEQVR4nO3WS4rDMBBF0ex/gb2JXkR62uAYS+jzyvE5kJmRrLpg5fUCAAAAAAAAAJ7h5/f9XvlLn688AcIE2GT1oIW5kB64AAWG/ogALYdPeUQYAcIECEgNetZet48hQJgAYSOD6P372LJO7zO3vxsECBMgYNZ3f+f90RtJgMkE6DjwCl8boPfwO4d+9g69z6fnfSBAmAABqcGNGHnncveBAAJ0E+DDYUak7gwBJq/Tu5cAk9fp3ev2Ac4ONjKI1L4CCFBjEKl9bx+ggpb3F2AhAcIeEaBypJazCLCQAGFfG2DnPbFzfQEuBrR6fQEuBrR6/RIB/ksFSAVOz/tAgDABwloOP2tYI+vM2jc97wMBwgQoZNa3+GzQK9ZveSY912YChAkQtvOv4Qpn75+eazMBwgQopHKMljsmPb9hAoQJUFTL4Xf+0vPYLj1wAQoM/dEBzhh0mABhAgAAAAAAwHR/BfTWPDRN3SsAAAAASUVORK5CYII='),
										$elm$html$Html$Attributes$class('black')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('This is the Moon Sprite')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAMAAAADACAYAAABS3GwHAAACuklEQVR4nO3WW47dIBAEUK8iq8mys7/kL7mKBgbbPBr6HKl+fRmokua6AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgOP8bggcywBIzQCYqqVwI4r45ncNg24MgNQMgBRGFO5u+W598+evH38z6DwkYgCkZgCkMLroQ/NZ+lIav0VSy0v8JgbAW8tL/CYGwBNDC/fyf/HuZzAG/mcABpCaARhAakNL1msMC8/A4SKXL8IZOFzk8kU4A4v1+n/51jdHFO5u+Uaf4c0gB70LXzAAA0jNAAzgWCsvPnzhop1nQtJZfeGhCxftPBOSzuoLD124aOeZkBSGlnXENzMMoOVsk8d2LAMwAAN4GwMwgJ0sKeLM3zopM++q8jZHMYCNMvOuKm9zFAPYKDPvqvI221tS+giPuntm3lVjP7ZkAJtm5l019mNLBrBpZt5VYz+2Ear0BhA/jV3ZhgGIAbQmwkWvLkD2NHYltLClN4C9UulNaAYgQ99oYpcfMQAZ+kYTu/xI2NLLXql0KDQDkC6pdCg0A5AuqXQoHKWX7qn0KRwDkO6p9CkcA5DuqfQpHKWX7in1alHHqwxAuqfUq0UdrzIA6Z5SrxZ1vMoApHtKvVrU8SoDkO4p9WpRx6sMQLqn1KtFHa8yAOmSUpcuA5AMKXXpMgDJkFKXLgOQU1PqTyXhGIA8Tqk/lYRjAPI4pf5UEs63h159yRIrLZ25gpf+kwHIrbR05jIAOTUtnbkMQE5KS0+ujUr/yQDk27T05DIAOTUtPbk2HcCnJ3+obJ7spf+0/DFkfgzgn+WPIfNjAF9b/jASNimsvmSJmxRWX7LEDRxF0UnNAEjNAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAmf4Aob/9qFnfqdQAAAAASUVORK5CYII='),
										$elm$html$Html$Attributes$class('black')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('This is the Bush Sprite')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/png;base64,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')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('This allows us to see what keys we have avalible to use, as you can see it has keys like Space, Shift and Backspace; keys we dont have mapped out to anything')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/png;base64,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')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('This is how I send Text to the project, p stands for paragraph')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/png;base64,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45xHE+YTd/oQaVlfsHrXNoq8exLldUXb0lzyCnPJLb2wwY6IiIhIbdF0LJFGqqTQShG55JzNv7TRmksO7niYjEAuOYXg6mamOH0pr60G8KSH0URxYT5aly4iIiK1RSMhIo3V2UNklLjj1cJ4aZurJx7kk2ezA7l8n3MGg1cn7ryw3zmc9i0gM/Oo1oOIiIhIrVEIEWmsirax8zSER82kh08wnl69Gd6jJx7Zu0nIAbBj+d82vne7nxHde+PfvBNduj9FpPN+/nNYn4wlIiIitUfTsUQarVz2/WcmHvdN5oEhy3kEKzmZO/jX1mWkXWhybjnLtrrzaPepTO9YfrPCuE3zdY8QERERqVUKISKNmf0AcV8+Tty1G5BrWcgbloUOLEpERERud5qOJSIiIiIiDqUQIiIiIiIiDqXpWCIiIvXIz3p7UERuA/pTJyIiIiIiDqUQIo1GVtYPdV0CUH/qEBEREamvFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMShFEJERERERMSh7ggN7/ZzXRchIiIiIiK3D42EiIiIiIiIQymEiIiIiIiIQymEiIiIiIiIQymEiIiIiIiIQ7nUdQEiDYMbzUP7E+R/J00MJRQc/5DEI9l1XZSIiIhIg6QQIo2QG83Dh9GheTJJ8fsoqoEe7/CMJtjfmayE5aQVlOJEYQ30KiIiInJ7UgiRRsXg2YXA0CiauzrjUhPp4zyjuQUuBUfJ/NFKKVBac12LiIiI3HYUQqRRMTT1wH5yHd+59KSj/7VaOeMeOITggJY0MZnAdoyjOz4np8yblnfH4O/TkiZONgoy4jnyv2RsZYCTC07N76NL//sAOHvgLZIzSjF4RhMUGkazpiYoSiMt6QvSfrQD3rTq/Fv8PVvQxAAlufEk7jmAzTWIgPBovD09cCk5S27qVo6ezuZnnPEIHUZQa0+amEw4lRZQkLWbo0nfUVh2vupmXWh3dyc8PZpCcS4Z+z/Aknu9GkRERETqJ4UQaVQKT8dhAQxtrtfKGVdPX5yyN7E/JYMyZ2fKSpzxCB9Iu2YnORL/OVaXIIKiYujQ5nuSTloBKDv3Nfv3JGEHfi4rBddOdOgcRlnq5+xPK6Bp8EBC7/kNBV9t4lyZB808PSg4/AFJmTacDCXY8KZt5/54FsRx6KuT/Nw8mpDI/tx17n0sVmdMTVvilLmJ/akZ4BpEwD330f6uNBJPnAXXXxAaFQWnt3JwfwalTm5gu1ENtf98i4iIiNwMfTqW3LbKbHnY7IUUF1kpdboL39Ymzh75mtyiQoqt33E6LY+mre7CueIxZaXlAQRw9Q3DXJSM5WQGxSVWzh1J4EcXfzzNF1qXUFp0luKSQmxFdmgWgXfTDNKSkym0F1KUtZtMqwfNvFtc6r+kvCbbj9+RdqYQV09fnAGTbwRmWxLHjxynsKgQW0E2tpKq1CAiIiJS/2gkRATAxQ2DwUSze54huuL2PBNGoLJBBYOrG04eUXTuH1Vhq41sk3MlreEOkxtG5ztp95vJtKuw/ac8E1BwVftimw2am3ACTK5uUHSWn6pVg1auiIiISP2kECICUFJISbGN7MR3OZJ99Yt3UyWHFBfZKDt3gP3f7sNWhVP8XGKjpPQkJ+PW8UPJlXuN1z22uMgGni1oApd92ld1axARERGpDzQdSwSg7Hsys0rw7BCDVzMzBqMbJrMvrteJ6UVZyRSYOxEU2BZXoxsGYwvczdeZB3XuO7KL/Glz9y/waOqGwWjGtZk3hiqUV5T1HQWuEQR3CMLN1Q1DU1/cjDdRg4iIiEg9oJEQEQDsnPvfOo7dHcNdUY/SxOBCie0Maf/9iLQfr3FIwT4O7zMRFPpbIkOa4lRqozDrP/wv0UpxZe3LMji5byuE30uH6N9gdC7BnpdM6rdxN15EXnCAw/vcCAqNoWNQUyjO4+zhtaSkVbMGERERkXrgjtDwbj/XdREiIiIiInL70HQsERERERFxKIUQERERERFxKIUQERERERFxqGqFEB+flrVVR4OqAaB79L11XQJQf56P+lKHiIiIiNR/GgkRERERERGHUggRERERERGHUggRERERERGHUgiRRqgd09du5N+T2tV1ISIiIiJSCYUQERERERFxKJe6LkCkxhgjeeHjhTzkd/5x4DIOjAWws2v2EJ5ZX1yHxYmIiIjIBQoh0njYE3lpYB9eoh3T1y4lavskHl56rHp9uA/k8959CKp0p4WFmxbzrq0GahURERG5jSmEiFRU+BVT45IwVbqziDQFEBEREZFbphAiUlGZlcN5VozX2G13aDEiIiIijZNCiEhFmo4lIiIiUusUQqRRssE1RzOuS9OxRERERGqdQog0QhlYzoBfl15EeWVw0GoHe3HVplKdn44lIiIiIrVH9wmRRqiATcveJd48iLe2rmPPt+t4fZChrosSERERkfM0EiKNkj3pY/44/OO6LkNEREREKqGREBERERERcSiFEBERERERcSiFEBERERERcSiFEBERERERcSiFEGk0fHxa1nUJIiIiIlIFCiEiIiIiIuJQCiEiIiIiIuJQCiEitcjoM4bdA8Zx/zV/01yIDBjH5wNiSR6ymOSevQl0ZIGNRigL+8XyN3/d+khERKQh0P/YIrXpp1R2njGQXnaN/U4RPBMeTPahxTzxfS7WsmKsDi3wVnkSED6BB+6+l4DmXriRw/ent/HprmUkW+1XNzc/xLMPTaZD6UYW/3MBR6t1Ln+e7zWFbmmxDDuaVUP1i4iISF1QCBGpRfa8eKbuv04DkyfeTnnsyU4jvcRhZdWgtkQGtiYvdTn/yjxDkXN7enQfy8QHYP7qpaRd1jaYmJiHcLPmgFsdlSsiIiL1gkKISC0weo1iR49ueACUJvHcxvf4osJoiNFjKBtjfo3/+cdhvRfzGEDuegbs2MaJKp2lBdGTZjFrRBhexlyS1sRh7Tcc87LRjF979iYrN9PV/9eMDPBkT8IK/m27UfsDrPt8coXHezjxbWci77+X8OZLSTt3YbuRVp1nElO4nH+dHs2UjtWp6S5m//aPjLwQXJrNJDkcoJgdu2fwZMal9GZyi2B290EMbWHGVniYt/b8kxX5F/a70iNoFM+1CybIzQWb9SQbD61iQUYuds5/T+4LZs+hNALbRRBmguzsL5mxZxt7rzWSJSIiIjdFIUSkFthzVnHvulUYfcawI8pw9f68T+iz7hNw7c3Hvbuxc/sCFuVX7xx+g/7EK6NasOu1GbydZKD75D8x3dtIwk3Ua3YNZWi7X/Nw2zD8S9PYk/YlicU30RFgMBmhJJe8wkvbXLwm8Hh4Bus+3kZe6Ohq9niSuVunMPeG07EMRAZ0Jznxn4z8yZMx94ziuV/cw8Zv95ANRLafyJtBRaxIfJuZPxbh3WoQc6LGYd0ee+m5d/anh/cunty2iuOGzizsNYoZAUkMO67pXyIiIjVJC9NFGqRWxAyPhPgVvLQ2EcuRBFa+9jEp1erDlUjf3izs/iI7ej/Gw+65fJIQS5/NsTz53X4O38y7/87dGNI5nLyUD9l3YUmIcyeG/KY3ObuXsK+oknUiNSjt+094Oeskh/P2s8hyEpoFEwrgFMqYIE/2fPceizJOcrgoi52WD1lx1ocBbfwxXuwhl21H4zlcBnZbEhuzSwjyrrhfREREaoJGQkQaIqMvId6Qvu/YpYXs6ac4YbXjXdU+3Hsz/94++P+4hznbPuTToltdlBJMj/4z6WL/iMW7dlLemzvtfzmTKOsy5qdm3GL/N1JMdl7mxUd5PxWDswsmAJM/gU1cCYtaQHLU5UflFZovPSjNI+2nCw9KsJcVg8GACajd+CQiInJ7UQgRuV3lf8Wc/xoYE9CZOb1f5OEz+/nU8hUbc3Jv4hO62nJv/9cY4raNv61bRlrphe2+BPi1xsN7Ji//YWaF9ncx5Yl7+XbT7/jX6Zp7eX/dJSyluaz+eh5z8yrffXG0Q+s/REREap1CiEhDZM8gJRtiAtph5nR5aPBri5/ZWI137K3sPf0Je09/grdHZ8a0684T977IjGILOyzbeOtYUhWnZLWlS//XGGHewd/WLeXoZQWcYuemsRx0vvDYiGfHOUwMTmXFur9z6Fx1Akgxdigf2aguWxonis1EentCXu7N9CAiIiI1SGtCRBqkTOLWJkL0GKYPCiMgIJLRkwYScpO9ZeftZ9H+N+mzaR4TD5/E5NuNblevp6+EL136vMajrTLYuGsbheZw/L3C8fcKxuwMYKfImkrmuQtfh8i12qHUSm5OKtbSG/VfkZXjheDvG0FXFxeMTi5VX6tRdpgVx7MICh3H/DbBhLr6ENmiM+N/MY7nvfRejIiIiKPpf1+RGufDcz1n8oTnpS2LBi1mEXD80HweqKEb7aWvf5U/d5jFrGlLGWzMIGFNHPHZwzHfyuqFslx2nv6EnaeresDddGzbGoOpNSOGLK2wPYdv1/2Of6XX5EqKIjYe3kDve/rzzgODMFXyEb3Xk3j0bSaWDuW50HGsdnOFn3I5fjaZFYUN8gYtIiIiDdodoeHdfq5qYx+flmRl/VCb9TSIGgC6R9/LrvjddV1GvXk+6kMd9aGGOuXel7e2Po11xkM8v/0mP19XRERExAE0HUukoXKPZPCoXkQFtMDs3oaYycOIsu5lc4ICiIiIiNRvmo4l0mA1JWLQeKZP8sVsspOdspclUxcTV82bHoqIiIg4mkKISEOVH89Lj8TzUl3XISIiIlJNmo4lIiIiIiIOpRAiUsN8fFrWdQkiIiIi9ZpCiIiIiIiIOJRCiIiIiIiIOJRCiMhtoSkxC97nk5mRVb/L+E3wG76ALz8Yd9N3bq9zxkheWPs+L/dqWteVNDKO+flr1IxRzNvyJQf+W/717vAWdV3RrXHw71plf5sCfr+E3RueJaIh/FDqb1MF+nvSWOjTsURuBxEjmRydycqRibdyP/XrM0bx1Nh2JC35Cym1dY4LnEKZ33scgUcX8IgltwoHeBIQPoEH7r6XgOZeuJHD96e38emuZSRbKzwj9kSWr8rk35PGELH9/0iqtQu4TqVtJjCkc086ePvi4WwnJ+cAO759ja3pF64znCEjl9H3stegNg5tHcBbqReuxZP2nWcysmMnfI12cjK3sSZuKQettfbdv75r/Pzd+FrBpfkQHvnNaDp6tcZgP0ri3iWsPnSAoku91Ni1Gj2G8s92qTy2P+mmfk+MgxawY5qdF387m83V7cAYxbwNLzPY+8odqfx92FMssSTwYt8+vGiMYt6GefjdRH1V147pa5cStX0SDy89VjuncOTvmv421QznTsTEPM4vWwXTysNM8dEZTP9yZ4UGnoR1nkbf4GDu9G6NW96nxK5ciOWyTox4tpnAiF/2Jry5F5Se4fvUZbz19bYKv9M3UkDc8jieemccD62azErLDQ+QekojISKNnoGYUf3wTtrM5jO1dxavfsOJMe7lo+0FtXeS8/z8+zDAKYl3T1XlP3mAtkQGtiYvdTn/+nwqizd9REbzIUx8YAL+V7RM37yeBO8YRvcy3EKFZrr6dqOHqbrHGWkV2Bm3nC/49D8zif18KTts4Tz4wHxizBXb2fj+4Azmfzjh/NdkPjh16QWLOWQOT0b5Ytk1jXkfLyTRpTeP9x9Lq5u4EqNrKA/7+mO+cdNruNbPXxWu1bkTI/pPo2PJNv7+0VgW780g8NdzGBngWYPX6krvoIcY4+F6aZOTDw+GDeXBan//bl365lcZ/9gkHr34FcvKdMfX4Qg187t2Y/rbVEOc3XEpPUnC/uXsO1tZA3fcjFa+P/wRW07kVNqFi99UpvS/H4/0lSxbN4m3Ni3n23Rr9Ws5soHPTgQzYniYRkMaMI2EiDR2xq70i3bn4JK93MSf+ipqQUy/SNj3FxIqvHnnNWoJG0cd45lhb1y2HcDv90v4ZFAyY4b/XzXfnbyLJ9rfxfHjsWwrq+oxB1j3+eQKj/dw4tvORN5/L+HNl5J2rsKu/L3EJbkzvV9XjNvjb3LkyJMBEUPxTvwvO7NKqnGcneSvnyK5whbLWXfCfz+Zjm08iTt06YVNcd4p0nJOVdJHW37ZsTOcmMMHqQcoAdbu7ETk8N7E+CxndVb1rsjk2YcZQcnsyUi7uZ+fa/783fhaXfx+R5cWR9ny+TKSrcC51/g88EMeubsnrpZ1FNXItRax58xhngifwD8NBlo28eed7sUcP76Jt2xVu0Tz8IXseCHy4uNXvv2SVwBOrOXhiz/fTYkYPoXpY7sS4mfEnp5M3LK/8pf1xy77GbNlnyYhKZmb5dVlJLOmDSM6xBOsp0j47F1eWhjPhRzj1/cPvDC2Ox0DPDFjJz1lLysXLmZlUkH5lJ+PF/LQhWGWwGUcGAtgZ9fsITyzvriKVbQgetIsZo0Iw8uYS9KaOKz9hmNeNprxayu8eq2R37Ub11LZ36ZLjHQcNZd5Y7sSaMzn4PrF/HF+PJW/hL6RhvC36RbYd7I1bifgS0zoZDpe1eAU+3YvYB9G/N2G8KDXlft9+WVUbwypc1i8ayflfxkPcPSmislk1/ZUJo/oS8elydf43kp9p5EQkUbOGBJJR2MGlqRK37qqoZOEERUBJ5KSL/uPMScpmXSvdoRc9Z+RgYiItliPJHKimqfy9u3DgCaHWXEi65ZKNpiMUJJLXuGVewpIScnAGNGVkPrwFpuLJ25YybPmV9ho4s6ub7P4iS+Jfex9nrm3NxfHBpzvJrA5fJ/+Py7Gn5wDfF/Smju9PXG0av38XXGtrfza4lZ4FMvF9JKLJT0Dg3c4d0KNXau1KIl3UuJJb+KDv7snttObeCvtJNlVPX7tVDrd04dus/dis+7iz7/sQ6d7+tCpQsA295rC6y9EYl3zF8YMm8SLmw3EzJzL9C41+K52677MWzSGEMsKnhk5lvEvJeI9Yhav/77dxSZms5H09e/y5ycn8OBjs3nb0o7JC6cQ4w7YE3lpYB863TOBlSfspCyfUH4d9wyoRgABv0F/4pVRLTi4ZAYPP7aYuMB+xHhX9svkgN+1a/xtusDk1Z0RIYnETpzK/8/evcdFVe+L/38dgcWGGEogQUgaFBqgDXjBy0EtCROzzBsW5Um32U6PmXW2+t1p/QzdW9wnsW1e9olu7uyi5S1TyRthIW5FVGCOIIIygozgZrDNEDQDen5/gAgKMoPctPfz8fDxkFm3z5r1WZ/5vNd6f9aaHX8Wz6jZzGrlMfnVtU3WsnkYPzcz+aU9eW7i16x4MYGlk+N43NOjVavTawsxugYQ1L65iaIdSRAixN2upweu9pfJb8+UDpUHniozhtKKxp/nZJBj9iZIYwcEMGX5EuYOsAN8CVYr6NKa7hg0rwdT/fwxFhwgwZobDDeyGcS4foGU53zN8SYKoNdfBleXds67t4QLwYOfwL30O/YVXitoBfmn3mfj/hhW7VjEpsxi3INjeGVo39pb24oTzrYmqiorUA/+nBVT3ibAxki5GRxUHR+EWF7/btxXBUdHJzBVUKkax8vTEpgf5EulqQIUV5xtaKN9dWBYwBw+/a0XyadTSb94gK+6jeSD4c/zRJulY91D2NiBqDI3sezvaeTozpK05j026z0IH9t4cK3PlDX1g8/TTxwgfe0oi9NNPMOfIpRjvLdsF2lnCtHu+xvv7a1AM3YUQXXz5Gx9j2VfHuSwthDdmQy+jd9FjiqAME1bBUPuhE8KgcMbWLY1A92ZNL6I29bs3c52P9eaa5vqZbN+2TYOa7M5/OVWDpe6oNG05jz5tbVNraC44GyvENh/HJyKY9WOGHaXevPk6EUMa0W+p9lQiAEXfHp2UnqauG2SjiXE3c5eAcytv3VvH8bGx5+hr03tn+fOrODJ7KLG8yi13STTjRsxZ6PNUYgK8kbRh/H0iIGoTCG8r/NA41lMppV3Z1SuIxmvOs9HqedvIxXBl2GjFzHAvLlBSkBjJnNtJ9i6PqgvsY/PYYJjg4/+Pa4u3aia5KMLebnYmt6JE179YnnBK4+N2+K5/o0XkKn9ov4v3aV0DI6fs0AzDr+U9EbpTdWVZZRXlTW5j7fi1msOB/r7Nth/XxLGja39r/EA0d/vIsPSlVlU/5rbV+AK1NRUUFVZBmYz2DS9htbuK1SRce5TppmMmJwn8OxVI8nnPia5QIXz7XQmG3FB46lgyDnb4O5KATk6M25qb1Sk1af/6Pe+w+IvC6nPBDMWWFjX7fBUu4A+A119f7uaHG0hRHrgqYDWDMpDT9WmawV541Z/gMvQtdWVdcUDjRvoj5+9nn6nLyDfaOamMfe09lyzpjzNtE3Xtm8oRn9tmvlnjEawV6z/Mrp221RL8ZhB6uCgumXLSTi0mHmtyztrNTvsqSp4n405qdQART+uJ8RnEUN6eXAoq9i6lZnMmFBQOmHslmgbEoQIcbczVmA2ueOqApq7GHgrppMsOphX/6Nn+qWJAZfGMowmBZVKARqmbVwmM6eYuZoAfAyBsHcves1AgoOcUBvPWvlUExUT/ILg4qdstzBX/2beDB4dxzjHRD7YEU/RlWa2pLoHzCVWjoHQsfYfK/gKoJsXLw2egHPu+7xbWtuVKK+0MgAJimVOsJmEXYs4essnPZm5UFIMwa44K4C5gvIaexwcFYq0c/mSA7McAAAgAElEQVSTFrCJIFyBKqNlg2VLL37JtMTagdpKz8l82KuIhWmH0V8FrpRj1YiFFutfc/tqprKyAhydcK5K5LPNiQC493MC83nKr7TNvgIYTXVH2pTNdxfLajuRNUaL07FaZnmn9nbHhNy6GCEseHc2YbqPeGPiXjIv/ozZdSIf7Ypun+1ZoHXnmhWabZuua3VzUq+rt021zJe+JjpxZ/3fN6d7tTNzBVU1YCgtuB5gXbmIwQTuKhfAyiBE5Y6KCgwdHEiJtiPpWELc5cw52eTjgY+6tbesq8ivuMTpun/5NU10pivOotOD50PeN03KOX4WozqEKWHdydy5jRRTAE+P6INKl0GmFZcMFeeRTHUr45vc063ssHgzYHQcUapkPtixhtxmt22H5iEPyM8mx6pLmjXoK4rIqCgio7yEUqC8su7viiLyLR6oquAVGMucgQrf71pEkqGlQig84O4BJgPlZuBKLvk/wQOe/a5fZXLtywO2F7lQamHHvKasvtynK2uguozT5XX7UmW06krvrevfrfe1RF9ApaMf6vpUDRfUnh5Ul2ZxgTba14ZMp9lQfBv5/CZzM/FGMTl6cFX3aXA3wBuNWqFUV2Blfa6u3Yzqxg1Vo9eVgac3aqdrn9mhCeoF+rqr/a590LiWkbR+J2kXf8YMKOo+eDZxJdmENaFTA+ZickrBU93n+hPVPL3xvKm8deVr4VxTnNzx7OlOk4tb4hZtU1vp+m1TnavG+nb8dMWl2osKHelKLhd+AtfuDcaA2Ljiam/dRYNrVGoPPCkkJ9/y8Uqia5EgRIi73cVjpOmcCArzbceNFJB0vAzPoICbcpXN2gx0rkOJVJ/lsLaQw1qFyEhv9NpsK55A48AYv0GoLh1gQ2vu5uDBgJFxvOBeTEJKIpWqQLxcA/Fy9UV1U3qPL0ODnNAdP0Z7DqNpmoJ7YBxzhnpwJuUTsvCuK2cg7g51vbAeM5n+yBQG9xqEuscgBvSL5cVAF4pzdtQ9ZSaPI5knwWc6z/n2xd01gnHDInAuTSTJyidjtYlm61/L+1qj/4bj5X489shMAu7zRa2Zz5PeFWScSq57p0BX29di9PZ9CB/RC5Vix/Wsnp85vDMFY3A0bz4fikbdh/BXZzPZp5iknVa+u8dcgFZnRhM5kbCH3PHs2b0+WNAn7SONgcx68ylC1b0IGjWb1yKdyDm4r/a9EoZi9EYXgsN8awME1xBmvTq0ifEFxegugueAEYS63oOi2FkRkJSQtDUDwqayYGwAanUIU159qpkXmLZ0rtkROn8NCbuXM6vVb0Btvm1qG7+WtgnACdV9tWVzsQE7+wdRuwbi1WD8lUNd+T0cFbB1xd01EC9Xb2rvq+aRfPoUDpq5RPv2xV3VlwFDpxNoc4ojhVbeBcGOoLAA0KaQ0qrvXXQFko4lxF2vkC07s5k8aQRBK7Pb6SVX1aRtSUL36Qgi1dtYr2swyXCWHL2CRpdCmhlMB7MxTPEgR9vU42Wb4RDG1J5VJPx4spUpMg8T7N0TO/ueRI1b07BwHNnxDJ/pG3QDg0YQ5pnNli2FrdpSrfMs2b+wFcs5ofbph7MthD62ktAGU4pPzORPR7PAXIad+xSiNK442kJlZS65aTFsPJlen+JgzInhA8dFRD8Sx2Kb2hf4fbJ/PSWtKJGxaC19i1qer3nN1T8L9vVKKlt2x2H32HRmPvcfUJlLxg8xbGrwEri23NfbZdZu4/0tAby2ZD3J9jR6RK/x4Cr+K+513vrdUr6aD0Z9Hklvv82K49Zexb3Mnri/EbZyNn/dNAl7sljx5Fy+uAhc3MXiN9x5a/5U1m1zAWMx2p3v8Of4uhcOmg+zInYn/z1/OQlRZoylxaRt3UumJvyGbfzMnviPCI+Zyrr9z2Fv5SN69Tvf4Y8PvcVb89fwtFJM2pYkDpdOQnVjuGXBuaZSKWC6jL7VuXG3aJvawh3XNt2OQMaPW8mQ+nFvs1jgDRSt5o1vN2NEIXDoal70uXZrrSdTnxkONUf55JP5HL8CZdoY4h3nEzU0juGOUHk5naQ9cRyy9haSMpCnwxQOxyW18nHKoiv4N//AQf9n6cw9etzPpUv/bM/y3BFlABgaNpiUw0c7uxhd5vvoCuXoCmXoSuVoRAll6bb/hxI3nTfa7YVd3YlcGc9rhmWMj23LN7PbEtFvMSsd9zAm5XA7XwG8h/Dl61lgeofxMWkd/xz+u1WH1D/RZTmNYt3+2RgXTuaNg9cCGQvOtbr3loSnvcGEmLTbGDcibdPdxnPSSr6KymL2tI/R/tq/jDuYpGMJ8WtgTuO9tzehV3m049tlL7M3bhWb9d3bOO3BBaUileXa1PZPQVA8UOk38ec18iPfpjqk/okuwymEp58fQai6OyqnXoTPnUio8Rh70xrcSbHkXFOHEqzKYv3fbycAAWmb7jb34EkW78VukgDkDid3QlpJ7oR0vXJ0hTJ0pXIIIUSncArjzQ9nM1rtgcreTGnOMdbHvlP7VnYhhKgjY0KEEEII0XYqDrPsucMs6+xyCCG6NEnHEkIIIYQQQnQoCUKEaGOSiiWEEEIIcWsShAghhBBCCCE6lAQhQgghhBBCiA4lQYgQQgghhBCiQ0kQIoQQQgghhOhQEoQIIYQQQgghOpQEIUIIIYQQQogOJUGIEEIIIYQQokNJECKEEEIIIYToUBKECCGEEEIIITqUBCFCCCGEEEKIDiVBiBBCCCGEEKJDSRAihBBCCCGE6FAShAghhBBCCCE6lAQhQgghhBBCiA4lQYgQQgghhBCiQ0kQIoQQQgghhOhQEoQIIYQQQgghOpQEIUIIIYQQQogOJUGIEEIIIYQQokNJECKEEEIIIYToUBKECCGEEEIIITqUbWcXQIhfI0/16ySGqGv/qExh6v7NHOvMAtkO4tPRT1GUsphFl5ucgRD1NGID/eltZwdlOxmTnEh+R5fzjufPysgZqP53IS8X1XR2YYQQQohOI0GIEJ1Ar1tFgA7c1HM44NfZpQGulpFcpMX4SzPTuwXxSqAvpVmr+P2FMoxXqzF2aAHbkhN+j3zC6w/3JP+HycRlFdd9ruCifpUXhkTg010FpotcyN/Mlh83o7ti3RaGBS9lreMeRh45TGmbl18IIYS480kQIoSAq3l8dDKv+en2Lrh1Kye1tAj9HX4B36HXPKLdKyi+cT/um87Mx5+gOms5y3ecorp7BM89PpeZj1zk/0s6xB2+20IIIUSXIkGIEO3CgWG9n+cPfXzp7WiLyXiehKwvWV5chtnSVdj780a/CUxwc8eZKv5ZUUSC9mP+Yqiqn8Wzx1PEBA5i0L3O8EsJqee2E5N7Gr2l2+jWjw/GTmM4AOVs/7FxOpbiPIGE8Efxqvs7IGIV08DqdCz1qNdYOjeSYFcz+cc3cViZSpTxHYbPO2j599GIioFejxKtdiE1bQNfmSxcTIkgeuiDZH6/Gfdxi3BuMMnWzQ932yw2HkukxAxUbeb7oinM6f4gzhyirMWV2xIx4E+sfcCh7u9nSB73DAD/zF/LyMy8+n21dwzi7aFjmdBdhanyNOtSP2VDxfUw59bHtTaly61gF6VujzLsXhU0sQ4hhBCiK5MgRIh2EOI3h7W9q9iQ8T6L/lWFm/tYYkJnYDy4gncrLFmDLU/8dhrRdqksPBhPxhUHerv407vBHKruk9kQGkB21pdMK7kEqn78od80llevYJqu5S4zAFdP8vKOk/VjQm5kLt/OyB3bwSGCbRGDOHRwuYXlb+ChaJYuicR+5zu88GUhbmNns3S6AklWrgdQOfgzoc+jPOsdgNeVIlKLDpBRbenSHgx4bCYe+TF8ZujJizdMrSk5yYWaKYT6+nI8K48ah+GEuitcyDppQQACUEPi8YUEHG8pHcuOEPVQsjM+JfoXF6b2f54//LY/CUdSKcXS42rHoAceZOGhWOaZXJg66L8arUMIIYTo6iQIEaKtdfNnam8XUv93Ke8W19210H3NBq/FTO3lxdrsIguu/jtw/29sKa/I5lBFGUZAX1TEoQbTx2gGoVz8mHm607Xrq0rk3YJBfNorCB/dD11m0LgmchTBhiRmxx1EawbiP2JP5BqetngNDoR4hDG1z1AiujtQVHqC7Wkr+OZSkVUdbpVmPlGqZNbuz6KGnjfPYPyC+N0ezBy9nvceBTBRfGo5q05mWbEVyxRd2M5fLhUB53lXN5QxGl/8SeWQFcc1W7eHb6pqgEskFBXxh8Br6xBCCCG6PglChGhr9l74/MaBgNDlZIc2nlReqbJwJUaSdad5qd8Mdt+bR0ZZEceKU9l+6VLdgPAe+KvsuN99FhneNyxa6YIbdJEgxA612gOTflttAAJgzkOrM1sehDhFEDt4JF7/SiUm8eu6jreVVJOZOsSDI7sWUXQFsGliHocniAofDjmxrDh1HlTDefKxecwxGVhxNL0Nx4RUU1peUv9X+S/VYGOLPWD5ca3GWHn9/kx5dcN1CCGEEF2fBCFCtIcrZWz6cSlLylu/ivyijxlZ+iARPXwZ1LM/r/z7o4zPWkV07rU7KdVka5cz8ZyFqVd3qoofiDlhx1R1P2IiFvPsxZN8o/uBBEOZxU/osnXth5/jgwQ+k8iohhMe3cwq//dZum0zDsHTCSWZFSnfoQP4KYuNJwbzp4HP4JeWTraVT8i6lVsPYbHsuFo6DEYIIYToiiQIEaKtmYrIr1YR4uYC5S10JK8A3ewaDZBuyGw6z3eF5/mu8AcSgxex1sMfr9wi8rnEaSNM6OGL27muPA6gGp2uGPvIADTKLtLMgOKLRq1AjqXrMHKscDvHCrfj5tyPqX2G8vvBi1lYrSNZl8i6s1pOX731GmoKV7N84yfXP7AZTNTEWThnzuOTrDzKAD9HBZoKNGwUHC0taqPlWrPQnXJchRBCiNsjb0wXoq1dPc2Gc5fo7T+D2F6++Dv0IKR7P1767QzecG0c95v+VUTRb3wZ7+WFj70Kt/oz0oXxARN4qceD+Nur8HEOYoybivKKS3Ud0yoSclMpdZvAmoB+DHRwwd/Zn/G9n2dN7wdROnaPbyln7z4yXYeyYH4YGnUvwma+RKRr69ZVWn6Sd0+uZeSepcw5fR57j0EMsrNgwSvFlPyU1+BfMdVAtbGAEmMZYOZCfhaV3SMYNzgCL5U37p5TiAr2o1KfzBkr74IUlZfBvUGMcXJA6WZrxfG4c46rEEIIcTvkTogQ7SAj933mXJnAH/xnsMnRAX4p49zlbDZUNh5ZYC7fw/IsL2JCFpBg1/BRrtUY6cHUkFnMcXTAvrqc7JI9zNNq61OQjIbN/D61ioWBE/iwjzNcLaf0X+c5kGv5Y4AH/nYxG/q4XP/gkVVMACj9muEpbfSivTObWPy2O0vnv8VXY83oj29irzbAioHpTbhaxqHC7RwqbIsC1qrSxbLuh3lE9ZvHgv4q7EwGLhR8zgc/7rD6xYz5F3axweN5XhmxnIU2Nz+i91ba4rgKIYQQXd2/+QcO+j9LZ+7R434uXfpne5bnjigDwNCwwaQcPtrZxegy30dXKEdXKIOwhDtT/r6eKWfmMT42WzrWQgghxK+QpGMJIdqZO2GTniI8yB2VU3c0Y6cSpSkmaa9ldwaEEEIIcfeRdCwhRLtzC3qKuXNn46YCoz6PpNhlvHfc4rcMCiGEEOIuI0GIEKKdlfBtzCy+jenscgghhBCiq5B0LCGEEEIIIUSHkiBEiDbWo8f9nV0EIYQQQoguTYIQIYQQQgghRIeSIEQIIYQQQgjRoSQIEeJX4R7Cl3/O9kUh7frWbc9JyzmwcQaadtxGu1JCeHPr5/xlxD2dXZK7TMfUv7uaEsrSfQdIP1H776NJ3Tu7RJZ5KJoN+1YS1bOzC9KOHprBV/uWN7+PHdquNH2uqX+3mqO7XiPoTjgBpR1u4O5uO+XpWEL8GgRFMzeshC+iM9rv3RxKKLOm90G7+s/ktNc2runmT2zEDHxyl/OcrsyCBVxQB87kyYcHo77PFUcMXChM5JuUeLKNDb4Rcwbrvyzhq1enEnTwf9C22w7cgk1fwsNfZIi7L+7OKqpzF7LgwKEm5pnLKB9vHK5UcCF/PZ/9uIOSK9dmcMGv3yKig/vioZgxlCSyJWkNmcZOejNLM/XPpddMxvUbzkNuHjjbmDEY0kk+Esd+/fVjanvfOJ57bArBrj2xM+eScWw1m7LSqbq+ljbbV8V5Ap/2yWPaSW2rzhNl7HKS55tZ/Pjb7LV2BUooS3f9hafdbpyQxycTZ7Fal8biUSNZrISydNdSPFtRPsv1YcHWNYQefJVn15y9jfV0J3LmRNwOLuPbi21WuDZjSf2rZxPI4xNXM96tmH1f/wc7DA2mndnEJ9r1vDYzlG9j0m6uOx3ZrnREW3/NXdsOW1JOBRf1q7wwJAKf7iowXeRC/ma2/LgZXX07rODSayZRQyIIvM8VrlzkQl48635MbNB+teRnktYnMevDGUz+ci5f6Np4VzuZ3AkR4q5nR/jzkbhp97K3HTsCrpGTCFeOsfngz+23kTqeXiMZ003LRwWW/PABeBPi05PyvPV8tnseq/Zspvi+ccx5ciZeN8yp37uTNLdwpoywu40SqhjoMYhh9q1Y1MYJ2yvnSTu5nuOXm5rBheDwGKLcy9i9YxbL93xHlfd8XhnSt/6qkkoTw8uhHuhS5rN020oybCN4cfR03FtRHMXBn2c9vFC1YtlazdU/BXeffjgavuOb7xexYvcakk2BjH8ylvBrG7PpS9To+QTXJPLJ5umsOlaMz6MxRKtd6tdy+/vqQETvyUx1drj+UbcejA+YwPjWHL/bpN/7Di9Ne5UX6v+t4At9x5ejTagjmRJWQdKWDugQW82C+tdgXnXofIZQQGWT6/qZpC3HUMInMdq16a21TbvSko5p66/p+u1wa1lQzvumM/PxJ7ArXMnyv09m6Z4dVHnPZeYjw+rbYVvPebw++gmc9V8Qv+NV1u1ZzxG90frinNnFt/m+RE0KuOvuhkgQIsTdThlIZJgTmUnHaEXzZ6HuhEeGwPEU0hr0Nlyfr00BCG2i5fT83WqObv3PVqRuPcjv/R7k3Lk9JF61dJl0duyey2cnd5CpTyW38As+O5JOdffBBN53w6wVx0jSOjE0cuBtNPgujAmawLP3tuJms/kQ+5NWsj8rmQtN9dyU4Qz3ceJMWiyHLuVRoo9n44lcXDXPEGgD4M2Q4H6Qv56NeemUGBLZeiiRcrcIwntYv0f2LiNZ6OfPTRfoLdVs/TOT/eMs1qV8wVFdKjr9d+z//gvO2D5McK/aIMPW8xkGdM9lX1I82T/locuKY3eBEyEPD6c2ZGiLfa0i9eJp3Pxm8mmgL/ff+ygfDp1ASPkJDpksW4Nq0krSTxwgdclA7FVD+e8jdWlTjer3PQRNepMNu77h6IkEknetZOnYPjfVMVNpIWnabLT1/85isKIH7zogmr9u/JqjJw5w9IdPWDcvrNFdE89R/8m6jZ+T/I8E0v/xDQl/f5MpQXVpL0oIb+46QPqJeKb4KGimx9elfyWwbqz1nUH1iBFoDBkk6ZqYNuo1NuyqLcP2tdEs+CCBoytHdGAnq+X6d41tj1d5wSeXLUdym72CbT6eQpophKdHNJMm1ybtSgtabOsVgp9fwvYfEkj/x9dsWBRGMzGTBe6Edri1Wi6nrZsf7rZZJB9LpKSqmDL9Zr4vMuLc/UGcAfBgSGgEdnmxrErZTPaldHL133EoL9WKuyDXlJByMA/P8FEE32VRiAQhQtzlFE0IwUoxOm2Tl9XbaCMBhAZBvja70RVPgzYbvWsfNDf90tkRFOSN8UwG+VZuys1jJGN+c5oN+Zduq8h29grUlFF+06XNn8nJKUYJGoimKzb4boE8YFuMruT61ccyQy7l9t743AfYPIzPfXBBf4qaazMY0rlQ05MH3FyaWmO7sqr+2brgiJFyYwUA7p7eOFbmoqvvUZWh0xdj5xbIA9Bm+2qs0vJhzmH0v+mBl5MLpsI9rCs6T6mly2+dR9/+Ixn09jFMxhT+OGQkffuPpO+k/6lPTVSNeJ2/vhmCccufmTrxVRbvtSN80RIWDGjDK709R7H03alodBt4JXo6Ly3LwC3qLf76uz71s6hUCvqdH/HHl2cyftrbvK/rw9yVrxPuBJgzWPbUSPr2n8kX+WZy1s+s3Y/+Y3hlZ7WVhbmH4FBv0GWQeWMQ9VA0S5dEYn/4HV54bh6rcwbydFNXKjraDfUPAJthRD02mNwf15B9q6DUnI1WB5rQ5q5Wt3+70tK5Zu86lChNBivmzGN2/Fk8o2Yzq5X179fWDt9YzpqSk1yo8SXU17f2zofDcELdFS7kn6QMwOZh/NzM5Jf25LmJX7PixQSWTo7jcU+PVm1fry3E6BpAUPvmYXY4CUKEuNv19MDV/jL57ZnSofLAU2XGUFrR+POcDHLM3gRp7IAApixfwtwBdoAvwWoFXVq2lWkaPZjq54+x4AAJNS3P3SybQYzrF0h5ztccb6IAev1lcHVp57z71rFVXHGgAoPJl/CJCSwfPQ6XyjLKccLZXgHFCWdbE1WVFagHf86KKW8TYGOk3AwOqo4PQiyvfy4ED34C99Lv2FdoBhQcHZ3AVEGlahwvT0tgfpAvlaYKUFxxtqGN9tWBYQFz+PS3XiSfTiX94gG+6jaSD4Y/zxNtlo51D2FjB6LK3MSyv6eRoztL0pr32Kz3IHxs4wGnPlPW1A8+Tz9xgPS1oyy+EuwZ/hShHOO9ZbtIO1OIdt/feG9vBZqxowiqmydn63ss+/Igh7WF6M5k8G38LnJUAYRp2jrtxQN1TwWD/uYOsSZyFMGGJN6LO4hWd5ak+I/Y0+kpZzfWv9rPAh6ZS6B+NTv0FbdcGirQG8woPT2aTV1s93alxXMtm/XLtnFYm83hL7dyuNQFjaY1bcKvrB1uqpzGL4jfnYjDkPW895/JrPvdQtSFy1l7Mqt2uuKCs71CYP9xcCqOVTti2F3qzZOjFzGsFbmtZkMhBlzw6dkZ6WntRwamC3G3s1cAc+tzsu3D2Pj4M/S1qf3z3JkVPJld1HgepbabZLpxI+ZstDkKUUHeKPownh4xEJUphPd1Hmg8i8m08u6MynUk41Xn+Sj1/G3kmPsybPQiBpg3syrlEE39hprMtZ1g6/qgvsQ+PocJjg0++vc4sgGoJvnoQl4uvp1f7Dr1rbaZysoyyisrqEZpsjGvriyjvKqsyX28FbdeczjQ37fB/vuSMG5s7X+NB4j+fhcZlq7MovrnhFe/WF7wymPjtnga1a4rUFNTQVVlGZjNYNP0Glq7r1BFxrlPmWYyYnKewLNXjSSf+5jkAhXObXC4armg8VQw5JxtcHelgBydGTe1NyrSuDbOWb/3HRZ/WUj9RXdjgYV13Q5PtQvoM9DV95erydEWQqQHngpozaA89BRvzZ9IWJA3bvUHuAxdm19tvnb+3Fh6O9RqD0z6bWivTTLnodWZedraTVjSNlmk6frnoJ7Pc54n2bjpkEUpNCbTrduN1rUrVmjhXDMZitHXf+c/YzSCvWL9ge/a7XAtxWMGqYOD6pYtJ+HQYuYZWljImnI6PEFU+HDIiWXFqfOgGs6Tj81jjsnAiqPp1AB22FNV8D4bc1KpAYp+XE+IzyKG9PLgUFaxdcUwmTGhoHTCOLX2JEGIEHc7YwVmkzuuKqCli3lNMZ1k0cG8+h8C0y9NDEI0lmE0KahUCtAwbeMymTnFzNUE4GMIhL170WsGEhzkhNp41sonfaiY4BcEFz9lu4W5+jfzZvDoOMY5JvLBjniKrjQ9l0p1D5hLrBxDo2PtP1bwFUA3L14aPAHn3Pd5t7T2Z6u8sm16tDWVBqrwwNW+gB17/oOjAJ6DcKSCcpMZzBWU19jj4KhQpJ3Ln7SATQThClQZLRtAWnrxS6Yl1o66UHpO5sNeRSxMO4z+KnClvC6wslCL9c8Jr6BY5gSbSdi1iKP1T58xU1lZAY5OOFcl8tnmRADc+zmB+TzlV2iTfQUwmuqOtCmb7y6W1XasaowWp2O1zPKO3rUxIe1CCWHBu7MJ033EGxP3knnxZ8yuE/loV3Q7bKyMUqMZRdWOjxK2pG1qUXP1T8HdOxBXZ1fmvDym0RKjnjlA8Im5/OloVqPPVW5OUFHWbLvRunbFCha09a1uOut19Xa4lvnS10Qn7qz/++Z0r9spp4JX8HRCSWZFynfoAH7KYuOJwfxp4DP4paWTba6gqgYMpQXXA5crFzGYwF3lAlgZhKjcUVGBoVWBVNclQYgQdzlzTjb5hOCjtoOL1uZ1A1SRX9HCdcCKs+j0EPaQN9zQRc05fhZjeAhT6E7m+lXo1Qt4eoSCSrfr5lzxW1CcRzLVrYxvDp5u5Y+4NwNGxxGlSuaDHWvIbXbbdmge8oD8g+RYdZmvBn1FEbWZELa1HdjKIjIq2uxyeq3SLC7URPCAuwv8VNvpcnH1w9lUQP5PwJVc8n+CUZ79sM1KrP0BdO3LA7YXySq1sJNWU0ZGXSdGVVkD1WWcLi+yevwOtFT/FLwCY5kzUOH7HfNJumEEdom+gMr+fqhVkGsEcEHt6UF16WYu0Eb72pDpNBus7Bs0Xt7cTLxRTI4epqj74EZGXR3xRqNWKNUWWFmfq2s3o7pxQ9XodWUQ6Y3aCXIqAOzQBPUCfVLtFfCefdC4lpH09k7S6o6Fou6Dpz3cmMFjwprQqSll5OgqUKk9cKPh+qvR6YqxjwxAo+yqfZCF4otGrWD9s70taJtu6Vb1z8yFI3NZmnn9W7B1e5E5j/uS9V0Mu/V5N6zLm+CeoD9c2MzxbLldUZzccVOB0VBCa56mffttfcu6fjtc56qR0xW3E+7dupyOjgo0FTzZKDgCXMnlwk8wpLsHUFA3zRVXe+sukFyjUnvgSSE5+e1zXDuLjAkR4m538RhpOieCwnzbcSMFJB0vwzMo4Kb8XeELaHMAACAASURBVLM2A53rUCLVZzmsLeSwViEy0hu9NhvLL+o4MMZvEKpLB9jQmrs5eDBgZBwvuBeTkJJIpSoQL9dAvFx9Ud2U3uPL0CAndMeP3dQx6xhOqO6rLZuLDdjZP4jaNRCva2MczMkk51cQGDqfYT18cfecwnP9/TDkfEPWFYA8jmSeBJ/pPOfbF3fXCMYNi8C5NJGkS53woNRm65+Ce2Acc4Z6cCblE7Lwrjsmgbg71Hb8avTfcLzcj8cemUnAfb6oNfN50ruCjFPJdekxXW1fi9Hb9yF8RC9Uih3XM11+5vDOFIzB0bz5fCgadR/CX53NZJ9iknZa+fhacwFanRlN5ETCHnLHs2f3+mBBn7SPNAYy682nCFX3ImjUbF6LdCLn4L7ady0YitEbXQgO860dt+AawqxXhzaRc1+M7iJ4DhhBqOs9KIpdKwKSatIOZmD2GUioU+MpOXv3kek6lAXzw9CoexE28yUiW/+YplayoP6ZCyj5Ka/+X1G5kWrMlBuzKDPfcNR6hhDkWUbawRuDk2taalfsCJ2/hoTdy5nV2re9tntb/2tph1sqp5kL+VlUdo9g3OAIvFTeuHtOISrYj0p9Mmfq2uHk06dw0Mwl2rcv7qq+DBg6nUCbUxwptPZKhx1BYQGgTSGlVd971yV3QoS46xWyZWc2kyeNIGhldju9+KmatC1J6D4dQaR6G+t1DSYZzpKjV9Doah/fazqYjWGKBznaAstX7xDG1J5VJPx4spUpMg8T7N0TO/ueRI1b07BwHNnxDJ/pG3QogkYQ5pnNli2FrdpSrfMs2b+wlcsGMn7cSobUjy2ZxQJvoGg1b3y7GSNlZCbFsCV8Lk+OW48zBvLz4lh3JLX+tr8xJ4YPHBcR/Ugci21qX+D3yf71lLSiNMaitfRtTZp9vebqnxNqn34420LoYysJbbBE8YmZtakuV1LZsjsOu8emM/O5/4DKXDJ+iGFTgxejteW+3i6zdhvvbwngtSXrSbYH8rfybN0TsowHV/Ffca/z1u+W8tV8MOrzSHr7bVYct/bK5mX2xP2NsJWz+eumSdiTxYon5/LFReDiLha/4c5b86eybpsLGIvR7nyHP8fXvXDQfJgVsTv57/nLSYgyYywtJm3rXjI14Tds42f2xH9EeMxU1u1/DnvMpLw9zuonZJkP72KPcQmR4d35dmeD8V9nNrH4bXeWzn+Lr8aa0R/fxF5tgPVjQm6LBfXPCurIEWj0SSxr7nha0K6oVAqYLqNvdR5gO7f1d1w73Fotl7NKF8u6H+YR1W8eC/qrsDMZuFDwOR/8uKP+DlGZNoZ4x/lEDY1juCNUXk4naU8ch6y9QaMM5OkwhcNxSVZcuLsz/Jt/4KD/s3TmHj3u59Klf7Znee6IMgAMDRtMyuGjnV2MLvN9dIVydIUydKVyNKKEsnTb/0OJm84b7fYywe5EroznNcMyxse25cvJbInot5iVjnsYk3K4na+K3UP48vUsML3D+KbefCxap0Pqn+iKPJ9fyVdjs3jpuY9vkW3lzpS/r2fKmXmMj7X2iXldgBLCm9vexG31TP5rX1MP27CgXVFCeHPbSsLT3mBCTFrrx42027km7XBn8Zy0kq+ispg97ePrD3O4S9i43e8VY+nM99xzDz//3KrRPW2mK5QBwLvXAxQW3tblwTbRVb6PrlCOrlCGrlSORq7oycy5go/bz5w8c7nJVNbb9wtntUUo7k4Y0nX81GbrdcNfBcfO/cARk8VvxWodxZtgTSl7Pk5Edzup5qKxDql/oisy5pxF734vNoXZFNankrgTNikcX9t/YahU8ImcztwJDiSv2UDyxXY+x9tDrwB8K1PYsDW36eDBknalz1PMibLhq5iPSbudxrPdzjVphzvHPWgC3cn5ZhMH22mcT2eSOyGtJHdCul45ukIZulI5hBCia3Ln6ZglzA33rh2Irc8jKf49/rzz7K/+qrcQvyYyJkQIIYQQHaiEb2Nm8W1MZ5dDCNGZ5OlYQgghhBBCiA4lQYgQbUxSsYQQQgghbk2CECGEEEIIIUSHkiBECCGEEEII0aEkCBFCCCGEEEJ0KAlChBBCCCGEEB1KghAhhBBCCCFEh5IgRAghhBBCCNGhJAgRQgghhBBCdCgJQoQQQgghhBAdSoIQIYQQQgghRIeSIEQIIYQQQgjRoSQIEUIIIYQQQnQoCUKEEEIIIYQQHUqCECGEEEIIIUSHkiBECCGEEEII0aEkCBFCCCGEEEJ0KAlChBBCCCGEEB1KghAhhBBCCCFEh5IgRAghhBBCCNGhJAgRQgghhBBCdCgJQoQQQgghhBAdSoIQIYQQQgghRIey7ewCCPFr5Kl+ncQQde0flSlM3b+ZY51ZINtBfDr6KYpSFrPocpMzEKKeRmygP73t7KBsJ2OSE8nv6HLe8fxZGTkD1f8u5OWims4ujBBCCNFpJAgRohPodasI0IGbeg4H/Dq7NMDVMpKLtBh/aWZ6tyBeCfSlNGsVv79QhvFqNcYOLeBtUibz2oy5PHTTBBNZ+8ezLq8C7nuCSUPGEeLui7MjVJfnkXlyNVuysqiycnPDgpey1nEPI48cprRt9kAIIYS4q0gQIoSAq3l8dDKv+en2Lrh1Kye1tAj9nXgB35zIZ1+fwqHBR47ec3kl1ExaQQUADm6D8KnJJSnlcy6Ug4vffxD1aBwO5hf5IK+4c8othBBC3KUkCBGiXTgwrPfz/KGPL70dbTEZz5OQ9SXLi8swW7oKe3/e6DeBCW7uOFPFPyuKSNB+zF8M16/Le/Z4ipjAQQy61xl+KSH13HZick+jt3Qb3frxwdhpDAegnO0/Nk7HUpwnkBD+KF51fwdErGIaWJ2OpR71GkvnRhLsaib/+CYOK1OJMr7D8HkHLf8+GlEx0OtRotUupKZt4CtTS/OXUWYoa/C3C4OH+FKdv5zjdQWoyltCXMM47FIZHr3iCfd9GNu8YlqOvWyJGPAn1j5wLdR5huRxzwDwz/y1jMzMq99Xe8cg3h46lgndVZgqT7Mu9VM2VFzfwq2Pa21Kl1vBLkrdHmXYvSpoYh1CCCFEVyZBiBDtIMRvDmt7V7Eh430W/asKN/exxITOwHhwBe9WWLIGW5747TSi7VJZeDCejCsO9Hbxp3eDOVTdJ7MhNIDsrC+ZVnIJVP34Q79pLK9ewTRdWbNrbuTqSV7ecbJ+TMiNzOXbGbljOzhEsC1iEIcOLrew/A08FM3SJZHY73yHF74sxG3sbJZOVyDJyvUAKgd/JvR5lGe9A/C6UkRq0QEyqq1fD6onGO5ZQebu5FsEFwq2NlBuNFgQgADUkHh8IQHHW0rHsiNEPZTsjE+J/sWFqf2f5w+/7U/CkVRKsfS42jHogQdZeCiWeSYXpg76r0brEEIIIbo6eTqWEG2tmz9Te7uQ+r8f827xeU5XXeKQ7ms2XO7BmF5eKBatxIH7f2NLeUU2hyrK0FcVcagokQ31d0EcGKMZhHLxa+bpTpNRVUbGpUTeLSgnpFcQPu23d1bTRI4i2JDEe3EH0erOkhT/EXssvlUD4ECIRwQrhy4mOWIazzqVsT1tBSP3ruDl/z3J6avWl8kr8Al8KpNJ1jd/H8ZF8yJD7E+xLzPd+g20oOjCdv5y6Tyny0/yru483OuLP2DNcc3W7eGbqhq4eomEoqIG6xBCCCG6PrkTIkRbs/fC5zcOBIQuJzu08aTySpWFKzGSrDvNS/1msPvePDLKijhWnMr2S5fqBoT3wF9lx/3us8jwvmHRShfcoIs8ucoOtdoDk34b2mv9fXMeWp2Zpy1dhVMEsYNH4vWvVGISv67teN8Om0E85u/Bhcwd6JqZxcFzHjOHepD1/VwOtfkI/GpKy0vq/yr/pRpsbLEHLD+u1Rgrr9/tKq9uuA4hhBCi65MgRIj2cKWMTT8uZUl561eRX/QxI0sfJKKHL4N69ueVf3+U8VmriM4tqhtbUE22djkTz1mYenWnqviBmBN2TFX3IyZiMc9ePMk3uh9IMJS16gldDr3GE6zkkZDX9EB8B89XeX30cAw/zmW9rn0GpN96CItlx7XFYTBCCCFEFybpWEK0NVMR+dUqQtxcWp71CtDNDudmpptN5/muMJElqX9lXr6R3h7+dYPEL3HaCL17+OLWdiVvB9XodMXYewaguZaHpviiUVuWlFbLyLHC7byavJiRP27n2FUvfj94McmPv84avyD8rWrFPBgQPAg7/XccaSKCcejxKq+MjsCQMp8P8gqsWfHNbFqz0J1yXIUQQojbI0GIEG3t6mk2nLtEb/8ZxPbyxd+hByHd+/HSb2fwhmvjm4+mfxVR9Btfxnt54WOvwq3+jHRhfMAEXurxIP72KnycgxjjpqK84lLdwOMqEnJTKXWbwJqAfgx0cMHf2Z/xvZ9nTe8HLRx30jFy9u4j03UoC+aHoVH3ImzmS0S6tm5dpeUneffkWkbuWcqc0+ex9xjEIDsrVqAax3B3M1mnEm9694et60xeeWocDnnx7CtV8HINrP2najmYvFFReRncG8QYJweUbrZWHI8757gKIYQQt0PSsYRoBxm57zPnygT+4D+DTY4O8EsZ5y5ns6Gy8XgGc/kelmd5EROygAS7ho9yrcZID6aGzGKOowP21eVkl+xhnlZbn4JkNGzm96lVLAycwId9nOFqOaX/Os+BXMsfAzzwt4vZ0KdBJ/uRVUwAKP2a4Slt9KK9M5tY/LY7S+e/xVdjzeiPb2KvNsDyMSFNuVrGocLtHCq0ZiEFdXAED5iPsqXw5kd8OXsPxsfeHh5exIKHG0woWs0b3262KvUr/8IuNng8zysjlrPQ5uZH9N5KWxxXIYQQoqv7N//AQf9n6cw9etzPpUv/bM/y3BFlABgaNpiUw0c7uxhd5vvoCuXoCmUQlnBnyt/XM+XMPMbHZkvHWgghhPgVknQsIUQ7cyds0lOEB7mjcuqOZuxUojTFJO217M6AEEIIIe4+ko4lhGh3bkFPMXfubNxUYNTnkRS7jPeOt+Ytg0IIIYS4G0gQIoRoZyV8GzOLb2M6uxxCCCGE6CokHUsIIYQQQgjRoSQIEaKN9ehxf2cXQQghhBCiS5MgRAghhBBCCNGhJAgRQgghhBBCdCgJQoT4VbiH8OWfs31RSLu+ddtz0nIObJyBph230a6UEN7c+jl/GXFPZ5fkLtMx9e+upoSydN8B0k/U/vtoUvfOLpFlHopmw76VRPXs7IK0o4dm8NW+5c3vY4e2K02fa+rfrebortcIuhNOQGmHG7i72055OpYQvwZB0cwNK+GL6Iz2ezeHEsqs6X3Qrv4zOe21jWu6+RMbMQOf3OU8pyuzYAEX1IEzefLhwajvc8URAxcKE/kmJZ5sY4NvxJzB+i9L+OrVqQQd/B+07bYDt2DTl/DwFxni7ou7s4rq3IUsOHCowQyBjIuOZ1SjPqiJrP1jWJd3bV9c8Ou3iOjgvngoZgwliWxJWkOmsZPezNJM/XPpNZNx/YbzkJsHzjZmDIZ0ko/EsV9//Zja3jeO5x6bQrBrT+zMuWQcW82mrHSqrq+lzfZVcZ7Ap33ymHZS26rzRBm7nOT5ZhY//jZ7rV2BEsrSXX/habcbJ+TxycRZrNalsXjUSBYroSzdtRTPVpTPcn1YsHUNoQdf5dk1Z29jPd2JnDkRt4PL+PZimxWuXajUsSx6YjgO+bHM3/MdNXWf27qO47lHa+ufI0aKS5LZ9+NKjv7U4ACf2cQn2vW8NjOUb2PSbq47HdmudERbf83d3A7Xc8LvkU94/eGe5P8wmbis4tqPHaYw/3ez8Gk07ym2/H0WSfWNk4JLr5lEDYkg8D5XuHKRC3nxrPsxsUH71ZKfSVqfxKwPZzD5y7l8oWuTneoyJAgR4q5nR/jzkbhpP2JvO3YEXCMnEa4c448Hf26/jdTx9BrJmG5a5hVY8sMH4E2IT0/K89bzWclFqmz8GDZ0OnOehNhNayhqMKd+707S5s5myoiPeONga99lomKgRwD2l1M5ZLJyURsnbK+cJ+1kMh7BcwluciYTFzJj2HD6+v5XNfgRV2lieDnUhcyk+Xxw2ZVhj87jxdEVLN8cT4mVxVEc/Jlwr5GE4iKMVi5bq7n6p+Du0w9Hw3d8k5lLidkVv34zGf9kLDWbZpFkBGz6EjV6PsGVn/PJ5kSqPF/kxUdjiK58kfV1nZ7b31cHIno/hVfpLjZd+6hbD8ZrhsK57Xxj7fG7Tfq977D4y0Kub9aMXt+xZWgz6kimhFWQNK0DOsS3w+EJnhvigeEyPNBowqDa+vfT53ywaQfFNr489tgipo6u4EKjduNnkrYcY8FfJjF6TRrfGm7eRNu0Ky3pmLb+mq7fDt8+h17ziHavoLimiYk1F0lLimXf5Wu120xZg+jC1nMer48eTnnWeuJ/yKXGtifujq1oRc/s4tv8SURNCmDzyuyufS5ZSdKxhLjbKQOJDHMiM+lYKzuRluhOeGQIHE8hrUEL6fp8bQpAaBP3kT1/t5qjW/+zFalbD/J7vwc5d24PiVctXSadHbvn8tnJHWTqU8kt/ILPjqRT3X0wgffdMGvFMZK0TgyNHHgbt79dGBM0gWfvbcV1HvMh9ietZH9WMhdu8WtTXV5AkSGr/l9Z/bzeDAnuB/nr2ZiXTokhka2HEil3iyC8h/V7ZO8ykoV+/tx0gd5SzdY/M9k/zmJdyhcc1aWi03/H/u+/4IztwwT3cgHA1vMZBnTPZV9SPNk/5aHLimN3gRMhDw/Hoc32tYrUi6dx85vJp4G+3H/vo3w4dAIh5ScsDiBVk1aSfuIAqUsGYq8ayn8fqUubalS/7yFo0pts2PUNR08kkLxrJUvH9rmpjplKC0nTZqOt/3cWgxW9DtcB0fx149ccPXGAoz98wrp5YY3umniO+k/Wbfyc5H8kkP6Pb0j4+5tMCapLe1FCeHPXAdJPxDPFR0EzPb4u/SuBdWPtLC9EHfWIEWgMGSTpmpg26jU27Kotw/a10Sz4IIGjK0d0QsqJN4Mfm45D5mqOVN4wycGPB5xNZJ1aT66xGONPh9h9Kg+6+/GATeNZzcdTSDOF8PSIZtLk2qRdaUGLbb1C8PNL2P5DAun/+JoNi8JwbfXG7oR2+DYpEUQPfZDMHzY3e0Gj/HJWg3Y4r8EdDg+GhEZglxfLqpTNZF9KJ1f/HYfyUq24C3JNCSkH8/AMH0XwXZaTJUGIEHc5RRNCsFKMTnu5HTcSQGgQ5GsbX6UxaLPRu/ZBc9MvnR1BQd4Yz2SQb+Wm3DxGMuY3p9mQf+m2imxnr0BNGeU3djz4mZycYpSggWi6bINvzwMD32fV7w+wYtrnvDI4Apdrk2wexuc+uKA/VZ9SgiGdCzU9ecDNpenVtSOr6p+tC44YKTdWAODu6Y1jZS66+h5VGTp9MXZugbVXrNtoX41VWj7MOYz+Nz3wcnLBVLiHdUXnKbV0+a3z6Nt/JIPePobJmMIfh4ykb/+R9J30P/WpiaoRr/PXN0MwbvkzUye+yuK9doQvWsKCAdZ37pvVcxRL352KRreBV6Kn89KyDNyi3uKvv+tTP4tKpaDf+RF/fHkm46e9zfu6Psxd+TrhToA5g2VPjaRv/5l8kW8mZ/3M2v3oP4ZXdlp7NfoegkO9QZdB5o1B1EPRLF0Sif3hd3jhuXmszhnI001dqegALoHzedJmM59lFdw8seoU+ZfBx3c4LjaAjS8DfLypLEom98oN85qz0epAExrQTKe5/duVls41e9ehRGkyWDFnHrPjz+IZNZtZrax/d3877MGAx2bikR/H7uauAti6MGTcN6z6/QGWR68m2tf3enqRzcP4uZnJL+3JcxO/ZsWLCSydHMfjnh6tKo1eW4jRNYCg9s3D7HAShAhxt+vpgav9ZfLbM6VD5YGnyoyhtKLx5zkZ5Ji9CdLYAQFMWb6EuQPsAF+C1Qq6NGtvLfdgqp8/xoIDJDR1e9xSNoMY1y+Q8pyvOd5EAfT6y+Dq0s55961VQf6p99m4P4ZVOxaxKbMY9+AYXhnat/YHUHHC2dZEVWUF6sGfs2LK2wTYGCk3g4Oq44MQy+ufC8GDn8C99Dv2FZoBBUdHJzBVUKkax8vTEpgf5EulqQIUV5xtaKN9dWBYwBw+/a0XyadTSb94gK+6jeSD4c/zhP1t7XkD9xA2diCqzE0s+3saObqzJK15j816D8LHNh5w6jNlTf3g8/QTB0hfO8riK8Ge4U8RyjHeW7aLtDOFaPf9jff2VqAZO4qgunlytr7Hsi8PclhbiO5MBt/G7yJHFUCYpg2DIQA8UPdUMOhv7hBrIkcRbEjivbiDaHVnSYr/iD2dkXJ23xReHKiQlLSZphOK0tm6eyW5bgv508vJrHt5PVGOiXywp6n5K9AbzCg9PVA1s7l2b1daPNeyWb9sG4e12Rz+ciuHS13QaFrTJtz97bBKM58oVTIb0rJochev5HLkyBo+27OIVTuWs6/UhSGPx/FC3V1cFBec7RUC+4+DU3Gs2hHD7lJvnhy9iGHNVZBbMBsKMeCCT8+2Pk87l4wJEeJuZ68A5tbnkdqHsfHxZ+hbl35w7swKnswuajyPUttNMt24EXM22hyFqCBvFH0YT48YiMoUwvs6DzSexWRaeXdG5TqS8arzfJR6/jbyYn0ZNnoRA8ybWZVyqMkfGJO5thNsXR/Ul9jH5zDBscFH/x5HNgDVJB9dyMtNJhZbq4BM7Rf1f+kupWNw/JwFmnH4paTXba9WdWUZ5VVlTf+I3oJbrzkc6O/bYP99SRg3tva/xgNEf7+LDEtXZlH9c8KrXywveOWxcVt8o9xwrkBNTQVVlWVgNoNN02to7b5CFRnnPmWayYjJeQLPXjWSfO5jkgtUOLfF4QLABY2ngiHnbIO7KwXk6My4qb1Rkca1YQQ3jQkxFlhY1+3wVLuAPgNd/bWAanK0hRDpgacCWjMoDz3FW/MnEhbkjVv9AS5D1+ZXm6+dPzeW3g612gOTfhva+lT6PLQ6M09buwlL2qZmBfL4Y5OpPjG/dvxRk3wZ9shM/Iybid+fjMHmQYYMe5WXRxcT9+3NKTom063bjda1K1Zo4VwzGYrR13/nP2M0gr1i/YHv2u1wLcVjBqmDg+qWLSfh0GLmNTFWp0mqyUwd4sGRXYsoukLTbY45lUMnr/+pO3Ae2+7reTJ4OA6FO6gC7LCnquB9NuakUgMU/bieEJ9FDOnlwaFrA9wtZTJjQkFpt8rTOSQIEeJuZ6zAbHLHVQVUtDj3zUwnWXQwr/6HwPRLE9cMjWUYTQoqlQI0TNu4TGZOMXM1AfgYAmHvXvSagQQHOaE2nrXySR8qJvgFwcVP2d7qwcLeDB4dxzjHRD7YEV/7A9PUllT3gLnEyjE0Otb+YwVfAXTz4qXBE3DOfZ93S2t/Xssr26xHewMzF0qKIdgVZwUwV1BeY4+Do0KRdi5/0gI2EYQrUGW0bABp6cUvmZZYO+pC6TmZD3sVsTDtMPqrwJXyRoFOi1qsf054BcUyJ9hMwq5FHK0fYG+msrICHJ1wrkrks82J/P/s3X1cVHXe+P/XrnJYlOFKhgTHpCHQEboQTdQWalcWk268lzaVazXKK1zzbhO31H6KXqmVUKbdaKvi2lq23pRiJqYLpZApljC/wPEmJsgBXQa7GMRrDrj7/QNEQJABuVPfz8fDx0PmnPmc95nPmc983ud8PucAeA5wBfVHSq60zL4C2OxVNW3P4fOC4sqOVYXN4eFYjXO8o3d1TkirUIKY98Z0QszreWlcMlkFl1C141i/Z0IrbKyYIpuKomnFWwk70jY1RLmfAE8tfTwTeeehmgsW8NZ/h7NxYyzHPaN4wruY/X9bR5YNIJsdXwYQ9Psneaj7LnZcqN0F13i4Qmlxg+1G89qVJnCgrb/5+yx09Ha4knrh70w4mFT99/XDvRrWWTuA3l3uJeD3Bxlec8Fvt7Gq71qW7txSz5WwPH4qsuHk4YUbcFkt5XIFWIvyriVYVwqw2sFT4w40MQnReKKhFKujidQtQpIQIW5zqimHXILw0TtBQXPuMnKZ3NJGptKVnsVsgZA+3lCni2o6fhZbWBBRdCMrcRUW/TxGDVXQmPdcP1b8BhS3YUz2KObT1JPN/BH3ZuCj8URqDvH+rjWcbnDbThj6eEFuKqYmnearwFJ6jsqREJ0rO7Bl58gsba3k4yqFezy9wP49JSrAaXJ/huG6AXTOPlj5A6jtzz2dC8gucrCTVlFMZlUnRlNWAeXFnCw51+T5O9DY8afQM2A5MwYp/GNXLCl1xl6ft+RR9kBv9Bo4bQNwR6/zorxoGz8BXGmBfa3JfpLNTewb1H6/2kC+UYjJAlF6XzzIrDpGvDHoFYqMeU08nssrN6Opu6FyLOZiiPBG7wqmUgAnDIG9wJJSeQa8hy8GbTEpi5PIqKoLRe+LzhnqjuCx05TUqT7FmMylaPReeFCz/HLM5kKcI/wxKHsqb2Sh+GHQKzT93t4OtE0NUT/ng4++49rgFlce/N0awirWsvKrg5y/Ap27aKpugFBbOQpdrus9edOvB1jS8xuoz8bbFcXVEw8N2Kznac7dtG++rW9cx2+Hq/zLxsnS5kVYkb+aFR9tvPZCpyFEjpuGW9ZcNmafaWDonhde3TSUlxVSAnDlND/9DA928wKq5ht10qJ1btoJkqs0ei905GPKbb87hbUGmRMixO2u4BgZZlcCQ/xacSN5pBwvRhfof934XdWYiVkbSoT+LOnGfNKNChER3liMOTh+UseFx3sPRnPhAJubczUHLwYOi+cPnoXsTTtImSaAntoAemr90Fx3qd2P0EBXzMePXdcxaxuuaO6qjM29Ezg534teG0DPq3McuscQ/ZsohvQajL77YAYOWM4zAe4UmnZxGoAzHMn6DnyimejXH09tOKMfCset6CApF5o/eKLZGjz+FDwD4pkR6sWptI1k411V6VnVggAAIABJREFUJwF4ulR2fyssn3K8pDe/+00M/nf5oTfE8oR3KZnfH6q6w0xH29dCLM6+hA3thUZx4tpIl0ukJ6Vh6zeBhZOCMeh9CZs5nSd9CklJauLta9U8jGYVQ8Q4Qvp4ouvRrTpZsKTsJ4NBTFs4gmB9LwKHT2d2hCum1P2Vz1qwFmKxudMvxK9y3oI2iGkzQ+sZc1+IuQB0A4cSrO2Kojg1IyEpJyM1E9VnEMGutZeYkveTpQ1lXmwIBn0vQmKmEtH82zQ1UynFP5/hfPW/PKwVgFrM+Z8LqQAqzn1DbkVvhodG4X+XN+7acEaHhuNVlk1mUZ1a6xFEoK6YjNQzDWyvsXbFieDYNez9bAXTmvu011Zv6++QdvhKYY3j4gznfy6kHCi35XG+KoHwDJjPHwaMpp+uP3pdOGFhyxntaeV41sHqtunQye9xMcxigl9/PDX9GRgaTUCn7zmS39QzHU4EhviDMY20Zn3uHZdcCRHitpfP9qQcnhw/lMCEnFZ68FM5GdtTMP91KBH6nSSaayyynsVkUTCYK2/fa0/NwRrlhclYz91oGuISwuQel9n71XfNHCJzP/28e+Dk3IPI0WtqBseRXb/nA0uNDkXgUEJ0OWzfnt+sLVX6kSVfzG/mewMYMzqBB6vnlkxjnjdwbjUv7d6GTS3GyTOKSIOWLp2hrOw0pzPi+Oi7E9WX/W2mON7vsoAJv4lnUafKB/ht/CKxyc8IAbCde5v+jg6zr1dDx58rep8BuHWG4N8lEFzjHYXfxvA/32TDlaNs/ywep99FEzPxv6DsNJlfxrG1xoPRWnJfb5Zq3Mna7f7MXpLIIWcgdwdPVd0hy5a6ij/Fz+Hlp5fycSzYLGdIWbyYlcebembzIvvi3yUkYTpvbh2PM9msfGIWWwqAgj0sesmTl2Mn885Od7AVYkx6nVfWVT1wUE1n5fIkXotdwd5IFVtRIRk7kskyhNXZxiX2rVtPWNxk3vliIs6opC0e3eQ7ZKnpe9hnW0JEWDd2J9WY/3VqK4sWe7I09mU+HqliOb6VZKN/0+eEtLbL29j4mTsTQ5/kmYnT6FJhw2r9ho/2JFx3FVcfMRSDJYVlDdWnA+2KRqOA/SKWZo8DbOW2/pZrh1tPuargOSCGZx7U4ISdkovZHPo8jl3517KEYmMc67rEEhkaz8NdoOziCVL2xXO4qRdolEGMClFIj09pwom7W8Mv+gYM/rejK3fvfjcXLvyzNeO5JWIACA0ZQlr6N+0dRof5PDpCHB0hho4URy1KMEt3/hklPpqXWu1hgt2ISFjHbOsyxixvyYeTdSZ8wCISuuzj8bT0Vj4r1pWwFYnMs7/OmPqefCyap02OP9ER6SYl8PHIbKZO3HCD0VaeRG1KJOrUXMYsvwUfxqYEsXDnQjxWx/Cn/fXdbMOBdkUJYuHOBMIyXmJsXEbz54202ndN2uH2ohufwMeR2UyfsuHazRxuE5087u4Z5+jKXbt25dKlJszuaQUdIQYA7173kJ9/U6cHW0RH+Tw6QhwdIYaOFEctVyxkma7g43GJ705dpIF5gDfp/zhrPIfi6Yr1hJmfW6xcD/pq4NgPX3LE7vBTsZpH8aafoYh9Gw5ibuZQc1GPNjn+REdkM53F4vkfdMrP4dpJYk9Cxofh1/l/sZYp+EREM2usC4fWbOZQQSt/x1tDL3/8ytLYvON0/cmDI+2K7whmRHbi47gNZNxM49lq3zVph9tHVwwBnpg+3UpqK83zaU9yJaSZ5EpIx4ujI8TQkeIQQoiOyZNRcUuYFeZdORHbcoaUdW/xStLZO/6stxB3EpkTIoQQQog2dJ7dcdPYHdfecQgh2pPcHUsIIYQQQgjRpiQJEaKFyVAsIYQQQogbkyRECCGEEEII0aYkCRFCCCGEEEK0KUlChBBCCCGEEG1KkhAhhBBCCCFEm5IkRAghhBBCCNGmJAkRQgghhBBCtClJQoQQQgghhBBtSpIQIYQQQgghRJuSJEQIIYQQQgjRpiQJEUIIIYQQQrQpSUKEEEIIIYQQbUqSECGEEEIIIUSbkiRECCGEEEII0aYkCRFCCCGEEEK0KUlChBBCCCGEEG1KkhAhhBBCCCFEm5IkRAghhBBCCNGmJAkRQgghhBBCtClJQoQQQgghhBBtSpIQIYQQQgghRJuSJEQIIYQQQgjRpjq3dwBC3Il0+jkcDNJX/lGWxuQvtnGsXSMSQgghhGg7koQI0Q4s5lX4m8FDP4MDvds7GiGEEEKItiXDsYQQQgghhBBtSq6ECNEqXHjovkm84OvHfV06Y7f9yN7sD1lRWIzqaBHOfXlpwFjGenjixmX+WXqOvcYNvGq9XL2KrvsI4gIGM/g/3OD/znP0h0+IO30SS6vskxBCCCFEy5AkRIhWENR7Bm/fd5nNmWtZ8L+X8fAcSVzws9hSV/JGqSMldOax/5zCBKejzE9dR+YVF+5z78t9NdbQdHuSzcH+5GR/yJTzF0AzgBcGTGFF+UqmmItbac+EEEIIIW6eDMcSoqX9si+T73Pn6P+/gTcKf+Tk5QscNv+dzRe783ivnigOFeLC3b/qTElpDodLi7FcPsfhcwfZXH0VxIXHDYNRCv7OXPNJMi8Xk3nhIG/klRDUKxCf1ts7IYQQQoibJldChGhpzj3x+ZUL/sEryAmuvaikTONgITYOmU8ydcCzfPYfZ8gsPsexwqN8cuECNgC601fjxN2e08j0rvPWMnc8gNyb3Q8hhBBCiFYiSYgQreFKMVu/WsqSkuYXkXtuA8OK7iW8ux+DezzA87/+LWOyVzHh9LmqeSXl5BhXMO4HGXolhBBCiFuLDMcSoqXZz5FbriHIw73xVa8Av3TCrYHlqv1HPs8/yJKjbzI318Z9Xn3pCcAFTtrgvu5+eLRc5EIIIYQQbUKSECFa2r9OsvmHC9zX91mW9/Kjr0t3groNYOp/PstL2toXH+3/e45zv/JjTM+e+Dhr8Kj+Rrozxn8sU7vfS19nDT5ugTzuoaGk9AJFAFxm7+mjFHmMZY3/AAa5uNPXrS9j7pvEmvvudXDeiRBCCCFE+5DhWEK0gszTa5lxZSwv9H2WrV1c4P+K+eFiDpvLKmqtp5bsY0V2T+KC5rHXCf6Z+zbDss6gUo6N7kwOmsaMLi44l5eQc34fc43GqjkhYLNu47+PXmZ+wFj+4usG/yqh6H9/5MDpJtwGWAghhBCiHfyib8Dgfzu6cvfud3Phwj9bM55bIgaA0JAhpKV/095hdJjPoyPE0RFiEEIIIYQQjZPhWEIIIYQQQog2JUmIEEIIIYQQok1JEiKEEEIIIYRoU5KECCGEEEIIIdqUJCFCtLDu3e9u7xCEEEIIITo0SUKEEEIIIYQQbUqSECGEEEIIIUSbkiREiDtCV8JW/I1PFgS16tPUdeNXcOCjZzG04jZalRLEwh1/49WhXds7kttM2xx/tzUlmKX7D3Di28p/68d3a++IHNNnApv3JxDZo70DaUV9nuXj/Ssa3sc2bVfq/67pn17NN3tmE3grfAGlHa7h9m475YnpQtwJAicwK+Q8WyZktt7T1JVgpkX7Ylz9CqbW2sZVv+zL8vBn8Tm9gonmYgfe4I4+IIYn7h+C/i4tXbDyU/5BPk1bR46txieiZpL44Xk+njmZwNT3MLbaDtxAp/6EhT3Dg55+eLppKD89n3kHDtezziyG+3jjcqWUn3IT+eCrXZy/cnUFd3oPWMCEfv3xUlSs5w+yPWUNWbZWq/0ba+D4c+8Vw+gBD9PHwwu3TipW6wkOHYnnC8u1Ou1812gm/i6KftoeOKmnyTy2mq3ZJ7h8rZQW21fFbSx/9T3DlO+MzfqeKCNXcChWZdEji0luagFKMEv3vMooj7oLzrBx3DRWmzNYNHwYi5Rglu5Ziq4Z8TnOl3k71hCcOpOn1py9iXK6EREzDo/UZewuaLHgWogfjzyZyJjrPm8oOzmfeSmHoVMAQx58ht/pA/B000DZj5w+s4XtRz6v8V0DTm1lozGR2THB7I7LuP7Yact2pS3a+qtu53YYBfdeMUQ+GE7AXVq4UsBPZ9bxzlcHK9ueux5j/IOjCfL0w60LlJecIeu71WzPzq7RNjVShkMukZKYwrS/PMuTH85ii7nFd7RdyZUQIW57ToRNisDDmExyK3YEtBHjCVOOsS31UuttpIqu5zAe/6WR9XmO/PABeBPk04OSM4l88NlcVu3bRuFdo5nxRAw966xpSU4iwyOMqKFONxGhhkFeg3nIuRlv7eRK5ys/kvFdIscv1reCO/3C4oj0LOazXdNYse9zLnvH8vyD/avPKmkMcTwX7IU5LZalOxPI7BzOM49G49mMcBSXvjzl1RNNM95bqaHjT8HTZwBdrJ/z6T8WsPKzNRyyBzDmieWEXd1Yp/5EPhpLv4qDbNwWzapjhfj8No4JevfqUm5+X10Iv+9JJru5XHvpl90Z4z+WMc2pv5tkSX6dqVNm8ofqfyvZYmn7OFqEPoKokFJStrdBh7jJ8jjyRQzL/17j355P+anCRvaZE5WrdBlAkNZG9ndrWLdjLuuOfI9bwALm/OahOmdwL5Gy/RhK2Hge1da/tZZpVxrTNm39VR2/HW6+zrq5zHn0MdwsW1i3aybv7EvkiMVWvdzFYzA+FadJSYvjnR1xbDdDv9/G8wc/L4fLcNipPezO9SNyvP9tdzVEroQIcbtTBhER4krW6mM0o/lzUDfCIoLg+Ctk1OhtaCetZu+kszw/7q1arwPonl7NJyNzmDz+vSZeObmX/+59Lz/8sJKD/3L0PSfY9dmsGn8fJffIAIIeG0LAXWs493ONRaXHSDG6Mi9iEEpqejM7T+48HjgWj8xvOXyhomlvVQ/zRcphwIuwvrPoV3e58jAP+7hy6qvlHL5QDJzho2+H8D+Dfk/AkRNkXfHmwX4DIDeOj86coALYcbg/QePDCeueyNYLTdsjZ/dhzL8vh6OF55p3/DR4/KnkfDWNnBqvmC+6EvD0LPr1ciclu5jOut8zsNtp9n+2jhwb8HM8n/n8nYn3P4yLeReXaYl9vczRgpP8d0AMf3Vy4u5f9eQvoeX88MM+3rE7toua8QkcWhhU/fdrRw7wGkDuDp6qPr67Ejh+DvOiB2HQKaiWHFLWvcsrSWdrHWP2onwyjDk0l3bgBF6OHUeIwR1seWTsXs+yhHSu5jG64X9kYXQo/fTuaFCxmI6xJWEVW4yXKofB7EzgyauXWXzWcSIaQCVt8WieTypvUiz6oUMxWDNZba5n2fDZLJ0VQT+tSu7xraQrk4m0vc7Dc1PbKGFRsf2cXeuYdNfF4ll2iO35pZUv2Lbw/u4aK1w4QYXnEOb4PIwPhzlds7TjaWTYX2bU0G7s3lHP2YMWaVca0Whbr9Bv0hKWRg/CRyklK2kVf1qejrVZG7sV2uHm8uLB4HCczsSxKu0wlS34iVr1ffnMEuLP1HjhQjFevdYR5nc/nc8UUuFAGY47T1rqGWZFDqffmpzrfktvZXIlRIjbnGIIop9SiNlY72n1FtqIP8GBkGvMqfVjYTXmYNH6Yrju7KATgYHe2E5lktvETXl4DePxX51kc+6FmwrZyVmBimJKyuouuYTJVIgSOAhDRzzt5BHAPZ0LMZ+/dvax2HqaEmdvfO4COt2Pz13wk+V7qtMf6wl+qujBPR7u9ZXYqpp0/HV2pws2SmyVnUBPnTddyk5jru5RFWO2FOLkEcA90GL7arts5C+mdCy/6k5PV3fs+ft459yPFDn6/h1z6f/AMAYvPobdlsaLDw6j/wPD6F8jwdYMncObC4OwbX+FyeNmsijZibAFS5g3sAXP9PYYztI3JmMwb+b5CdFMXZaJR+TLvPm0b/UqGo2CJWk9Lz4Xw5gpi1lr9mVWwhzCXAE1k2UjhtH/gRi25KqYEmMq9+OBx5ucgEBX+gV7gzmTrLqdpj4TWLokAuf01/nDxLmsNg1iVHB7f9kCCLvfG+uZXTfsKHZWFCgrvr7jruZgNIMhuKGz1a3frjT2XXPWhhJpyGTljLlMX3cWXeR0pjXz+Lut2+FO99PbQyW3qAcTx/2dlc/sZemT8Tyi87rBmxQ6d4ISm7WyLWpWGQ2zGPOxaf0JbN1xmG1OkhAhbnc9vNA6XyS3NYd0aLzQaVSsRaW1XzdlYlK9CTQ4Af5ErVjCrIFOgB/99ArmjJwmnuHqzuTefbHlHWBvEy8w1NJpMKMHBFBi+jvH6wnAYrkIWvdWHnffPJ0VLS6UYrX7ETZuLyseHY17WTEluOLmrIDiiltnO5fLStEP+Rsroxbj38lGiQoumrZPQhw//tzpN+QxPIs+Z3++Cih06eIK9lLKNKN5bspeYgP9KLOXgqLFrRMttK8uPOQ/g7/+Z08OnTzKiYIDfPzLYbz/8CQea7HhWF0JGTkITdZWlm3KwGQ+S8qat9hm8SJsZO0Jpz5Ra6onn5/49gAn3h7u8BAMXdgIgjnGW8v2kHEqH+P+d3kruRTDyOEEVq1j2vEWyz5MJd2Yj/lUJrvX7cGk8SfE0NLDXrzQ91CwWq7vEBsihtPPmsJb8akYzWdJWbeefe085Kyz7kmCNXkcyc5ueKW7onnCRyXzu11cPwCpFItVRenh1eDQxVZvVxr9ruWQuGwn6cYc0j/cQXqROwZDc9qE27wdVtxxc1YIeGA0fB/Pql1xfFbkzROPLuChBirX3fAMDzp/z/6sE80u40ZUaz5W3PHp0T7D01qLDMcS4nbnrABq8y9nO4fw0SO/p3+nyj9/OLWSJ3LO1V5Hqewm2etuRM3BaFKIDPRGsYQwauggNPYg1pq9MOgKyWri1RmNdhhjND+y/uiPN3F53o+HHl3AQHVbjcvktdnVyk5w0/qgfix/ZAZju9R46dfxVcONyjn0zXyeK7yZX+wq1a22SllZMSVlpZSj1NuYl5cVU3K5uN59vBGPXjM48IBfjf33Y+/okZX/tR1gwj/2kOloYQ4df670HLCcP/Q8w0c711Hr6LoCFRWlXC4rBlWFTvWX0Nx9hctk/vBXptht2N3G8tS/bBz6YQOH8jS4tUB1VXLHoFOwms7WuLqSh8ms4qH3RkNG9Zl1S/LrLPown+qRYLY8B491J3R6d7BkYq4+F1COyZgPEV7oFDCqoPQZUTlcK9Abj+oKLsbc4mebr35/6kbvhF7vhd2yE+PVReoZjGaVUU3dhCNtk0PcCeo3BJfziRxpaMyhSzjRT4zGKTuOD84U1ruK3X7jdqN57UoTNPJds1sLsVR/5pew2cBZaXrFd+x2uJLi9SxHhwRWvbeEvYcXMbcJ486ccOZy3lo+Mh2lAjj3VSJBPgt4sJcXh7Nr17+Lbi4xoV5k/2MWh23NK6NRdhU7Cko7zFNrTZKECHG7s5Wi2j3RaoDSRte+nv07FqSeqf4hsP9fPZMQbcXY7AoajQLUHLZxkSxTIbMM/vhYAyA5GYthEP0CXdHbzjbxTh8axvYOhIK/8omDY/Wv582QR+MZ3eUg7+9ax7kr9a+l0XQF9XwT50CYefvrlXwM8MueTB0yFrfTa3mjqPLntaSsZXq0FWVWLuOF1jmPXfv+i28AdIPpQikldhXUUkoqnHHponDOOIv/MQKdwglT4LLNsQmkRQUfMuVg5URtpceT/KXXOeZnpGP5F3ClhCbNWGj0+HOlZ+ByZvRT2btnAd9U3yVHpaysFLq44nb5IB9sOwiA5wBXUH+k5Aotsq8ANntVTdtz+LyguLJjVWFzeDhW4xzv6N3snJAbhxHEvDemE2Jez0vjkskquISqHcf6PRNaYWPFFNlUFE0r3krYkbbJES7hPNQLslMO1v+ddwnnD6Pnco8lgVVpRxu8s5HGwxVKixtsN5rXrjSBA219s5vOah29Ha6kXvg7Ew4mVf99/XCvG725lMsVYC3Ku5YcXSnAagdPjTtwLYFw0c1kzqMPY/1qFonmwmaV4RCNJxpKsTZvAk+HJcOxhLjNqaYccvHCR9/cy7iXyS29wMmqf7kV9XSmS89itoCuj/d1i0zHz2LTBxEV0o2spJ2k2f0ZNdQXTX1jxW9AcRvGZI9iPj19spk/4t4MfDSeSM0h3t+1htMNbtsJQx8vyM3B1KTTfBVYSs+RWXqOzJLzFAElZVV/l54j1+HJm40oyuanCi/u8bw2jMJd2xs3ex65PwNXTpP7M9yjG3DtLJO2P/d0LuCnIgc7aRXF1XGfLKuA8mJOllTty2Vbk85+3vj4U+gZsJwZgxT+sWcBKdbaJZ+35FHWpTf66uEL7uh1XpQXZfMTLbSvNdlPsrnwJsa429UG8o1CTBbQ6n25dkdYbwx6hSJzXhOP5/LKzWjqbqgci7kYdN7oXa++5oQhsBdYqs6Aa30xaItJSUwio+ASKqDofdHVc3bVTlNSp/oUYzKXounhRe274JZjNhfirPO/NtZf8cOgb87WHGibHODZ9zF81G84lFvPMePyEH8YPRf9+QRWNZSkAOBNvx5gyc1vYJ3G2xXF1RNdD0+uq1oH3Xxb37iO3w5X+Zet+rg4WXqh8gSKo66c5qefQdutxvyNTlq0zrVPbrh0n8nzj4ZjTYvl/TN5zSrDURq9FzryMeU2dW5WxyZJiBC3u4JjZJhdCQzxa8WN5JFyvBhdoP9143dVYyZmbSgR+rOkG/NJNypERHhjMeY04a4sLjzeezCaCwfY3JyrOXgxcFg8f/AsZG/aQco0AfTUBtBT64fmuuE9foQGumI+foz2GabuiuauytjcO4GT873otQH0vDrHQT3EodxSAoJjeai7H566KCY+0Bur6VOyrwCc4UjWd+ATzUS//nhqwxn9UDhuRQdJaeKdsVpEg8efgmdAPDNCvTiVtpFsvKvqJABPl8peWIXlU46X9OZ3v4nB/y4/9IZYnvAuJfP7Q1VnozvavhZicfYlbGgvNIoT10a6XCI9KQ1bvwksnBSMQe9L2MzpPOlTSEpSE29fq+ZhNKsYIsYR0scTXY9u1cmCJWU/GQxi2sIRBOt7ETh8OrMjXDGl7q981oK1EIvNnX4hfpXzFrRBTJsZWs+Y+0LMBaAbOJRgbVcUxakZCUk5GamZqD6DCHatvcSUvJ8sbSjzYkMw6HsREjOViAZubdv6+vNQQG9Kcj8np+4ZeeUhJoyOo5/6ObuyCnDTXm03vHGpW0yPIAJ1xWSknqm7pEpj7YoTwbFr2PvZCqY192mvrd7W3ynt8BkOnfweF8MsJvj1x1PTn4Gh0QR0+p4j+ZVXMDprY3h+xGhczqxjf5FS3XZVt9MOlOE4JwJD/MGYRlqzPveOS4ZjCXHby2d7Ug5Pjh9KYEJOKz34qZyM7SmY/zqUCP1OEs01FlnPYrIoGMxpZKhgT83BGuWFyZjXUGHXcwlhco/L7P3qu2YOkbmfft49cHLuQeToNTWD48iu3/OBpUY3MHAoIboctm/Pb9aWKv3Iki/mN/O9AYwZncCD1XNLpjHPGzi3mpd2b8NGMVkpcWwPm8UToxNxw0rumXjeOXK0+rK/zRTH+10WMOE38SzqVPkAv41fJHK+GdHYzr1N/+YMs6/W0PHnit5nAG6dIfh3CQTXeEfhtzH8zzfZcOUo2z+Lx+l30cRM/C8oO03ml3FsrfFgtJbc15ulGneydrs/s5ckcsiZWrfotaWu4k/xc3j56aV8HAs2yxlSFi9m5fGmntm8yL74dwlJmM6bW8fjTDYrn5jFlgKgYA+LXvLk5djJvLPTHWyFGJNe55V1VQ8cVNNZuTyJ12JXsDdSxVZUSMaOZLIMYXW2cYl969YTFjeZd76YiHMzb9Grpu9hn20JEWHd2J1UY/7Xqa0sWuzJ0tiX+XikiuX4VpKN/k2fE9ICOvcaTbDbjxz5/uj1C+8aQkA3Z7rwe2J+//trr1d8w8aNsRyvkbToI4ZisKSwrKH6dKBd0WgUsF/E0uxxgK3c1t9y7XDzFRvjWNcllsjQeB7uAmUXT5CyL756zoeb9xB8nJ3h/gXMu7/GG6vb6cbLcJgyiFEhCunxKc28nXLH9Yu+AYP/7ejK3bvfzYUL/2zNeG6JGABCQ4aQlv5Ne4fRYT6PjhBHR4ihI8VRixLM0p1/RomP5qVWe5hgNyIS1jHbuowxy1vy4WSdCR+wiIQu+3g8Lb2Vz4p1JWxFIvPsrzOmvicfi+Zpk+NPdES6SQl8PDKbqRM33OB5QJ5EbUok6tRcxixv6h3zOgAliIU7F+KxOoY/7a/vZhsOtCtVz2gJy3iJsXEZzZ830mrfNWmH24tufAIfR2YzfcqGazdzuE108ri7Z5yjK3ft2pVLl5oyu6fldYQYALx73UN+/k2dHmwRHeXz6AhxdIQYOlIctVyxkGW6go/HJb47dZEG5gHepP/jrPEciqcr1hNmfm78DQ7yoK8Gjv3wJUfsLTWxogGKN/0MRezbcBBzQ7NPRdO1yfEnOiKb6SwWz/+gU34O+dVDSTwJGR+GX+f/xVqm4BMRzayxLhxas5lDBa38HW8NvfzxK0tj847T9ScPjrQrviOYEdmJj+M2kHEzjWerfdekHW4fXTEEeGL6dCupBbfXfBCQKyHNJldCOl4cHSGGjhSHEEJ0TJ6MilvCrDBvPDRVQ9PWvXXd0+OFELc3mRMihBBCiDZ0nt1x09gd195xCCHak9wdSwghhBBCCNGmJAkRLaIjDD/qCDFAx4lDCCGEEKKjkiRECCGEEEII0aYkCRFCCCGEEEK0KUlCRAvrzgsPr+Sz3t3bOxAhhBBCCNFByd2xRAu7TGaREeV/7/ibewshhBBCiAZIEiJamI2DOZs52N5hCCGEEEKIDkuSENFC3Jn68CLmulf+9UP2cp44faHG8s6MGbyC+XzJxwQyxsMdtysXOJi5gbmFxe0SsRBCCCGEaB8yJ0S0kGLWH5qD/67l/KW4vMG13Lr7ozn7NsP2zmfKDxAeNIJwOQqFEEIIIe4o0v0TbcqWpUf9AAAgAElEQVRelMY7VhsqFWT+9C0/OPVkUJf2jkoIIYQQQrQlSUJEm7Lbiym5+se/KrDjhLMchUIIIYQQdxTp/gkhhBBCCCHalCQhQgghhBBCiDYlSYgQQgghhBCiTcktekWLULpP5tCvH8Dt6gvuC8gJAK4YeWHvBj7/VzsGJ4QQQgghOpRf9A0Y/G9HV+7e/W4uXPhna8ZzS8QAEBoyhLT0b9o7jA7zeQghhBBCCOEoGY4lhBBCCCGEaFOShAghhBBCCCHalCQhQgghhBBCiDYlSYgQQgghhBCiTUkSIoQQQgghhGhTkoQIIYQQQggh2pQkIUIIIYQQQog2JUmIaHPK8CUc+noJEUrt13WTVnPi2wOV//bMI1ip//3i1qIEzmbv12uJ1rd3JE2gBLFwx994dXjX9o5EiA7KieAFf+ObTePQtncoN6Abn8CBTRMwtHcgQojryBPTRQtyIiRuC2/qNvP4c3uwNrRaQSZpqWCp87Llw1n0/xC0k1azd1LrRanRxTAx9DECPLRgLyD3zBa2pu3i/JVr63S+6zEiQ6MI0t2LGzas5z/ng8/WcPpKw+XW3kg4438TzYO6e+mCjULLQXZ9tYYsm3ptnU4BPPSbWQz38UPbCUpsJ9j/xQJSrGrD5TaDMnIFh2JVFj2ymOSWLbqSNojo2Gd5MsQPnQZsljyy0neycvl+zK2wuRtpqX3VRz7Lo2oKU/dfarngarhxnJ6EzZzOtJFB6D0UKCrGdCqNxGXvkVLgaBk3TzdpNXtjA6573ZayiPC56bTGoSTa4PvqEEfa8nKKjMdIseV16GPBkrSZlOiFTBuZzJ+SLrZ3OEKIGiQJEW1ONe7kpfnttHGX0Ux+9Encc9ewat93XHYZwuOPzOT50AKWfnWUCgAlnD+MnotP0S4+3RfP+SuuaO/SUObwRvx45JH5PNzpczbuXMBPV7wZGBrLM4+WsmLbOs4D4M1Dj8YTqTnBZ1/FcbpExa3bvXClI/+c18eTyBVLmKbNZO3i+WRYwUPvT9jQXngoYL7VdgdA8efJ8X7k7liGqR02b5i5hFcjIXn167xy6iLOWl9CIvzRaYCCRt/esuzZbJzxHik167GDdzpF2zEnvcVL7R1EY9RMtqeUsnl8BPqkrW1+YkQI0TBJQkQLcCIsYRtvhrlW/T2Hg9/OAaBo90s8HpeBCihDF3LwjTA0APY0XvxtM870ufoTtfCPRIX4oVNULKY0Ni5fxfZT5Q69vXPPh+ntfIbtabsqO8i2PLZmPMyK0DH0TjtKzhXwvD+aIPvnrNq3BnPVlQ9z3cs2N+IygCBPyP58DVlWFcjji6/uJ3hCOA91T2THBRW6RzFcV8iurQtIsVW978LRJmwEoCuBk+bw8qRB6HUK2Ioxp29l0fw9mADN+AQOLQyqXvu1Iwd4DSB3B0+Nfw8ToBv+RxZGh9JP744GFYvpGFsSVrHFWOMKgOsI1n8xmaL49dgiJvNooBeKWkjKshheSg8iLFAhY/nrJKZWvceYSUrS9dF6DPwj69+OIFCnYM3YwaIXNpBRenUbjdWrE7qhU1k4M4xgH3coyiMj6V2WrcnA4uC+OkoJHE6YLo9t6eevX+gaRPSS2TwT4o1iyyY5KZ/ASb6kTJnG6lNV7+8znHmxlZ+ThmJM6TtZuXgrGaWOxNmLkBBvbCmLeGVHRlVnP4eM1D3V72mTeq2+AnQJszEHY0Pf0xuWoRAycw7TIvzx0bqjqMWYM5J5a/kG0q2AMpx3vpyKxliMT6AXluStZOjGERkIxnXzmb7pbOX+3+R3vlI3Qp6ew+zIIPQ6BdWSR8b291i0KRMbgNKLUbGVseoUKDKnkbj86uflSfSmRJ4kB7uPP5rcJLacCuKZkd7Y0lcxfe5+zMpQ3vziz2hTklGDQzFoFGy5abwVt4pkc2Wc+qdX83HkWaaOe6vq8/Rn3o4EgpNieGpTvoPHcNV+TBqE3gNUSw77Vq9i5f78a4mhQ/XaEEfacicC567jgyjvylWy3iX86Z3Xrpa0Zb06uK+5+49hjRxKmH4riWbHixdCtC6ZEyJaQDkpc8fQ/4HHmL67GHvGKsIfGEb/B4YxLC6j+sdRTV3Gww8MY/BLadhuWF5DehH1xgqm9TjL2hdmMmbKYjYW+DPvjTmENGX+SIVKzZ+58isqOHtzjwbAnd69vCg5n4dP2EZWPLOXlVFriQ7oj4vDG1AAlfKaQ7cqSinv3IN7ulX+uHv2vB+3n7+nPCCe/2/KXlZO+TuzfzMaz05N2I8+E3h5ZhBFHy7mqSdimPzCu+y2XOst2nbMpf8Dwxi8+Bh2WxovPlhZJ/1rdMo1GgVL0npefC6GMVMWs9bsy6yEOVT3Qaq5EhozArYvZsxvRzN2xnpSrIBaik1V8BkYhO5GdeDszaiRruxePJepLyRRZBjPvOheVQsbr1fNr6ezfkUoatIqpo6LZuryNJxHvszSSb0c3ldHeQT6orWdxXRd4tmVsIULmR14nrUvxPDU/P0oQ8Pwca6xinYor709nWDrVl6cEs2Y594lSzeBV5cMRetQnKXYbKAxDKLfdXWAw/t60/XaJA2VoeChlJKy7nWmT4nmqRnvkqUbyWtLRtSYQ+AKxveYviYH3agJ6FMXM3VdIYbIEfRToGW+804Ezk3gzRhPTImvMHlcDNOX7adI61l5QgQngmcuYWGEE/sWz2TMlEVssQYxr9bnpaApTWPRCzuwGMYTpdnJ1BeSsIWMY5T+2jr9QtzZ/d9RPPzITNZaB7F0xWSH5yI0Xq9OBM5cwZuTXEmLn8/kcTN58cNSQpcsYVqgk4N10hhH2vJyjAnP0P+Bx5i6vRB7veW0Rb06vq+qKQeT6k1woMzxEqIjkSsh4pahDBxHVGAeGye+xW5z5WvmZVsJ+2Iqo0KcSE9t/Axaxfnv+IloHh7Qn6xvTnDZZTBP9AvACRWtogBeaLs44+YRxUDTOtbt+hEX3TNM/O1yJpT9F4nm4sYDvfw9uRc1PNjvSXpatnAOb/oNeox7gPNd3IFS3DSuON0VzvCyXWzfs5ESTTijfxfL89QYFtbY56F1R+tczL70TMwFQEE+puMOvLEG0463WFb9Vz7mdXuIjBhHiMGJlOM1Pk9nhaLt77Fs/9nKv42pVXN6jrE2PoU3Fyzlk7BCTMYcjOkpbNudjrm05pZKSdu0it3Hy4H1bMkYyVKDLwr50Gi9uhIRFYGS+govbqqai2DewFsDQ1k/PBT9hy05xMIJvd4LZ2smlrpn/10HMWqoKxlrVrHl6/PAWVZuCiVsSbfqVfQjJxBiS2bq4j1VZ7vzWbl6EGFvRBDqmsruUhpxkd1r1hO2Yirrvwgj13QW4/E09m1PJr3A8TPEN1+vV5cPYumRAyytfqGQbVOiWWZ0pIxL7E5YWaOwfFZ+GMGo2CD6KXtIAaAYU2o2RtsZLPZuZKTnYNLkYIvxRkfLfOdRQnlmlBfmD2NYtCO/qpB8jF9fXR7EqAhvLNtjWJ1auQ+Jy7YStnMqkWFdq67qqVgyjmE0umOyjkfNSMN0vJRcWxgePZyqJ7jlJm1md0E5kM/uTSlEvx/KqMANrDQ2Hmbj+zGIqEgvMuKjWb2/am6D+V22DE8kargfa405166GNFavra4N6vUqh/b1IkU2CNZ7AWdvdueEEC1EkhBxy9DovdE5BzB75wFm11mWoVUAB364bNv44Kv7eS50DfEPABUFZGYd4iftgFqrOZUd5KO0zzkHYI1nv8/fiOw7hM7mzx1IEE6w6x9r8XokmgXPTQNsFJ78nMyLXnhdXaUT0DmPlH+sI+syYM1m6/dDWNQ3HJ+vjnK68T1BNe4n2bSC2X/9G2HGHEymYyRvTyGjCZ1Vpc8IXo4dR0igNx7VZ/SLMdc9G2kvxXT8TD0llGNOWsbYlM2EhA0ieOAgQp9+mchJKbw0ZeW1M5L2Ykzma3GpailoXHEGlMbqVfHGoFfw0C3laESdFSyeLT73xNkZoJ5hKzpfdBSTZbqWiNrMeVjtV5MQJwyBXjj7+PHBkfF13nwGrQfQaBJSOWfq+XEpGEIGETIwiOChU3l30gi2zJjLyuOOTZS/+Xq9urzunBAVq6nO8dVgGU7oh09nXkwowTr3qs8VsB+rugJRWZ5qLwf7JeyoqHZQFbCjgNJC33m9L3qlmIz0wvqXK17oNCpmU96116xnMVmVqk5rZaXZ1VJQXVHtYFfVythRUKo/XxWLuUYZuflYGYmuhxMYmzJ0rKH98MdH44phyTZOLKm9yFbQrfYLjdVrq2uDer3KkX1VK2NwVpSq69RCiI5AkhBxS7Hb0njpkcW1J8o2icp50wL+x+SKRuNORVkelz3ns+KBUs5fVoFiSlTAnse1GQHFWMtUnNzccQGHhpJVXNjCO1u24eLiRecrhdiuhPPcM1BSVgiolJWVgr2AwsvX3mP9uRgULW6dAEfuwlWaycopUewOGUTIwEGERczhyZGhvDhxMcmODL1Qgpj3xnRCzOt5aVwyWQWXULXjWL9nQj0rV/6INxxLPulJ+aQn7WS1djjv7JxDdMRmUj6sZ15FPW5Yr0rl9k2JMTy1Jt+h8m5GUVEp9j7danSUm8aesYphz+1p5pDDKupFTKn7MaXuJzFhK9Gb1jEtOpS1x/c3Xm5L1mtjc0JuVEafyby2JAzbh6/zVOIxzKXllbfnXlI7E7r+rZUbu7rWzX/n246zUqcD7dzgqtd2sCnshWybEc2y44110hur19bXdvXqwL4qXdFooMhaKgmIEB2IzAkRLU+5uQd8qPZyUJTrOoE2cx5WjS8h141/bo5SbLY8Ll9xxd8wBLeS78m1ARTzU5EVNN54Vq/riraLQnlZMZcbLK8+Kpcv52FTVTQ+4fQmj1OWyrOq58/nUdapB9oaE020XdxBtVHi6G2AobqzmpiwjMlT1pOlCSKs7udjV+vv8Gh9MWiLSUlMIqPgUuXNA/S+6G7UcXKE7TwWm4JG28CkhrqrN1avah4mC+gDgxp/HkFD++qwcsynzoPOG5+64VvOYlHd8TFcW6DRe6N1vvZek7EQ9EEEN7brTYqzEItVBVfX2m9p63ptIsXgh17NZNu6dMyllZ1mH4PnDfvldbXId958FrPqTr9Ar/qXq4VYbAp6g/e117S+GLQqFnMDV0/qpaAz+FVXiWLwRUchlqorkza1brvWDa1HPRXYUL2ac8i90X40kaazOz4umhsn2zfZljekZdtyB2i90WtKMZuaUp9CiNYmSYhoQeWVP9o+g3i0TzcUxalZv2H2U2eweAQxaqQ/+h6eaF0rf6jU4zvZkuHKqCUvEz3UH73el+Dh41iYMJtRDj8tS0FvmMlD+ofQd3+IIaHxTO6rkJ2xrWpegcrp7w9SqHmMiUPC6anxQx8Qy/CeKtknv3ForgaAiy6K0YZwencfjH/AfGb8ZggVZ7ZwuOo0dkX+XrLK7ueJ30Xhf5c3nroYIvvdizX3c3Id3ZOBE1g6cwQhgb3Q9fAlZFIoPhRistQ5S1pQiMXZl7ChvdDUrBNrIRabO/1C/Co7Itogps0MRefg9iuDCGbhpgSWPj2csIH+GAJDiFzwRx7VFZKRntf4+3GkXi+SnJiCNXAqby4YTnCfXhgCgxn19DzenBlUu7/W0L42gS09DSO+BNftIJUeY3d6KYGT5hA1sBf6wOHMezqoVqfanLSVdEJZ+OqzRAT2Qt8niLDxz/Jq3LjaE5QbjNOJUSvW8s6CcUQMDSIwMIiwp19mWoiC+Xhm7ec1tGa9tgRzHlbFl9CBlUOFNH3GMWukX5OKaJHvvJrGxt2F6KMXsnR8EAZ9LwwDhzNv5tDKz0TNZHdqIbrI6cwa6ou+TzBRCyfQz3aM3SlNe06MLuJZZg/1Rd8nhNkzw9Ca0thdNR/EZsrDqgni0YFdga4YJo0ntL4MoKF6VY+xZXse+piFLB0fjEHfi8BfDyU6bgXzhja1M9+Z8Afms3f4FMbWmxW2TFvekJZpyx2nCR6Ej5pDSksMixNCtBgZjiValHn7ZraEzGHaX7cxz7nmbR17MeujRJ6p0RO7egvK3MQYxq65NllQNW5lZaIfL8eu4dNaZeSz5YX5EPtHopYkMLvqwXimjD3sbsrYF6U3YQ+OxqsLlF08Q9Y/Yvmo1njwdbyT4sofgucy7wENlJ0m+/ACPnBkUnqVctzpHRxFmJsGyn7ktGk5644cvHYl5cphPtq3mom/fZJnnpxGlytWcnPjWZfm2KR0AFVV0Qwcx9LI6XhowGbJYd/yZWw5VWc9407Wbvdn9pJEDjlz7ZafajorlyfxWuwK9kaq2IoKydiRTJYhzOH9RD1PurGU6JFTWRrjjsZZpSg3h32LV7Ky0SEjVzVer7avVzF9finzYqbyzkh3oBirKYeUTYW1hlc0uK+O7xFYU9iWPpl5owahfF3zoXyXSFm2jLVLZjPt7URm2bJJTkohV+d7bZC5NZUXZyjMi53AvPcn4kEpRZazZCR9RJFDcZZjSs8hYvw45kW4V9ZrUR5Z219h2braE2pbtV5bgGrczKJ13ix8NZFDdhWbNYdtScfo16QHkbbEd74cY8Jc/mSdw+zoJWxeqKAW5ZHx4XtVQ9vKyYhfzDJlNtOWrOEZDRSZjrFy/uskOzCHp8Yek5GciT42gY91CrasFJbN31x97KnHN7MyyZeFb2zjkK0QU2oaaRZ/fOqWcoNjw7hmPn+yzWF29MtsXugKtkLMxmNsqXvioQU02Ja7jmD9V3MIrl5zOge/nQ4Us/v5KBY5dHOMFmrLHdKNsAh/bCmvkNak+hRCtLZf9A0Y/G9HV+7e/W4uXPhna8ZzS8QAEBoyhLT0b9o7jA7zeQhxO1H6PMsHfwlg24S5bL/BAwKVX89j7xvurH1kPtulg3PnqnpOiBI/mueT5Gx7h9JnAh//ZRC7p8xli7m9gxFC1CTDsYQQog711GZeWZ0NutrPFVAChxI1PAi9tiuaHkFMiw7F+XgKKZKACNEh6bQqacveZZu5vSMRQtQlw7GEEOI65Rh3bOC6xzsovoTNimCW1h1nisk9nsSLi/fT5Of7CSHahOXrnaxu7yCEEPWSJEQIIRykHt/A1BEb2jsM0dGoqfzpt6ntHYUQQtxSZDiWEEIIIYQQok1JEiJaRPfud7d3CEIIIYQQ4hYhSYgQQgghhBCiTUkSIoQQQgghhGhTkoSI25NLODtHzGDyrXrrhcA/8sm3Bzjx7QFOfLuWWX3aOyAhhBBCiJZzq3bRhLi9Gd9j7APvQZ9n+eSvg1pvO536Exb2DA96+uHppqH89HzmHTjcetsTQgghhECuhAhxZ+vkSucrP5LxXSLHL7Z3MEIIIYS4U8iVEHEH6Uz4f05jzJVv+dh8lMOXK5pdknbgBF6OHUeIwR3sxVhNabw1/y2SC66to/QZzrzYCYQFeqOhGIsxjbXL3yLZDCj+RC35I5EDvdF5uEJRHhnJ61m5Jh2z6mgUTuiGTmXhzDCCfdwry0h6l2VrMrA4WoR6mC9SDgNehPWdRb8mfAZCCCGEEM0lSYi4g1TwwwUzGIbxdp+RFJ03stX8JZ8WnqOoKcUoQUxbMhmDcRVT52diU7wwhPiDc411egzntbf/TKD5I1Y+txiT3R1DxHB0Wicwl4PSDa09k8TlGzCZi1H0w5m94GVes8bw1KZ8h8LQ/Ho661cMwrRuFVNT86GqjKXWmUz90LEyhBBCCCHagyQh4o6Se2EPMy/sQePSl7H6UMYEzWFG0AUO5X3Jxz98y2G7I1dHPNFpwZJxDKP5InAe86nMWmsYIscRQgovvbCBlFKA/NrrlKazOi792t/mzWwcGsGbIUFoNuVjazSGbkRERaCkvsKLm9JRAcwbeGtgKOuHh6L/cCtmB/ZECCGEEKI9SBIi7ki2yyfZnHOSzTkawv2fZXmfSQT9qoRh352k0dFQ6jG2Jxfzamwin0RkYjTlkLY/mWTj1UkVTuj13mDeQ0ZpQ4V4EjJzOrMjgtDrXK9dRMlS0EDjSYjijUGv4KFbytGIOsssnngoNGFYlxBCCCFE25IkRNyRFOd7edw7lLH6QIKcSsjM/YTNp880noAAcJGUuGjG7hhEyMBBhEaMY2nUCEKfj2HR15eq13K+QQm68X/mzUld2b14LtNTz2JVnQiJ28Kb+qbshYopMYan1sjQKyGEEELcWiQJEXcUnTacGb0HE+7pjr3YyN7sDcwvOIPlX00tqRyLMZ3txnS2b9rDrB1reDTMD+XrTFTKMZvzsI8MIth1T9VwrJqc0Af2AuNmVu8/W3XVwxWfmldErlJBRcG57gI1D5MFRgUGoSUfa1PDF0IIIYRoR3KLXnEH6UxQzwfoWZZO3MFF/ObQZl4914wExDWEWXHPMmqgL7oenhiGjyBUBxZTXvWVFNP2naQTysJXnyUssBd6vT9hk/5I1EAnoByLuRD0gwjr4QQ4oR85nahA5fptWXLItXkRMn4ohh6e6LRdqxZcJDkxBWvgVN5cMJzgPr0wBAYz6ul5vDkziHpKamhn0NwVQE+tH+6dwMn5XvTaAHpq3Jv4oQghhBBCOE6uhIg7SAWfZ63k85su5xJ4DGLaivFoPZTKW+Nuf4VlO2o8aKNgPy/OgHmxE1j4/kQ8KMViTOGt9HIAzB++xUrDPGZt3cYseynWUylsSy1kWo86m1LTWR2fzKuxf+bjUQrk7uCp8e9hAmxfr2L6/FLmxUzlnZHuQDFWUw4pmwodHFYGEMCY0Qk82OXq39OY5w2cW81Lu7c5MEFeCCGEEKLpftE3YPC/HV25e/e7uXDhn60Zzy0RA0BoyBDS0r9p7zA6zOfRUeKo5hLOznB/Pt33Npub/zgQIYQQQgjRCmQ4lhBCCCGEEKJNSRIihBBCCCGEaFMyJ0Tcnsp/5FOTjcwm3/VKCCGEEEK0NklCxO2p4gybT7d3EEIIIYQQoj4yHEsIIYQQQgjRpiQJEUIIIYQQQrQpSUKEEEIIIYQQbUqSEHF7cgln54gZTJZZT1W6Erbib3yyoKGnqfdi1Iq1HPj6ACe+PcDe69brxayP9vLJTN+2CLb19HmWj/evILLuQyGbQTd+BQc+ehbDzRfVATgRkbCXbxKGNnB8NFdjx50QQog7lSQhQtwJAicwK+Q8WxIz632aujJ0MrOGQvKMKMIfGc1T8XXXK8WYkUa6qbTVQlRGruCbL5cQcRO91UbLOLWVjUZfnokJvrlOsRLMtGhfjIlbMd1MObc8J0Li/s43749AW+/yS6QkpqBGPMuT+raNTAghRMcmSYgQtz0nwiZF4GFMJrmg/jU0Wnc0trOkGc9jtV7Cdl2mcpGUhGWs3H++tYNtZZdI2X4MJWw8j9bfa3aINmI8YcoxtqVearnQblen9rA714/I8f5yNUQIIUQ1SULEHaQz4f85gzX+ITzk0txxWp5Eb9rL3k0JfPLlXg5s+iPRC9Zy6Ou97E0Yjv7qakovRi1IYO+X/6+9u4+Oqjr3OP7tuuTMVTO2ZKIJo9DJAonBm4RCAjZRyyhN0AoiYH3JFUnlForyokAV8QpRARWoAtbKXcVQFAUJUQGBUGhSMbGQhJKkJQZDM03sECgT7mIGWHOCt/ePBBIgwAwQA8Pv81/m7LPPc/aZrDXP7GfvWc/OL9az+YPpZMRf03zwGobM/5itb6ZhbdWz8cOpbP5iIZnHOwmPI2POwuY+Pmb90qmM6BkWfMhGMukp4ZTnF+M95ZBj1EJ27tjMlumJWCLTeetPp5ZjRZG5tOm1nTs2n6EcK4yk595j29LHSR+VxcpN69m5Yz2blz5OfHMLW9+HeP2DD9m2YzPbvviQ9Usnkt5cEmUdPp+dOzazPSsZizWVV5tj2Ln6FwGXOgXTh1laSIk/kSEDOgfY+6k640xPhNJCStqYVjrbvQKBPdfwODJmLmx5/6yew4TW8XZJYeqb77D1i83s/OJDVs5/iKRWSZVj1EK2rZvOhOfm89Gm9Wz74kOWPZfSarYiDEfaVJatW8+2Lz5m/ZsjSWj9ZjynMJzzP2bnjg28NSQCS9IktjS/RzbPPHWWaR+FBdXYnWkkKAsREZFmSkLkCnKMv+13QeRA3kx7ic23PsLo6BuIDLofA6uvkBeeXo07djgZ1lxGP70Wb8owhjgAwkgan8X09DA2zhjP0MdeYLknkanzJ+EMBzhMfl4ZxDub/24+Jz0Za1UBeS6ArmT8ag5ju+zh7afHM/SxGbyzN46pv5pESpAf5IzYRBKMelwVB0875lo6gd59BnLXrDL8B/IYd+tAevcZyD2zj5dj7SN71EB698nknaq2CrlaWGIGMzapjLmPPUC/H4/hhdWVTUmPkcjYrJHEun7L6GEZPPjYLBYU7ANL03ne1ZPp3Wcg/WYU4/cW8kxzDL2H/ybgUqeg+jArqXBBbNJ5fjNvxJEUDzUVlaeXtp3jXgN7rl3JeHMOE5IOsmrGZIY+PJ5n36/Fao84cTxzzvOMsJXx8s8zGfrkMmpiR/J61r3YW4VisSeT5F3G6LR7uOfpYiIHj2Ns3+Zkp+dIXs1KxZL/GqMfnszcmkSGJAUzGo3kTx5K7z53M25NA/6SN7irT9OYD5xZctq4uCvq8NriiLe32ZmIiFyBtGxXrig1+9cxfv86rFfdzP2OVIYmTuLJxP1srf0jK/+2g8/9xwLoxcRdUkxFRQRVnuGYJYVUlfqo8TqJ7BIG7kSGpHfDnTOGhQV7AMietQJn7mhGOK8hf+1hvEX5lDCJdOc1rFl7GIxEBqWEU7W0EDdg9B1GRnwt7zy8gDWupqu6Zq3A+fvRDEkJo6igMfCb7hKNzXKQGnewoxWsSrJn5FLiAThM0dq65tejsNtoGjPXQWAfrt1l7R3MWfhwe0yMLtFYAU+wp1ujsVtNPAfaWh9z9nsN5LkafYeRkeBjzX+9THZp03N2uX5DyfFOeqYxJMFH3i39kFYAAA/ZSURBVBNvkVfRCNQxa2Ey67PScHZZx/LjJXfeMpYvLmu6v9JNFHqcxMZGQOk+4genEuspZNyiAipMYNESNjrnMyTYsQiQ6anDQzIxXcLAFcR7V0REQpaSELkieY9+ybLKL1lWaeWuuMeZ3fMREv/9EAP//GWbC7dP5Td9YIZj+sFvmoCJiYFhAYymD6muqtqWEzx7qPIYJDmigT3gK2ZjCTyfnop17Sb8fZ2kWKvJzm9ac2F1dMNu6cXE3M1MPOXaJTYDCOKDnMVojq99+d1llLf1id4sJievgVemZPNRehkVVZUUbsojr42ZmW+L328CRssERTCMphkDf1sDeo57DeS5WmO7Y/PuoaSi7WdsOLoR6a+jfHfLca+rFrcljlhHGOxtet3vbWhJsEwT0w8WwwDCsNujwZ3Lickts5oKl9luSQh+E//x/w8RERGUhMgVyrB8n3u6pXK/I57EsEOU1XzEsq+q2/2DeovDFOWVwXQnzvB8DqQnYq3KJb/VwnG/t5BnfzyD/AsNyuvD9EdhswLtt7kV+M+U6Bwkf2Ym969OJqVvMqnpw3gx415SnxjDC190zMJua2Q4+BpOWyMTEG8DXr+B1dpWMnjuew30uV6M96L/HMdartHO73xrFFZ8eIKedhIRkVClNSFyRbHb7mL2rdPYOuhJno7uROWuJdyzcQ6Plf+RLUcDKcUKgFmP22vgiO3W8pqtO7E2E7er/sRLTSVZcaQ7kxmUFEHFpqZSLGj6Zttj7U5K/HksRD81nKpKaogmxnHhfZ2/RtwVReQsXcBTD09jeU0ESc4eJ6/J8Jtc8PZJAfXRjYQu4K6pO78kxLcHlxvsPbudocGZ7zWQ5+qt2nPWNqarlgOWriS0WsxudXTD7q+nJqBSp0Zc7nqwdW3ZSIFuOLpcyN7IZz/X6ojGTh1VNSrFEhGRJkpC5ArSicQb+nDDkSJmbnmBO7Yu45V/VOP+v4t8GbOMNQX12EeMY8KA7jh6JpEx/SESvMWsyW/1zb+vmDVFkDRmHCm2Sjbmt2x/a5bmsrwknCFZz5M5IA6HoztJacOYPn8iQ4LdWnZvMSWucOJTelyc+wtWeAoTZj7OkL7dsXeJIjbtXlLt4K6qPfn79731uC3dcQ7oitUIO9fn2rYF0keXROLtDZQUVJ/f/VBLfmkD9vg4TltnfY57DeS5mqW5LC9vapPxw+7NbR5n6vDmncl2F7CmKpz0CeNIj++Ko++9TB+TDBWbzrgF86mq1m7CZU/lZ2lRGIRhT3uIQTHnMxaNTYl1TDKDenbGaHPMw4hPiYOKQgrbcyZOREQuKyrHkivIMTaUz2VDu1+nkZJ5M5hlTGRs1iJ+ZoUDVcXMnfYaeSd9CDtMYV4xZroTa8mKk0qxoI7lT0+DKb8gI2s+E63gdddSVbKONUF/fV9HztpKHhg+gPj5lVQEcaaRlsWWV1JbthKOXczOTMBfyDM/mkFeQFU8hyEymbFzhmOLNOBALSU5LzNr9clrQsyKXN7OiWNiVjZbLUDNah4MYoesQPtwpA8g1p3PrNLz/Va+kZKcfFy/G0C6I5dsVzD3GshzrWP5k9Ng+i/IeGURUw044C4jb1Fu8/E9ZD/9MpFZo3n+fwZjpYGa0tU8M2MdAe89sHs1z8yI4sUJi9kyxcTrKqS83MR5HqPhylnG8pRJjP3dKqZa4MCaZ7mn9Q5ZRjJDUgyK5uUHvwmAiIiErO/c3KvfvwJtfP3117F//z/bM57LIgaA1JT+FBZt6+gwLpnxuFTiOOGqu8i9K46PN77JsotUZXVZM5J4MfeXGPMyefZK/oE9I5HpudOJXDiGpzZdyML4zqTPX8xEzyyGzm77V+iliX34fFaO2MW4x5Y07cQlIiKCyrFErgxmCQtmrMBtjb6yf7Xa3hl3zhvMvaAEBOAgefPeYJW78+klWdLKNdjZxYLZK5SAiIjISVSOJXKF8JTmsrC0o6PoYK4CspdepL72Fl28vkLWYUpWL2n5jRMREZFmSkIkNDX+nY+rvJRd7EXnIiIiInLBlIRIaDpWzbKvOjoIEREREWmL1oRc5i6VxeCXShwiIiIiculTEiIiIiIiIt8qJSEiIiIiIvKtUhISEq7BOec9Pnou8creflVERERELgtKQkLCYfKz8zHTH+cBR0fHIiIiIiJydkpCQsXudayp6cGI4XGaDRERERGRS5qSkJCxj8KCauzONBKUhYiIiIjIJUxJSAhxV9ThtcURb+/oSEREREREzkxJSAgxPXV4iCCmS1hHhyIiIiIickZKQkKJ38SPgWHp6EBERERERM5MSUgosUZhxYfH09GBiIiIiIicmZKQEGJ1RGOnjqqaxo4ORURERETkjJSEhIww4lPioKKQQl9HxyIiIiIicmZKQkKFkcyQFIOinHxUjSUiIiIilzIlISHCPng4qZ61vFNwuKNDERERERE5KyUhIeEa7OxiwewVVJgdHYuIiIiIyNl16ugA5GI4TMnqJZR0dBgiIiIiIgHQTIiIiIiIiHyrlISIiIiIiMi3qtONXXsE3Nhm64xh+W47hnN5xAAQeZ2dYMauvQx0Jpz1+Ob88m8pkiuHxlxERETkwmhNSAj49LNtbb7+kzv6f8uRXDk05iIiIiLnr9PXddUBNzb917F//z/bMZzLIwaA73e1EczYtZ8E9rpdZzjW/xKJMdRozEVEREQuhNaEhJRw7nxpBWt+2Rujo0MJdUZv/nvFCubeEd7RkYiIiIhcdlSOFUpueYRJt9bz3sidhOzPhfxbb5zOCaTFdOOqb3x8XZPNu599wr5vAu/Cah/Dw6l30yvSBv691FQvZ0XhyX10+t7djEjNINH+fa7Fi2ffBt79dBFfHW9j7mTJynpyxo0i4bM30SoQERERkcBpJiRkGNz50CAi/7KRDXs7Opb2EkGCcyYjohr49JOxzNm4gaPdpvDErb0Dz6avuo+Rgx4g6kA2b7yXwZx1qzjUdTxPpPZr6cO4i0fvm0wvtvHxxvHM/XQ2n1RXc+SUrty/X0Ox7U4evUPzTiIiIiLB0ExIqDD6cfet4ZS/tR1vW4fTstiSZfD2tGpSxg8mKcaAA2XM/fk0clydSRk/ibHpccTYIjDMBlwleSyYvYQiT9P5jlELWTliH8uLInAOiMNu9VG19g2eml1EcxMcaRN5cUI6CTaTmtIVFBkjGeF9jdsnFzTPzHQmZdQkJj6SjCMSTHclGxe+wdxNdYHN3Bi3c3tMOLs/m83n+xuAaj7Y0Z+Xkn9Krz/tpDyA2ZBON9zOTZZqcgo/wWUC3lpWlNzOnNSh3FS4ncpvIOqWTBL9G3hj4yJczX263G105tvOH/4SzjM/7ofx2eehO/skIiIicpFpJiREGDf1JsGop+YvDWduZIkjc0w0+TMzuf2ODEbOzsPtBzCINHzkL36NcY9l8uCTb1FuH8yrWfdia326PZkk7zJGp93DPU8XEzl4HGP7hjUd7PkQL2alYyl6jUcfnszCqmSGJLWeIQgjfvwcXn8knMJ50xg5bDzPvO8jNSuLsfFhgd1kZC9u7FSPa1/LPTZ4vuKQpRsx3wtwoACOmTS2+rPxGxMs3bjRChDBTV2jObSvlhjnO8z52XrmZrxNZq/eXHVaRz6+/Koey3/042ZNhoiIiIgETElIqOgSTaSlgZqzlmKZFC18jZyKg5i+g1QVFFC0F2Afa+bPJXttCRW763BVFDD3/UqM+EQSWn+49paxfHEZHsBTuolCTwSxsREAxKankeDJZ8G8Aipce8hf/Fs2tp49MJLJGBFNyaKXWbipjCrXHoref4vlVdEMSusR0EL6ToaNq/Dh8ffAOWw9cwbdR8SRBg4RzrWWwLKAY/v+zNf04vYfNCcVV/XjJwm9CCMcm2EA0diutnBtTAZ9j65i8SdTeKfcR8yPZvOQI+K0/tx7GyAiAntAVxcRERERUDlW6LAYgIn/bG38dZRXNLZxIAxH2jimjkklyR6BxXK8fTHW1qd7G06UXmGamH6wGEbT+Y5o/O5cKo7XJJnVVLhMhhxv74gjxhpObNYqdmadfHXv3s6B3eOJd6vJkSMNHDrioxEjuDexdxXvfnYLP09dxLw+wLG9lJVv5WvbD05qFnZkCx8UbuAfAJ55bIp5jxE396eTa8NJ7fymCRhYEBEREZFAKQkJFV4ffn80kVbAd6ZGJmZbCxd6juTVLCfe91/jwexiXL5GjLQstmadPrtw1iTnXPz1rHoyk1mlbSVC53bsiIejRGOz1PLJxv9kG4C9H1fj45A/0BUZJvuqnuOlqnCs1giOHanlaNQ05vTxse+oCTRwyAT8tew7cU4DniMmYddGnFaSZQ0PB7O+zXU4IiIiItI2lWOFCPOrXdQQTcz3g1+cYMT2wGGWsWpxES5fU4IQExsVxLf7jbhc9VjsccQev7zRg1hHq1hcldSYESTERwcd3wkHdvH1sWhujGopi4qw3cS1/lpq/jfYznx4vbUc/SacuNj+XHvor9R4ARr4+oAHrN2IOtE2HNvVBo1HGjh6Uh8GN/eMhr/v4kutShcREREJmJKQULF3O8V/Dyfh1h7Bn+uqxWN0J7VvU1mUtecwJgwOrp+qvE2U21KZOiWFWEdXUsaMJr31qnazmOU5tTjGTOfF4UnEOroS/8MBZM6cw9QBAS5MN7eytcZHr6Qp3HZ9D6LsGTzc5yY8VR+zK+DfCTFwxI7nNsdtOK6/jf6p8xh5s8GuklW4mi7CV3/dQr31bh7ufxc3WHvg6DWFtBtMdn25jWMn9dWD224Jp2bHdtraPEtERERE2qZyrJBRy4ef7uKn999JAruC+vE8s2IZLyzuxvRXstnqN/F6Klm1tpiER4LoZPcKXpgRxYtTnmflYBN36QryKuJa1oTQSMWiaTzlncTEzOdZNj0cvPW4KopZ7g60PKuB8vyZ5Dgn8JP7srkWDzXV8/j1n7afkhycg3ETzlvvI/pqOHKwmvI/TOGDqtqW457F/Do/nEeTJjO1jxWOfMWuz5/jXdcpO4/dciepXXbx4Ue1iIiIiEjgvnNzr37/CrTx9ddfx/79/2zPeC6LGABSU/pTWLSto8Ng1Mhh/HbFyqY/jH68vPJZLK+PZOpnPkY/9CBLl+V2UGRRZCzNJmP3ZIbOrgyp39BoGvNPufOlZTxjvsLgl7afuL+OHXMRERGRy4PKsUKJuZ3XX3qff1ijA9ry9uKKImX4vTjjo7CGdyZ28EhGxNaTn1cdUgnICUY01r3v8+Kvt4fm/YmIiIi0I5VjhRjPjhze6KBrR8bfy4QJ44i0gtddTf7sWSw4z52wLnlmNZ+8Vd3RUYiIiIhcljrd2DXwBcg2W2cMy3fbMZzLIwaAyOvsBDN27amL3XHGY99mjDuWzGPUktavfIfrL5ExutgulTEXERERuRz9PyvUPOjSLywUAAAAAElFTkSuQmCC')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('This is how the Stars were placed, it generated x amount of stars in various places so I didnt have to hand place each one')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/png;base64,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')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('And this is what determined when each star was supposed to blink, also at random')
									])),
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAVoAAAKbCAYAAAC0DUL6AAAABHNCSVQICAgIfAhkiAAAABl0RVh0U29mdHdhcmUAZ25vbWUtc2NyZWVuc2hvdO8Dvz4AACAASURBVHic7N17XJRl/v/xV8DMxGEwARWH0EFNxBpJRfcbdJAycVu1PLSZfpcy22w1zU2tjH4eemi4qWWarbaWRWnmMcFcofyOZowpkI6UiKKMGCMQh2I4NMOh3x8goKIOyETg5/l4+Adznz73PeN7Lq775rpu6tN38G8IIYRwGKfWLkAIIdo7CVohhHAwCVohhHAwCVohhHAwCVohhHAwCVohhHAwCVohhHAwCVohhHAwCVohhHAwCVohhHAwCVohhHAwl9Yu4HJu3NJnOD38buVmRSWlZzZjPJnf2kUJIUSzNTtob/IZzqCQ3g12cJ7T/7eFXNv1FXSTVyi9/JzJS15PdmkVTpRd3w6FEKKVNb9F66LC6eevSTmcSk22VvFb9fUXpFR3xKX0FLm/WKgCqq5/l0II0aqaHbQuCiVYS7FVV3HlcRad8Qh4mF7aTtysUoH1NKcOfEFBtQ+dbg/Hr3MnbnayUppj4OQPaVirAScXnG65l4HD7wWg6Ohq0nKqUHiF0qNPEB3cVVCeTXbqf8n+xQb40KX/g/h5deRmBVQWGjAePorVtQfavqH4eHniUllEYcaXnDqXz28449lnDD26enGzSoVTVSmleYc4lfo9ZbVfFM4dBtLz9jvx8nSHikJyjnyKqfBqNXTEWzeU7r5dudm5EttPBn5IOUp5cy+uEKJdaXbQKlxV4N6X7r09KS08Q2F+USOtT2dcvXxxyt/DkfQcqp2dqa50xrPvCHp2OMtJwxdYXHrQIySc3v4/knrWAkD1z19zpLal/Ft1FbjeSe/+QVRnfMGR7FLce42gz4D7Kd2/h5+rPeng5UnpiU9JzbXipKjEig/d+g/Hq1TP8f1n+e2WUAKDh9P9508wWZxRuXfCKXcPRzJywLUH2gH3clv3bIyZReB6B31CQuDclxw7kkOVkxtYr17DLz6h9PSxkWlYQ365Cze7wq/NfkuEEO1Ns586qMg9xOmzP1Ltciua4MfpP/AOXK+wbrW1GKutjIpyC1VO3fHtqqLo5NcUlpdRYfmec9nFuHfpjnPDbaqrakIWcPUNQl2ehulsDhWVFn4+mcwvLn54qS+sXUlVeREVlWVYy23QQYePew7ZaWmU2coozztErsWTDj4d6/dfWVOT9ZfvyT5fhquXL86AyleH2prKmZNnKCsvw1qaj7XyGjVUWql2cUPt7g7VZZSXll2llS+EuNE0u0Vb8cspfvoFIIWszDvR3R2Gn08aGfnX6FV1cUOhUNFhwDRCG75erEIJNNbNq3B1w8kzhP7DQxq8aiVf5dzI2nCTyg2l8630vH8GPRu8/muxCii9/FysVrhFhROgcnWD8qLLWqRXq+G3vK85fvxeAoIe509BP5GXoSczO1/CVggBtNTjXeU/UV7pjELlzDVvX1WWUVlhJd+4jpONhLKqkU0qyq1U/3yUI9+mYLWjnN8qrVRWneWsfic/VV66VHnVbSvKreDVkZvhoj7Wq9dQRVn2V/yQ/TWufg8S1Hc4/sWfkGWxo1ghRLvXzK6Djnh27oabqxsKV1+8eofi5fITPxfa8WxX9Y/k5lXi1Tsc7w5qFEo3VGpfXK8S+eV5aZSq76RHQDdclW4olB3xUKuvvMHP35Nf7of/7Xfg6e6GQqnGtYMPCjvOrDzve0pddfTq3aPm/Nx9cVNeowalL55qN5ydqrAV51CBChd7DiaEuCE0r0Xrosarx/10Vnvi4lyJrfgs2Uf0nLfrNruNn3/Yyenbw+ke8jduVrhQaT1P9ndbyP7lCpuUpnAiRUWPPg8SHOiOU5WVsrz/4wejhYrG1q/O4WzKl9D3T/QOvR+lcyW24jQyvtXz87UeQSs9yokUN3r0CadfD3eoKKboxDbSs69cQ/Utd9BD1xs3hQvVFcVYsvVkF9pzLYQQN4KbZBZcIYRwLBnrQAghHEyCVgghHEyCVgghHMzlVv9erV2DEEK0a9KiFUIIB3P58VxGa9cghBDtmrRohRDCwSRohRDCwSRohRDCwZoZtO6MWv45B94ZRsMRB5R3zeGrgyuZpAWlbjKfHdzMW8PqhybUjl3Ogf3LGae9joqFEKKNaeboXaXo443MiQon3COB2BIABSERg1CnbyLeBDbe56VVffl49ouMSp1LrGoMr83w59iy6Ww12VfanwdG8+atjY/O8lPWGoYeOcF1TlEmhBAO1+xhEi0GPcnMJCLcndi4UlAGMzzUg/QPEzHXrmPa+AZvh65izsLn0SnD0RhW8Fhcrp1HqOSb4ysYf6qxEivBmishK4RoE5o/Hm1JEnuS4dWIMNRxCVgHhhOqzmC9vmGQ5rJ1/jrCPn2RR23x/PO5fRQ04RCW8myM5Y2PICshK4RoK65j4O9SDPFGiAon3ENPfkQw6vTt6M9fvJa6dxBaNUBPdAEK9KmNDmzYaGnSdSCEaA+ua4aF+u6DQRSEeJG6sb7bAADvUKKiIrBtfZl/qmeyZMFUDE+8TXKJPXuXrgMhRPtwfVPZlCQRa4AlU6Zi8U5jzUXdBl0YtXAm4ZZtRK5KJl25lNiPonltdhJ/W2CwqwvhQteBEEK0Zdf5HG0pifFJ2DS+qFP1F3UbaCe8yBxdDmvmx5BuA0qMLF0QhyViJq82eORLCCHaO5lhQQghHEz+MkwIIRxMglYIIRxMBv4WQggHkxatEEI4WLMG/g4L/ROJhkMOKEcIIdofadEKIYSDSdAKIYSDSdCKVqAgYvluDr0zrNEBg1qGO+Pe281X0aEOPEbzeE9YyaFdcwj5oxUmHKadBG1P5mzbzWfTezZ9097jiTn4Oesm+Dd40Z+J733OoQ/HE9hiNf6BdB1DzMFPiBrY+IA9QoiW1U6C9jqc3MbSjTnonnyaCO+al7xHPs2zuhw2vL6N9NatTgjRDrTtoFUGE7XrK45+t5aJAUoCJ63l6HdfcfS73aweWd9a8x44nrc+3cyh777i0P4PWD0rFE3d0gpS1/6brZZBPD89BLVHKHOmD8K88W3WnLR3SEcABdoh/2D1p5s5dPArDu3/hJjoEQ1axO7oxkYRs+tzDn23mwO7lvPayJ61v9Yq0M36gEPblrNu1+cc2hXNjCfnsGP/bg5sm0N47RdA4PQ1HNq1kKjla9i9/3MOJHzA6ieD66YTUuqeZ/fBNUzS1tcUHr2ZQ++NqFlHO5nPvvuKo19MpZ/Kl0f/89/a67WSiV0bXNbew4h67xMOHPyKowc389ny8YR4NDxXf2Z8upsdrwxj3IKV7D64m6MHP2fHKyHN/DW9I6GvfMCBhGjGaS+8bwo0Q/7B6m2171vCB6yeHlL7vtWe14djGryPQO/J7Likpa5SB/P8e59w4GDNNY8a0uWiI1/9s9GR0OkLidm1mQMHv+LQ/s18tnwyod4XV68ctpADB6OZOGQyq7fVvL+HEqLrpmxS9h7Bkk8317zv2xYyUdusiyTasLYdtDYji0cM5c4BU9iQaSN9/RTuHDCUOwc8xLS42pDsOozX3owk0BTDtPGTeHqxEZ9xr/LWkz0v2s+aZXqImMqSN6cSXhDHorVpTRqGUX3XTN6NjkCduo5pT0zisefeRU8XNLXJox4yk7eigrFsXUTkmOnMi1cQ/spC5jQMBVUhW+a+Qax1EE+NtLH072+gV4UzcUj9IDwqzSD6mf7NY/c9wkNzk/CZEkXUEHf7ijS9z2MDhnLnX97lmDWHLX//c+31msGGCwMCeQ/hX+9MJaRgEy89MYlHnnmXY5rxLFk4hIvzRYkmIpLhBZuY+uDD3PP4fNYbipoxdGVHQl+J5l9Dcnn7mXlsNVXUXs+prIsOwxa3gqfHTOLp1xNRjXyV1yb4AxUkxiZREDiECG39nnQjw9CYE4lNufAFqUQdGk6gYQVPPz6LRQZ3RkVH1QfdNT8bSnyUJejXvsHU2vf0mGYk/1o44pJrAaiCmDTFF/2CSdxz70QiX4/HbAWUwcx5cyYhlu38c/wUpn5YQtjIvqiafJ1EW9a2g9YOmvARhJDE24t3kXzyHKkJ7/J2fAmBI4eha7Ce5eC7vG3wIiwE4leuI7VJidGRiEnheKeu4+XXE0g+eQ5TqoH1c99HbwNwJ3TkINTHNrH4w2TSTafRr3qbLWZfwkcG17UCraYkDKlJGEwlWE4mkXwykWQTeGu9GhSaxAfrjVgAS0oMG1I8CB8XdtEkmddDO3I8oZZ45s3fheHkOUyp+1i6MhFCIwjzuGTlzO3MW2XAVFKBxWQkdt/pJh7Ng7BXovnXkKKLQhY6EjExAuW+d3npQwOppnOk7nuft+ML0Q0LQwvYUnaRWNCL4SNrQ1EZzKhwX0z7dpHa8BDp8Sz6MJl0Uxrxy2LQW3oxakhNf/y1Pxu5xC5fyvq4ZFIvXIuNaSh1wfS7rOluw7DyDbamFmErKSJ93z4M50E5cBjhmixil23CcPIcqXErWLPPrgGZRTtyfePR/uEp0Gi9wGzEVPfZriA99RxE+KJRUh+o3oMYrlNitXqhC++L8qDR/taZsif9tGCKM1488HkdLwI1SgrST5Nf91oW6SYbPtpuqDHWvmbDCmCzYbXWHN0KqFQe9WGcn4O57lxsmM2FMNAfDZBpb71XpCBQ54sqoBcffzv2kmUZePsADY5dcLLh+TSdauDTLAlVQuY2TOYG3TTKbgRqlfhoXuNwxCUbmbvgowSTLY2t8TmMGjIM3ap/k64bQph3Bhu2nmuwsg2LKa3+PbGdJt0MIYH+KMmx47OhQDtsKnOmhBGi8UJ1oRlqTbr8i816jmONzB6iDvDFx3KOdFP9MTLTc7DqLltVtGPtPGjt1ZGIV6YSWrCNaa978Vr0P3g2bjor7Z5251rs7Lm02vGaSolKyUWTptX/GtrYV0PTe02tySsY+swuLNdaz2q7vlkuLEksfi6R8OiZRM02EPm6scExa7qCHlt17oqbp8clYJowglG6GPQjw/BOjWHP+Suu3nS9I/nXwnAsG9/gsfVJmEoqavpjFzY+i52tsYthBSsXX6frvGqiDWo3XQdWGouUCsymQtB0Q1v3a6+CQJ0/mHMw137e1UOmMie0hNhlMSTvW8fbBi8mzhpr/6NdttMcM4FWF3x53x0AOaSbwVvbE5+612pabfmmrGsGWkMq7261c7ABeKDVemE9fw4zYLPasKJEXXchlHh7Ky/vD7TVLFNddsEqSE/NAW3wJTe/mslJTYCrFz5X+JRZTyayJyWBRcuSUI+cw5y7avuabVmkm692PWuZ9MSmehE2NpJRoUqS4xIvmblDiVobVH9zS9mTQA0UpJ/DZsdnQxnYC63NyJa1Nd0jAAGBXZrUv2ox52BRdkFTdyIKtFpf6aO9wbSToM3BdB40A4cQ4u2OUqmoC12zPoFkBvFs1AhCtP7ohk3l+QgP0vcl1PTleYQyZ3YYtvh3WZlSARQRv2oT6YHjmTPW/8qHvEgR8Rv1FOie5q1ZwwjR+qPVhTDulfGEKgFKMcQlYuk3nqgJIQRqexI+fSqPBuSgj2tCFwWAKoinXqk5l5CxM3lKV4IhNrEmrE1ppFt8CYsIQg2oB0byqK6R1pclB7PFA114MBoPBUpl/Q05U9wmDIQRtWQyETp/tL2DCR87mSULxjT5mWJNt8nsHjaX1ztf/RengoQVLN2nZFTUzNonLIqIX197PV8ZRkhvfwJ1IYx6cg5vTQ9u8IWay544I96jxjJcZSRWX3T5zgMjmPNkCIHaICJmRxKuTiN2X00r+ZqfDVMWBcqehA2suRmp7j2GGSObNtqdLSUBfUEvxk0KwRtQaiOYGO51ze1E+9JOgraUPWvXYVCPZPWXOzn87U7euvB41/ldzHt5G6bekazevp51UYOwxL3BS2tPA+6Ezp5KhDKRpcuS61uWpjiWbi1EN2Mqo7pe4ZCXsOxbwdS58VhCnmb19vV89s5MRqlLKLDVL//nMiM+T77GZ9tX8VqEAv38+SxNaVr3hDUzkT2WESz5dD2rZ/iTuXYx8xJKaxbaDKxZlQjjlvPV/k9YN8lGoqHk8h4JWxJrViViG/Iau7/+L4e/XV7/eFfBPl56bgV6ZRhz3lvP5x8tJGpCXzBlXVd/7NUVEb9sBXsIJ+qVmqcbLAdrr6fuaVZ/tJ6Y917k2SEemFNzLvpiKtDHY7CAxRCP/rJ7TDYsBj2m0JnEbF/Fq6GlxM59gw2m2sVX/WyALTWGeWvPoVuyngMJm/lsYTDJcUlN+g0Em5GlL7xLuu5FPt+/md3LwzAbMhrtJRLtV7OmspHRu1pH4PQ1xAwxEjn23/KHFBd0HUPM9vGYXpjIvIMt1acuRMuSm2GibVK64+3djbDZYwg065v8m4EQvycJWtEmKQdO5bPVEajNRjbMj2nic89C/L6k60AIIRysndwME0KIP65mtWiFEELYT1q0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYBK0QgjhYC6tXcDl3Lilz3B6+N3KzYpKSs9sxngyv7WLEkKIZmtm0Kq5JeBeumu7464CW/FZzv/wJdm/2K67oJu8Qunl50xe8nqyS6twouy69ymEEK2peV0HLp3wuqUMs3EzKQd2km31pfuAe/FsgYKU6o64lJ4i9xcLVZVlVFS2wE6FEKIVNa9FW3mGM0fO1P14PvMsfgM9UbkAFwWjMx4BD9NL24mbVSqwnubUgS8oqPah0+3h+HXuxM1OVkpzDJz8IQ1rNeDkgtMt9zJw+L0AFB1dTVpOFQqvUHr0CaKDuwrKs8lO/W9tC9qHLv0fxM+rIzcroLLQgPHwUayuPdD2DcXHyxOXyiIKM77k1Ll8fsMZzz5j6NHVi5tVKpyqSinNO8Sp1O8pq66tusNAet5+J16e7lBRSM6RTzEVXq2GjnjrhtLdtys3O1di+8nADylHKW/WxRVCtDct0EfrjEdnX1wsaRRf1vp0xtXLF6f8PRxJz6Ha2ZnqSmc8+46gZ4eznDR8gcWlBz1Cwunt/yOpZy0AVP/8NUcOp2IDfquuAtc76d0/iOqMLziSXYp7rxH0GXA/pfv38HO1Jx28PCk98SmpuVacFJVY8aFb/+F4leo5vv8sv90SSmDwcLr//AkmizMq90445e7hSEYOuPZAO+BebuuejTGzCFzvoE9ICJz7kmNHcqhycgPr1Wv4xSeUnj42Mg1ryC934WZX+PX6L6wQop247qcOVH7D6d21jMzUFKxXWKfaWozVVkZFuYUqp+74dlVRdPJrCsvLqLB8z7nsYty7dMe54TbVVTUhC7j6BqEuT8N0NoeKSgs/n0zmFxc/vNQX1q6kqryIisoyrOU26KDDxz2H7LQ0ymxllOcdItfiSQefjvX7r6ypyfrL92SfL8PVyxdnQOWrQ21N5czJM5SVl2EtzcdaeY0aKq1Uu7ihdneH6jLKS8v47XovrBCi3biuFq3K72F0fZScT95Jbqm9R3RDoVDRYcA0Qhu+XqxCCVQ3sonC1Q0nzxD6Dw9p8KqVfJVzI2vDTSo3lM630vP+GfRs8PqvxSrg8kIrrFa4RYUToHJ1g/Kiy1qkV6vht7yvOX78XgKCHudPQT+Rl6EnMztfwlYIAVxH0Dr7DCWojxt5ydua9rRBZRmVFVbyjes4mV912WJVI5tUlFup/vkoR769cqu5od8qrVRWneWsfic/XdadobzqthXlVvDqyM1wUR/r1Wuooiz7K37I/hpXvwcJ6jsc/+JPyLLYUawQot1rZteBD359elOVsYdzlipucnLmJqfGW5eXqf6R3LxKvHqH491BjULphkrti+tVIr88L41S9Z30COiGq9INhbIjHmr1lTf4+Xvyy/3wv/0OPN3dUCjVuHbwQWFHeeV531PqqqNX7x64ubqhcPfFTXmNGpS+eKrdcHaqwlacQwUqXOw5mBDihtC8Fq3yVjp4uKAO+ht3BdW/XJS6hrTsa7Vubfz8w05O3x5O95C/cbPChUrrebK/20L2L1fYpDSFEykqevR5kOBAd5yqrJTl/R8/GC1UNLZ+dQ5nU76Evn+id+j9KJ0rsRWnkfGtnp8b65u46FhHOZHiRo8+4fTr4Q4VxRSd2EZ69pVrqL7lDnroeuOmcKG6ohhLtp7swmscRwhxw7ipT9/B0pUohBAOJGMdCCGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEgzUzaN0ZtfxzDrwzjIYjDijvmsNXB1cySQveY6M5tH8hozwarhDCa7t289l0/+ZXLIQQbUwzR+8qRR9vZE5UOOEeCcSWACgIiRiEOn0T8SYoyI/HMONFIsI7EhtXBIByYDih3hms33rOrtL+PDCaN29tfHSWn7LWMPTICa5/ljIhhHCsZg+TaDHoSWYmEeHuxMaVgjKY4aEepH+YiBmgJJFYQwlLRoahiduFGQVhowahTo1hz3l7jlDJN8dXMP5UYyVWgjVXQlYI0SY0v4+2JIk9ydAvIgw1ta1VdQbx+tzaFSpIjE2iQDeM8K6ARxijQpUkx+kpsPMQlvJsjMVnSbvsXzZGq8zaKIRoG65jhoVSDPFGiAon3ENPfkQw6vTt6Bu0Vm0pu0gsWM6oCH/2FIQTgpFFerunYpCuAyFEu3BdU9nUdx8MoiDEi9SNtd0GF9jS2Bqfw6iIMTxVEIxVvwh9ib17l64DIUT7cH2z4JYkEWuAJVOmYvFOY01dt0G99LgE0ic8zcTAHGL/bmxSOFrKszHKnN1CiDbuOp+jLSUxPgmbxhd1qv6iboM6Jj3x6TYwJxGb2uh8CEII0a5dX4sWsO1bzD0DFl9lDS80asjUJ5Asv+sLIW5A1x20V6ZA7e1L4MinGaXJ4INtaY47lBBC/IE5LmiV4SzZ9SJhtiz0qxazweSwIwkhxB+aTM4ohBAOJoPKCCGEgzUraDt37tTSdQghRLslLVohhHAwCVohhHCw9hO0LoP5aMRrvN7xiisQrJ3MFw8tJe3hFaTd8wABv2d9dlMQ8sonHPpwDN6tXcoNrw/LI5bynp8Dn4IUN4T28wmqLuRAdiqWX6+w3EnHtL69yD++gr//WIilugLL71qgvSrIT01Cb8m6wp8r92TOtlWE7JvOY6tO/861/bG43PIwj98/kX7eXVHYTmFMWsmm40dp2l9t+/HykJkMzl7KmFN5DqpU3OjaUdBmsO5IxpWXq7zwcSrmcH425j/4CIumuLd5ubWL+KNzvpNxw2fTr+wTPtiyl3LNUzx13wLGlz3FelNha1cnxEWa9Rxt586dyMv7yRH1NJ1Tf94b+QT3AFDMjq/n8UpR/WKl52h2h9+H36XbFcbx0IG9ZNp5GO+B43l19hhCA73AWkhBeiJvz32b+POAcghvffki3vp4bCFhBKqVWDITeXvBCuJN9eM7KIctZO9CJWvmZhA6fSQhAUrIN7L0mblsNSnQzVrLxxO71ax87F0eeHJ7/di9ymCiti/nUc2lldlInP8w0+JqjqPsPYw5syMZrvNFTSHphu0snb+JZLtGTVMQHr2BJV03MfrJ7fUjsfWezI6Pwkl+bhKLU/4Y41W4+L9O9AhfEj55ii8tAF786S+beZxVzP1ipx2t2u7Mf/CfjHe79PUKDhyayzM5ldR0HUzG58xGznQeyeiOaqxlJ1h9+CNiSuq/rTWdR7Cg72AGd/CEX3M5fGYHC06dqL1+tfvI2kW+z33c3UENjexDtG9tv0VbfYRndh6p6aMdPuKyxbbiHQzduQNcH2D7A4P5Zl80b9o9VGMtZTDPLowkMHUFT881YlH6EhgaBKqLVqJfqBfznphIbIEvo6KX81p0JKbH3ye94WqqICZNKWHNgkn8MxMCQoLxtgJUkLr8Ke5criDklfWs7n3piRhZPGIoi6/WdeA9hH+9MxVtyjpeesKIWdWTia/MZMnCHB6btc+OAddrB2t/cwgR2u2sN9W8qhsZhsacSKw9Iesxgi8eGEqPRheaWL7nHbLvuP5xhrtouuFW9gOmuv6fQkzmHBT9+nIrOzl1zULPsvDLmSy8ZteBgmBtGGnGjxj/qxeRAybwwh0D2P3tYfIBdcdHiQkJIu34Rp7IzQN1f17o/wTRFUt5oq5lrWDwrd2Z+83rzLJ6ETn4nxftQ7R/bT9ofxdd0HiDOTmJVFMRkIvppPGytTLjYog9XwGcI/ZDPZPeC2OU7n2WpjZcy4Zh5RtsrR3JLH3fvharUjtyPKGWeJ6ev4tUG8A5lq4cRPibEYR57Kud2+3qLgzWPnxkT9avOg3KYEaF+2KK30XqtTeHsv3M0qde/B1Up5xsayXW6x5nWImbmwdYSyhTP8wzY6bg+d0M1lpLQOmNpzNQZU+x9sn+cQdL8rKBs7xpCuOhwF704TDf4MpDgYNRnn+fWabaL4fyvbyZNZiP/HUEmPbX/caUZtrD5+WVQB67s7N5oe+FfYgbgQStPWxJbI0vZMns9eyIMJKankZiQjzxqUUNV8Jsyqr/MfMcBYxE01UBDYeHtJ7jmEOGi1QQqPNFFdCLj78de8myDLx9AHta8hcGax8yDN2qf5OuG0KYdwYb7JpQE6i2cKLYgvJKuweoHWe4sXWaNMBbFVRWllBeVgg2Gzg3ZWN7VZBfXD/OcvGvFeDsUvtF0pk+agWdujyLsdslm5V54QO1QVuBpay+37i4ouE+xI1AgtYuRegXTGL0tkGEDhxEWMQYXps4grBpU5h3sH5qHpVSCTQI0Ub/J9mwOXC4SGvyCoY+s+u6nqhIj0vANGEEo3Qx6EeG4W33hJr8Tl0HNsrKSsDNA8/yvXy8ZS8AXfp7gO0sxS3YmgWwXnVpBWmp0Yw5X060rgAAIABJREFUc/UbcFffh2jvJGjtVoE51cDWVANbP9zFjG2rGB7eC+XBC7NGKNEE9kJJzc/KwJ5oyEF/vuVbr1Yaaw1WkJ6aAxOCCfHYdc0pg9QuXvgoKsgvt1weyiY9samRTBwbiTpUSfKqRLsn1Px9ug4g15xF2YDb0KrhVO3NMK3Gl4r8Lfxob60AVGDjCt+J15THCQuM7twLnzPS3yquTILWHh6hzJgdhCluH8nmEtS6EYRpwLzx4mddNRGTed7wNlvMXXh0ejje6XHE2tWx2RQ5mM6DZuAQQrxzOGaxga0mLExxmzBMeJGoJZNRrk0g3epFgC6ECF0R6xdsb3BTzoUHBswluutZove8Q8xlza1c9sQZmbFwLMMtibykL7p0hSur7Tq4qhaYoqjS/Dkpxcu5/94p/Ji4l/IuT/GXbiUY/3ugic/RWjhTBpG+OgZl7sdYDVRX2tmFUc7uU4f5+12jWRVUwZums1gUnenjM4AHSGTWmbMyr50A2kHQDrpjHjE9vepfuHcFowHyN3NPoqGFWhml4DOIZ6PH4u2jhPwskrcuYvG2i/tok+ONaGcv5zONEssxPYvnxlz8xMHVeIxg3dczCal7YSp7v5sKFBI7bSLzDl5oGZeyZ+06whdEsvrLx1E1fLyrYB8vPadkzuzxzHnvcXwoId98muS4T5t8HQr08RhmDyLEEN+ECTV/R1WH2frFMhT3T2LK4/8LZacw7l/ApiY/Q1vO7hO7eGDAcP7zl5GoLnq869osBVv4++Fy5vYdzX96ekJ1Mfm/nOWrU4USsqJO23+O9o+g9jla5bL651nbvK5jiNk+HtMLDUNeCNEcbb5FK1qY0h1v726EzR5DoFnP0j/IHygI0ZZJ0IqLKAdO5bPVEajNRjbMj6l9HlcIcT2k60AIIRys/QyTKIQQf1BtOmhlSh0hRFvQpoNWCCHaAglaIYRwMAnaFjbojnkcDQvF54prePFI/zl8PWIFaQ+v4Ks7ul9xABZxfbwnrORQwhxCHXiBtU+u4dC2f6Bz3CGap/dkdhxcw4xLh9v8g1N6juarETN5up2NuCNB28Lyi9LYez7vioOIKL2H84If7E78f9zzxVxGH2+vf6bZkznbdvPZ9J6tXYj4o1GF8umIecy/4vx+7Y88R9vCMrO3MOsqyz3dvPCsyOabIosMQiLEDUKCtoUEB81lU+8uNT80Ms5CQI+Z7NZp637+z8ODAcg+/RYPfd+EVq1HEBNn/4OJ4b3QKCHfbCR21Rus3Fcz7sJFU+5YskiOXcfi5QbMgHJIFLsX+mPK9EIXYEO/Xo9q3FhC1VlsfWEWS1NKUQ58ns/fCeJYXBGBQ4LwUdkwG2KYN38X6TZAGcqSL19FvexRpsXVDBGpHBnNgdklvPzgYvRcMuVOwFqOToJLp9zBI4iJUf9gYmgvNEob5vREPnh9BVtP2jv1j91vTR3NsCjWRQVxbPEsXk6oGWP2alP/eI+NZvcMG4v/Mr9+0HRlCK9tf43A+Ck8tqp2jF6VF+ELVvJWRF/UtiySt77LvFXJ9SOedQ1lTtTTjBrYrf4Yr28i+cIKvYfx2uwxhAR2Q6MGizkN/dp3WRR3uv5z4TGCdV9Gkr9sHZaImnqVthz0i6fwckIpeAQzacnzPDWwG0rLceLj6sfQtcu13lcbBE5fQ0xELrHpXQgL8UVtLeTYxrd5+UOjfcNyuj7A9mEjCar98c57VzAegIxLBjdS4OMzgo8CwwhWQ37uV8w9vJek6rodcXePCbzQsxc93FywWs6y+/hGonNqxpdQeo5m9729OHw8m4CeOoJUkJ9/6T5+X9J10EKMadEE7ZxJ5OnCRrsNMs+sIGjnTO75LgPrr4f5+86ZBO2cydCmhCz+THwnmhkhRWyZP4tHHp/OyxuzUGtqB9XpOozX3owk0BTDtPGTeHqxEZ9xr/LWk/W/vqvUSjLXzmexQcnwKcFkzp/F0lQvRk0Iru8rVvUiTJvEyyMe4Z4xK0jvPZW3Zgfb15dcO+XOnQOmsCHTRvr6Kdw5YCh3DniowTgQ/kx8M5pnu55mzQvTeeSJ+XxwPog5b868vD9VFcSkKb7oF0zinnsnEvl6POZmDO7aWMhemPonpGATLz0xiUeeeZdjmvEsWTgEb6AgPh4Dg4gIr/8dVzkwnFDvDGIbDISu0oQxXJ3Ay09MYtrK0wRMeJVXR17Yxp9J0a8yztvIomcm8chzMWQGRvLWwhFc+C5Sqr0gfRdLX5jOI2Om89LGEkJeWcicgZeO2etB2JQRsHU+j9z3MKOfW4e+AMCd8KgonteeY81zU4icm4A6IowAB/RzqjSD6Gf6N4/d9wgPzU3CZ0oUUUPc7du4fC9jds4kaM9mjlYVsunrmv8DQTsvGUHOuTOje7iy48hbPHEwkZ+8hjNX27lucfBtz/HOba58k7qG8QlLmXWmnHtCJvOcR8N9+HG3j4mFe+cy+MvNpHW4eB+/NwnaNkQ5cAwT+5UQO38R6/elYTKdJnnbv1m8sWbuME34CEJI4u3Fu0g+eY7UhHd5O76EwJHD6m7WWPNPY0hJQ598DmvBaQypaSQn56D09m1wA68Q/fq4mhZsgYE12zLwHjKMkBa6qaQcOIaJuiw+WPA2sSmnMZ00snXxJpK9BzEq9NJwuTD1TxG2kiLS9+3DYO8g5DVHQxPRSMhSP/XPvPm7MJw8hyl1H0tXJkJoBGEeQEkisYYSQkaG1YaigrBRg1CnJlw8ELo1jQ9er7nmydtWsCZFSejIMLwBeg9jVL8S4le+S3zqOUwpu1i8MgkGDiO8a+0Zpmxi3vJd6FNOYzKdxrBxE3qzF/10vhefikpJfvy/WZxwmgJbKebUfcSnlIJHGOOGeJC88V02pJwmPWUXi9anOWawcUsSH6yvacFaUmLYkOJB+Lgw1C16kHK+Ob6Dz4vyMObtISYXevj41XzRO/UhsocXh79/nzdzznKiPI9vTJuJKerMQ/5+DRoDhew9ZeBENdisqezOr6zfRyuQroM2RB3YE2/LaZIbnQpHgUbrBWYjprphDStITz0HEb5olNQO2WjDClhtNrDasNjAYgNUyvoPoTWHzAaz9+abc7GpfQlQg+F6pm64cB7abmhUfXl++1c8f8myZO9LZqm4zql/VD5hzHlFicp2nC0XDaFoz9Q/FyarHEZ4111ssIQxKlRJ8jL9RQOhWwuyGkwSWUpmZg6E+qMBLNpu+FjPcaxBl4jFlIVZFUSgVgHnK8A7mImzJ9d0o6jroyDT+5JYsJaQnpJx+Ulq/PFRFXIsvf78LCezKLAGXb7udbLm52Cu+3zZMJsLYWDNudo9JOi1VFlIK7swTGUltupyUChQATaVHwE3uxIUEk1ayMWbFZc1iPuqYrJ/pcE+Kur30VJ1NoEEbRvk+A+K8uIpB5TK5k5BcEVWSyIvPzgf/TVP5jqn/rHmED/3bdKfXMiMhZEYnni/pqV+YfE1pv65MFnlqAh/9hSEE4KRRfrSK6zdHO6Ez45iRm8ji59bzJ7UXGz0ZMa2VYRf1v6yYbtSM/WS1221X6gtTqVEpeSiD2HL91BUwNX6UqsK2fT1aywsbnxx3VVrpf7YxkjXQRtiST9NgbonobrG5tuqwGwqBE03tHV9VQoCdf5gzsHclLBS+dJPW9/vFhDYBXV+DpkWuPCfXdWg5aXx7tjof7bGp9ypadFd+TyayhWNqxcal8bbDFZLGnsMRjYsiCFVM5bXpgTV1lQ79Y82mBCPRjetUTtZpTZiDE9FBGPVx102ELrKuxvausaUOwEBvnD+HCbAZsoiX+VPv97156rWdkNT91tDN0J6e2DaF0Nsau00Ph7+BPg04Zdc82nMNi80mvpt1F198Wk0Aa90vex7Xy8+Vw+0Wi+s589htr/a2hmKmzk5pTWbzAo1wT5e1173D0SCtg2xpWxnwzEPRi18lYl39USr7UnIsMnMGVtzs8usTyCZQTwbNYIQrT+6YVN5PsKD9H0J9k0VXseD0ClTGaXzJ/Cu8cwZ2QvzvgSSbYDtNMdOQuCQcLRKoGsoT428dApYuHjKHXeUSkVd6NpStrMhueY8Jg0Jqj2PMUQtf55R3k28KB4P8J9h89jUV3v1/jfTdhatTEMzYQ7P195kMsVtwkAYUUsmE6HzR9s7mPCxk1myYAyBDTZNj0sgXTuSiaElGOKMl/9GoQriqVdqrnnI2Jk8O9CGITaxppV8ch+x6R5EzJhac4yBI4iaMghSE4g/X3Od0s/b0OgG1VxPZRfCZ4wntCmdniVJNX3JY8ej8wA8gpg0YVDj/aZXul72vq+XnOtTupL6c7VXZSHmCleC/fqgcXJB2ZQUqj5BzJk8evSZzOv+vejj2pngjv15+o7JvOz9x/0F/Y9bWVviMpiP/jKBwXUv/JUDD/8VKGbH1/N4pQlTbl3dOTY8Nxei/sHEJauYU/t4V/yq7TWLz+9i3stdeHV2JKu3e4Elh9S4N1i09nTTDmPNIN7gwaR31hKgLCF93wpeWnYhYIqIXbmOsOhIPvtyPAVmI1sMaVhHXbqTq0y5wzk2vDAXZv+DiQuX87waLOYs0pN3EdsCfcBXYtr2Bm+Hr2XOwqkkjn8bg71T/5j0xKdH0s8nidhG+out5kT2WIax5NOZNY93bZzPooQLb/pp1r+wCJ+FT/PqeyNRU0hmyjZemr+rthVYxJ5lKwhZMJWPv4zEainEZEggPr1bE/7arBT94sWsWfg8b30xsuZ9TzZiDmzKXwTY975aMxPZYxlRf65rFzMvoaldKSd45/sjLL9jMnt7Krj88a6rM55aw3NVo3mhz2Q2ubnCr4WcKUojpsy+6YdaQ5sej/aPUkd7UvMcbU82jJnBhibd3W/PgpizbTmhhlmMXp7W2sW0msDpa4gZYiRy7L9b7sbXDUJatEJckQK1ty+BI59mlCaDD7bduCErro8ErRBXogxnya4XCbNloV+1mA2m1i5ItFXSdSCEEA4mTx0IIYSDSdC2AJlSRwhxNRK0QgjhYBK0QgjhYBK0QgjhYPJ4V3vj3Je7753BsIBeeDtDseUoCV++gr6gfU6YI0RbIEHbrnTj7uHLGKc+yhdfL+BUsQ3Pjt2hSkJWiNYkQduedJ7IME0OOze9gv7CmAF5h1u1JCGEBG270sXvdjx/PkJF32X8vz598aSEHzM3sClxJ7lVrV2dEDcuuRnWbijxVHuguOUBhvmcYueu2azefwB6zWZa2GD5RhWiFUnQtifOgEsW+v9by7GC45hMq9j0w1m8Ax4goLVrE+IGJkHbbtgoKysB63lyyutfLfi5EJTeeDq3XmVC3OgkaNuR3Nwsypy74u1a/5q3mxfYLBRLH60QrUaCth2pPLebY2W385f7JxJ0Sze6aKYwrl93CjL/S2ZrFyfEDUzukbQnVd/w6Z6VPH7fozz16LO4VRWQmbmMtYmH+eNO8iFE+ydB285UFmzh4+1bWrsMIUQD0nUghBAOJkErhBAOJkErhBAOJkErhBAOJkHbAmSCSCHE1UjQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg0nQCiGEg7m0dgE3BhceGRxNdFcFANYf32dwSiq2Vq5KCPH7kBbt76KSzw/PIWjnbF74saK1ixFC/M4kaIUQwsGk66ClOPkRGfwokV398HOqJLsolf8YN/NZSaX9+1D14eX+oxnt0wVPyvmpJJvdqe+zpKC8bhVN5xEs6DuYwR084ddcDp/ZwYJTJzA74JSEEC1DgrZFeBE5+DmmKQ4TfXAjxl9dGdxnAnPvGk323i18U23PPlz48x1PMF5xmLn71mKscqWHVx96NFhD3fFRYkKCSDu+kSdy80Ddnxf6P0F0xVKeMBU66NyEENdLug5agNLzPiJ9cvnP4R18XpRHZvlZPjPu4bBCx+iO9n6XudLpZheKS9L4pqQQc3k232TvJaauNevKQ4GDUZ7fzCzTCYzlhRjz9vJmVjHB/joCHHVyQojrJi3aFuDZwQ8/Zy2zhq9g1iXLDt9s714sHDCd4On+k/miQwbGwmyScg6zIy8PCwCd6aNW0KnLsxi7XbJpmRc+QOb1nYYQwkEkaFuIteI7Zu2OYe917CMz+32G5nfngc69GNx1ANPuuo9Hjq9g/Kns2kfBKkhLjWbMGekmEKItka6DFlD8Szb5Ci13e9rzvVUJzldez2Y9y3/P7WXh4beYlWmhh28f/ADI44QFenTuhU8L1S2E+H04+3TyW9DUjdzd3SktLXNAOW2zjiprHnS8j6e6d6WyOJ8iPLnNS8ff+/wPHX86zom6m2HVVKl0jO/eBWt+Jjm/ucBvNmy/AXjxSNBw7nYqp6yyGrV7EON7hdCl2MC6nDxsVJJVfgujAu/nPudCskpteLp1526/YUzrWMzeol+oar1LIIS4ipv69B38W1M36ty5E3l5PzminjZZB1DzeNcdo4m8tTt+Ciguy+NEbiJvfm/AWH3JeiGRTOvaBU8qOHBoLs/kVAJqHgiawLRbtfRwc0VVUUxa7n6ijXtJavCEWEDnEcztO5jBHp5QXUz+L2f56tRmluRZfu8zFkLYSYJWCCEcTPpohRDCwSRohRDCwSRohRDCwSRohRDCwSRoW0Dnzp1auwQhxB+YBK0QQjiYBK0QQjiYBK0QQjiYDCrTzrjc8jCP3z+Rft5dUdhOYUxayabjRym/9qZCCAeRFm174nwn44bPpl/lXj7YMokVSTkE3LeA8Vqv1q5MiBuaBG074qL5KwM7niJBv5a0nzMwHV/GF1keBN9+D66tXZwQNzAJ2naki6YbbmWnMNWNL1OIyZyDwqcvt7ZmYULc4CRo2w0lbm4eYC2hTP0wzzyxm9m6XpRZS0Dpjadza9cnxI1Lboa1N1VQWVlCeVkh2GwgAStEq5OgbTdslJWVgJsHnuV7+XhLzaQ6Xfp7gO0sxTIquBCtRroO2pFccxZlbrehVV94xQutxpeK/OP82JqFCXGDk6BtRyrNn5NSfBv33zuFoFt6oQ2czV+6lWD84YA8RytEK5Kug/ak6jBbv1iG4v5JTHn8f6HsFMb9C9hkkllzhWhNErTtTOXPO/l4+04+bu1ChBB1pOtACCEcTIJWCCEcTIJWCCEcTIJWCCEcrE0HbV7eT61dAvDHqUMI8cfUpoNWCCHaAglaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMAlaIYRwMJfWLsDRNNqZ7A3W1vxQlkjkl1tIatWKhBA3mnYftGbTCoJM4KN9jq9ua+1qhBA3Iuk6EEIIB2sXLVpN5xEs6DuYwR084ddcDp/ZwYJTJzDbvYfOPHLHX3muW3f8FFBclofx1GaeM53FVruG0rU/c4OH85BPFzwpJu38XqKP7Cep2jHnJIRoP9p80Ko7PkpMSBBpxzfyRG4eqPvzQv8niK5YyhOmQrv2ofH/Kwu6QcyhpWwqgU4eWh5wa7CCi47l9/6VHnlxzNp3gmyn7kQG/5Xl/YsZk3KEfMecmhCinWjjQevKQ4GDUZ5/n1mmEzWtz/K9vJk1mI/8dQSY9pNpx146eajh1xMcKMjDDJiteRgL6pcHdHuAeyoO88QRA0YACok+HsQDdw3mbqcjfC6tWiHEVbTxoO1MH7WCTl2exdjtkkVlXviAXUGbds5AWsBo/vNgdw7nZ3Mi/wg7sjPIrAZwoU/HLqjUWjY9fN8lW2bjowLKW+BUhBDtVhsPWoAK0lKjGXPGvm6CxthK9vP4njTu7hzE3Z378IDuOR7z28z4bw11QW3N3cg93x7G0jJFCyFuIG38qYM8TligR+de+FxjTWsV4KTA80orVOfxTc5+lhxby/jvUsFHx2AngEpOFBVChz61PwshRNM4+3TyW9DUjdzd3SktLXNAOU1VSVb5LYwKvJ/7nAvJKrXh6dadu/2GMa1jMXuLfqGqbl0/HrotiC4lGZywgctvNsp+q1kS7D+aSR3AUmHFWdGVv/S8h6HKTD44XfPkws+Wcm7vMZTHfKrJKf6FKkVnBna6h+cCfMjKPSc3w4QQV9Xmuw4sBVv4++Fy5vYdzX96ekJ1Mfm/nOWrU4V1j2YB2Ir3EH3cjwXBc9itgJ8y32HosQxsQHG1K8G3TWB0sCeelJP9SyoLDsbV/wVZ5RFmfQ1zg4cz996RdKKcn0qySco6ISErhLimm/r0HfxbUzfq3LkTeXk/OaIeIYRod6TXUQghHEyCVgghHEyCVgghHEyCVgghHKxNB23nzp1auwQhhLimNh20QgjRFkjQCiGEg0nQtkUeI1h3cDOv3aVo7Uralt6T2XFwDTN6t3Yh4kbT5v8y7IZkyyFxXxKWgopGFioIXbCBtzQxPPTMLgoaWUM01MLXy6kPrz8wmYBT0Txu53jI3PIwfwt7GK1PN3zdVBz/vz+zOr2kwfI/M/Z/Hia4Sy883aCiOINjR1ay9fhxGTiujZCgbYtsyayfm9zaVYhGaPyG8pBTKrOymjCanNIDLIfQZ/zA/9z/yGWLXX0GE1B5Cn3iJ/xYDF63/S/j7luGq+0p3svIacHqhaNI0LaUrqHMWfg0o3TdUFNCvvk08cvmsfRgKQCB09cQE5FLbHoXwkJ8UVsLObbxbV7+0Nhg6EV/Zny6lvDUFWxQjuCpiF5osJEZt4jHXk/GphzG6m9fJAyAQmKnTWTewQutWgXhy7fwVrhH7c8z2fvdTADyY1/moQXJtWM/dCT0yZk8P2EQWh+wmdPYs3IFSxPOXTQ2xJVoJqxkx5Pn+OeIpRgubOAxjLe+mIpy2SSmxfsStX05IfrpjF5+usF2y9nxZCEvj1mMvqTxfbc4j2AmLXmepwZ2Q2k5TnxcboOF9l6vpujO32/rzpkzS9nblMHg8zbwcR7g/GeCGwna8oyFLMtouH4hvv5rCe91Oy4ZOVQ2uU7xe5M+2hbhTsTsFxmnNrLomUk89Phc5n14HDPKi9ZSaQbRz/RvHrvvER6am4TPlCiihrhfsi8lmohIhhdsYuqDD3PP4/NZbyiq+U9vS2DagKHcee8Kkq2X1lCBftYj3Dngz0yNLcSavIIHBgzlzgFDGVoXGgp006N5a4IHicvmEjlmOi9tLCFs4UKe1dnX32vW7yNdPYjhA+vXV4eGEUIa8foisKWxNT4HTfgIdHWn70/4sCAshngSf6+QxZ3wqCie155jzXNTiJybgDoijADVheX2XK+m8fEdykM3nyAmM68Fz6MxSlycodhSICHbRkjQtggPfLyVWExJGFLPYTalYYh7nw0Hiy5ezZLEB+trWrCWlBg2pHgQPi4M9aW7y9zOvFUGTCUVWExGYvedvnSN5lEOYuI4X5JXLWJlgpF002kMG99lQ7ovw4f1uuRr4QrOJxKf6kFoRHDt+u6ERgRDqr6upZoel4DJexCjLoS3NoxRgYXo44zNCrBm8Qhj3BAPkje+y4aU06Sn7GLR+jQu+35qMZ2JvK0Plqyv2O3g9PMKfIr/Uf1AwrGjjj2QaDHSddAickmMMzJp9qvs+DSN1PQMkvclELvv9EUzMljzczDXtehsmM2FMNAfDZBO/esFJ087ZvhFbRABag8CF27h6MKLF1nOd7RzJ7no49OYMSWcEGUyBuUghodA8rLE+nM16YlNjWTSyGCWpiQTMHIIWnMii1Iau3l3ZUrfyRz+k46aRmgxu7+Zxyx771Zp/PFRFXIsvb6v1HIyiwJrUJNqsJfaeyiPqM+y7vBZh36ZuGpmMSXMl+P/N4NvZLqPNkOCtoWYts3loZRgwkNDCAkN49k3RzJq/Sz+tiqt/j+eSolKCQ3/J6oa2ZfVanPcf1ZrDluem8TiJoZeQ+Z9elJnRzJ8oIJUdTghGFmkL22wRi574ow8Oz2CMI9SQoZ0w7RvKalNPI4tbzPj98bV/Vzc1LHmL2m+2rA5qEWrZvRtOjj/ETsc12TGVTOdmcPvoeDrGaw3yU2wtkS6DlqQzWQkfuP7LH5uCi/FlqANDUbTYLnKuxvaun4CD7RaL6znz2F2RDHKRjoCTGlk2rzop/O1YweuaFy90Lg08l1ckMieVA9CI8IIjwiGZD2GS/peC/TxJKsGMWrKCMI1GeyJa0b3R7WFEyV5df/MTbnBZD6N2eaFRlN/HdRdfWsm02xMY9fLTkrPoUT6FPL5qRMOm1POtfN0pg1/gILE2byXkeWgowhHkaBtEf6MmvU8k4YFE9i1C1pdOMN1HlhMWRc/l6kK4qlXRhCi9Sdk7Eye0pVgiE1s4f+cFZhNORAwiOG9O6JUKuozxJbEhq1ZaKdE8drYEAK1/ujuGsKkBdHMGXLJzTCPB/jPsHls6qttpO+2CH18Gurwp3k2BJLjky4/h5JEYg02widG4J26j3hTi57ktZUkEWsoIWTseHQegEcQkyYMurw//GrXyy6uPHTbYNR5XxHT3Bt9zr508e6Ln3c3XAHXW/ri592XLq41hbh4T2HaiIdxzVhLQr4SP++a5X5qr2YeUPzepOugRZRgoRsTZyzkWY0HKmsh6YYYXl5muLiPNjORPZYRLPn0/7N373FRl/n//x8BMyPIYBxEHFYdwAQsxAPS7pgHwsR11VI7mP5WMyvb1HJXbUP7Krqr9lmj9VD70bIsyjLzkGgmph9QA1PxgFMOKMqoCwJxSIZDMxz6/eEBRNDhMCr4ut9u/jHzPsxrRuY511zva65rBmrLeZJXL2LerpJ6z3o9BSFz1rIMXln0AAAgAElEQVTmyRqt0fe+ZSTAif8Q/tzma6Fu3BjDOt0MXv7kK2arag5XKke/MpK/mmbw2qQ3iZnrDKZsjPrDrMtqWFdCfkI8ybNm0M+SyPKkup5DOYmxh8mLCMMYF2+bVvtNlRC/aBGrFrzGv78ZAaZs9MkpZPnf2Bdd/+tlBUcdEzqWsWPfscb3q9//FC89/TTX/md7RzOnN2QfncI/Dp7EpfPD+KhU8OAcZj9Y47jMFbwR+5WszNwCtOilbO6WOqzhP30VMYNSmDDmf2tc+Grd3Ecs4etZsPRPkcTetmFdt5MD4b3mEe20k2GJSXfgw0S0FNJ1IJqf0hVNt0G89lww5qRtt+8HCredG8riQyzRH5KQFTclXQei2QVNj+bT8V7knYhjUa3uk9Yll29Pb7/TRYgWQLoOhBDCxqTrQAghbKxRQXu3tCLvljqEEOJmpEUrhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA2JkErhBA25nCnC7iRE/cHDMXX+3e0UVRQcnYDKafy7nRRQgjRaI0MWjX3+wygi7YLbVVgKTrHxZ++I/OSpckF3eemo6u3PbnJa8ksqcSO0iafUwgh7qTGdR04tMft/lKyUjZwZP9WMs1edOk9AJdmKEipdsWh5DQ5l0xUVpRSXtEMJxVCiDuocS3airOcPXb22s2LGefw7uOCygG4LhjtcfZ5nK7a9rRRqcB8htP7vyG/yoP2D4bh7dmeNnZmSrKTOPWTAXMVYOeA3f0D6DN0AACFx9/DkF2Jwk2Hb0Ag7dqqoCyTTP23V1rQHnTo9Rjebq60UUBFQRIph45jdvRF212Hh5sLDhWFFKR/x+kLefyGPS4Bo/Ht6EYblQq7yhJKcg9yWv8jpVVXqm7XB78He+Lm0hbKC8g+9gXGgpvV4Ip70GC6eHWkjX0Flp+T+OnIccoa9eIKIVqbZuijtcfZ0wsHk4GiG1qf9ji6eWGXt5NjadlU2dtTVWGPS/fh+LU7x6mkbzA5+OIbEka3Tv9Ff84EQNUv+zh2SI8F+K2qEhx70q1XIFXp33Ass4S2XYcT0PtRSvbu5JcqF9q5uVCS+gX6HDN2igrMeNC511DcSuI5ufccv92vwz94KF1++QyjyR5V2/bY5ezkWHo2OPqi7T2AB7pkkpJRCI4PERASAhe+48SxbCrtnMB88xoueejw87CQkbSKvDIH2jjCr01/YYUQrUSTRx2ovIfSrWMpGfojmOvZp8pchNlSSnmZiUq7Lnh1VFF4ah8FZaWUm37kQmYRbTt0wb7mMVWVl0MWcPQKRF1mwHgum/IKE7+cSuaSgzdu6qt7V1BZVkh5RSnmMgu0C8KjbTaZBgOlllLKcg+SY3KhnYdr9fkrLtdkvvQjmRdLcXTzwh5QeQWhNus5e+ospWWlmEvyMFfcooYKM1UOTqjbtoWqUspKSvmtqS+sEKLVaFKLVuX9OEEBSi4mbyWnxNpHdEKhUNGu91R0Ne8vUqEEquo4ROHohJ1LCL2GhtS410yeyr6OveE+lRNK+9/h9+ir+NW4/9ciFXBjoeVmM9yvwg5QOTpBWeENLdKb1fBb7j5OnhyAT+CzPBz4M7np8WRk5knYCiGAJgStvcdgAgOcyE3e1LDRBhWlVJSbyUtZw6m8yhs2q+o4pLzMTNUvxzn2Q/2t5pp+qzBTUXmOc/Fb+fmG7gzlTY8tLzODmytt4Lo+1pvXUElp5m5+ytyHo/djBHYfSqeizzhvsqJYIUSr18iuAw+8A7pRmb6TC6ZK7rOz5z67uluXN6j6Lzm5Fbh1C8O9nRqF0gmV2gvHm0R+Wa6BEnVPfH0646h0QqF0xVmtrv+AX34kr8ybTg8+hEtbJxRKNY7tPFBYUV5Z7o+UOAbRtZsvTo5OKNp64aS8RQ1KL1zUTtjbVWIpyqYcFQ7WPJgQ4p7QuBat8ne0c3ZAHfhn/hBYfXehfhWGzFu1bi388tNWzjwYRpeQP9NG4UCF+SKZR78i81I9h5QcIfWICt+Axwj2b4tdpZnS3P/jpxQT5XXtX5XNuSPfQfeH6aZ7FKV9BZYiA+k/xPNLXX0T1z3WcVKPOOEbEEYP37ZQXkRh6ibSMuuvoer+h/AN6oaTwoGq8iJMmfFkFtzicYQQ94z7ArqHSleiEELYkMx1IIQQNiZBK4QQNiZBK4QQNiZBK4QQNiZBK4QQNiZBK4QQNiZBK4QQNiZBK4QQNiZBK4QQNtbooNWOiWb/3mjGa6/e40pE9AYOfjGZoJvP2yKEEPeURs/eZdz0L5bqVjI7aizJL63HFDGD2SHZrHgxBr01k3k5D+eb8MH41n12oncuY401U3UJIcRdrmlzHbjreOuT1/FJjscUFgYrpvPCpgvWHWunJsDZrc5pEaGMzKJcZO1bIURr0ORJZdR/mM2X70WgTvoXz0zbRVYDj6+vl6Hp6+kKIcTdoYlrhinQhvjhDqANRuu8i6xiKw+VrgMhxD2iSS1adZ/XiHm3L/rIZeRNWchI4794JjKBfGsOlq4DIcQ9ovEtWmcdsxdEwLZIliakYM6KIeSTV1g4wsDUbTm3Pr7KRGqRrPUihGj9Gjm8y5WIBTOIMG9j0dspmADLqfXMW5lNj1mv1xjyJYQQQlZYEEIIG5NfhgkhhI1J0AohhI1J0AohhI1J0AohhI01Kmg9Pds3dx1CCNFqSYtWCCFsTIJWCCFsrPUErUMonwxfyGLXencgWDuZb4YtxfD4Mgz9w/G5nfVZTUHInM84+PHoy3NICDRjlrD7i8n43+lCGq0tYUs+Y8uc4HonUWoaB54IXcrBXgE2Or9oqtYTtFUF7M/Uk/JrPdvtgpjavSt5J5cR/k0koYl7ybitBVqrnDz9YeKPnK9nBjM/Zm/awZfT/W5zXXeIMoSXJ/mhX7ueNJs/mCNPhC7hYJ9eeFh5hFunKUwa+RlLnt/Ney/uYOHoxTymcau1Vwnxa+OxREzmKW3jq3ukx0KO/15ndW3i7tHE2bvuIlXprDmWXv92lRsedkUcysskq+L2ldUYxm3LeeNOF3GXcI8YQ5jyMH9PKLH9gznrmOBZxI59x6yc0EhJB59eOOV/y9cnTpNjceeBXlN44k+LqVj/MvE1p/I4tZ3YjDE8OSaQr6INMg3oPablB61dL94fMZH+ABSxZd885hRWb1a6jGJH2EC8r9wODF/GRICCbQzbv8fqVq17n7G8OWs0On83MBeQn5bI8sjlxF0ElIP493ev4x4fhyWkH/5qJaaMRJZHLSPOWF5dy5AF7FmgZFVkOrrpIwjxUUJeCktfimSjUUHQzNV8Or7z5Z1P/Ifw5zZXz4SmDGbu5mie0ly57bOa45MALCTOf5yp2y4/jrLbEGbPmsDQIC/UFJCWtJml89eTbOX0ldox0Xz6KqyaOJN1Rri8RNFqFmrieGHih7dePcNxICuDvckw7iUmO7OJM7C5EhYRDEf+SXKNx3Uft4Id484wdfTy6+4H0Dy3gi0jDEwY878NbAE7EP7AQHzzdjKtyNpjLBj2vYyhxj3GQme6P/cqPTq5EX+yoMaWHBIT0nn1ySH0WGm4oe6b1tXnH7z7O8crt59m/+NPA/BzxrsMPpFeHdp2HRjVawRTvb1xKc9kT8qHzMyurkHjOZyo7qGEtnOBX3M4dHYLUadTr8whHUB0xGQ8zm8nz2Mgj7RTQ2kq7x36hJjiu7xl0gK0/K6DqmO8tHUGgd98zqHKGzdbirYweOsMAndtw1CZwwd7ZhC4dQaBDQhZlMG8vGAC/sY1vDB6PM9MXMTyhByun+NRSQ+dG7Evjqf/Y9NZld+XhUsm3NivqApk0hQv4qMm0X/AeCYsjiPLDFCOPvp5evb+Iy9szOaGqXgtKSwaPpievaewLsNC2top9Ow9mJ69h10LWdwH8T/vvkJI/nr+PnEST7z0H05oxvLWgkFW9/caN/2LpcmdeDlqLP5K0Iy4skTRfCuXKDKn832xmvDes9kdMZuVgToeUTXy81wZSEgQZOivbwHm6w1kufvhf8OTUhAU1BnTqZSGdwupQnmhI+w4fajBk9dfx8ENJ0wUmW78ZMvSX8DkHkiQpo7j6lXBniORBG6dwYsZRZhzNtB/6+W/4QE1QxZw6TiQ8EvbmJ7wb5bkOTIseDjhV97hateniAnpjdn4ORN3LWTisUOofCeyRFuzm0NB6O+6sD95MQ/vWMp7pV3520O9pauiGbT8oL0tOqBxh6zkw+iNORhPpRD38XrijNfvlbEthtiL5WC5QOzH8WRp+zEyqPa5LCSt+Bcb9YVYigtJS0gg6WLzVKkdMRadKY5587eTdOoCRn0CS1ckgi6Cfs7WniWH2MXLSPQYy8I5r7FwVjDGFUtZd6r81ocCVGXy5Y+r+dOO/8fElKOY2g0k+rF/sPv343jB07Nhb1q1Fxq1hfy8WqGVlkKapTNB/gogkPFLFvBqHwXQlR5aJcbkhn41d6DvA4MJNO0lJr8prTc3ejz8RzrkfcuuCzdWYMm/QD5u+HRUNOEx6qOAS3uJOptKSvE5vkw7ylmFN32dABwZ5h+K8uIGZhpTSSkrICV3D++cLyK4U9B1F4UNxp18XVYBVbnsyMyEdl0JsEG195qW33VwO1gOszGugLdmrWVLRAr6NAOJu+KI0xfW3Iks4/nqmxkXyGcEmo4K0NcIKfMFTuitDK0GUeAf5IXKpyuf/jCm1rZ03D0Aa1e/yE9i0aJEvnxvBJqkf/GMtevAXcdESvYeUrL3oHbWsTh0FDP/4AkNWTlDefkaurl2ZlkM6NOUPBnUGWWWjpGD+qI2B7PK6IW/JpsT1/2/WMEhiBc7O7L/aBKpDTuyBme8ey3mz97pfLF5NZl17WK2YEaJsu7Z7puonKLinOqumnITRShQ2QF4EqBW0L7Dy6R0rnVYqRsecOUbQDmm0uquhqLycrB3qGdyftEQErRWKSQ+ahKjNvVF16cv/SJGs3D8cPpNncK8A9UXaVRKJVAjROv8C7VgseGVEHPyMga/tJ2mTanehCWKrnEkwLMXz2j7Ee7pCZf0fJK8l68bsjyRqQCTWYlaXet1pZATadm86h+IT353iIsjy78vPYKc0ZrOXOlbtl6Az2BCfz3ExOyyhh14jTPeQYuZ1sPCju1zOGiq5z9Y3QE1xeRbtQRJcyvHoF/C6LMFN91LVo+yDek6sFo5WfokNn68nL8+G8m6DDdCwrrWGLeoRONffVvp74eGbLIuNn/r1Uxdi1qWk6bPBm0wIVZ0E6gd3PBxVKOua1ufV1g4zpm4v73BOlMYC+da38eLnTdPBE5mc8RCtoQMxPfXQyxJmMeA/TG8lXmuYRfHis9gzAJNt9rNMEg7cgaTNpjxOldObNtMojmQkYP8UBtTONGQDzK7AF70dSPl9G5SGlLbNUq8uy9mWl8l/7d9DvH59T+4WuuFhgukZTThb8K+MQflkmoCX8+u0t96h0jQWsNZx6tRkxnZxw9Nxw74DxlOPw1kpV0/1lUTMZnXBvmh7abjtelhuKclEqtv7mKyMV4ETZ9BhLi3RalUXAtd47b1JNGPuW9NJiKoE9puwYSNmcxbUaNrXZRzILx3JDuGTGRU7Vb3tSWKlrI0IZlV82PIGvQKC0d0sK48pwCGecChH1cTvnMJE0/s5dvixrYUzxN/pABNUCC1rx9Z9CkY3fsRoT1Dkv4CSXolERGdydIbrFuz7gqfzoMJr9LzQWZjvgMo6dD9bab18+JU4kecpDPe7t3xdu9OB8faH4UKgnSBoE8kscHfDi7LLCqAdkEMc3ZEaefQgB8nlLHj9CHyPEaxMrAXfR3dCHAJ4Anfcaz07SI/crgNWnzXQd+H5hHjV+PK6YBljALI20D/xKRmWuCxBDz68vKSMbh7KCHvPMkb/8miTdf30SbHpaCdFc2XGiWmE/EsioyxfoiR83DW7JtByLU7XmHP0VeAAmKnjmfegautoBJ2rl5DWNQE3vvuWVQ1h3flJ/D3aUpmzxrL7PefxYNi8rLOkLztCytfh+oliqZeWaKIU+uZt1LHp7NeZ7x+5q2/lhfv4aX91j7pWykneWM8xk8GEaHdzNqaj51/hrQsJf7GRJItYE4wkD/eizT9+fpOVocuTPDtwtkzG/i+qjH1OaP16YWLA4Q8Gl3j/w6yj07hHwdPVt+h7MtInZKkt+Mb9EFQU8Z/txPjNY6pg5YQaV/H8K6bMOV/xYuHyojsPooP/Fygqoi8S+fYfbpAxvTeBo1aysbTsz25uT/bop6W6co4WuXb1eNZRXO5PI73tfxFPLE4pVlDwcN7Ct88VEHUng/51sZDRTVjovnyyZO8Ys1YZNHqtPgWrWjtCol7exmaCFc0gLHZzuuAN5l8mXKIPTYfj98WDSdZvni9hOw9SoJW3P0uJrH24+Y+aQUpmdsbeQGsoUpI3vQhybflscTdSLoOhBDCxmTUgRBC2FiLDlpZUkcI0RK06KAVQoiWQIJWCCFsTIL2Tug2lphd0TzZ8U4X0li2XppFiNZFgva2cyViymg8EmKIbabpEevX8KVZuP+PjBm6ioUTd7PsL7tZOn4Vf+7eHcfrdmqepVmEuFdI0N5u2gjG64qJ39i8v3Kq09WlWU5buzQLOHqE4lNxmvjEKN7bFMVGI/QY+DZ/7up1/Y6nthOb0ZUnxwRKq1aIW5Cgvc20gwbhn59CvLHGnd0ms+XAKiZp6zgg6C9sObCC8Q3uZri6NMtePrB6aRYoS1/A27ujiU//ntO533MwcQXfF6rp3vXBWr9uubw0iyZsCD0kaYW4KQna26otPUI6Q+2p/IwG0kxe9OjW9oYj3Lt1RZNv4ERDZyJprqVZUOJgD0WmfGr/UrVxS7MIce+RoL2tvNB2VJKfVWsFAIuBZKMS/5CuKFEQ8twC3hoXiBIF/iGdsKQZSGtQP0NzLc0Cbv7P83vVT+w6cfyGbbZdmkWI1kPmOmg2nvxt0BxebHf5ljlvC8MS99ZqTSqvLLpQOzUL0euzce/jh1qpYOSTfQkzZ+OzsZgQrTMZ2xq4BlazLM0CjpqZTOnnxcn/e5Xv65qu1aZLswjRekjQNpsCYg4uZsfVGfDLTXVcgCogz2RBqXa9YUtGsgHTiEB6+HfAPyuOOHUgOu0Z/DXZ6PU3X36ktqYvzQKOmunMGNqf/H2vstaYXfdOd3RpFiFaDuk6aDYV5JXlklp85Z+5rI5WaAFpxmLUHb1uGG5l0Rswqv0IGxOIWb+dWL2SfuP64sMFTqQ1YI7bJi/NAo6e05k6NJz8xFm8n17/RNrNsjSLEPcACdrbqpzkhBQsPn1vXNer2MAJoxcREc7oE85zIv4Mmoh+uGekNGgO06YtzQIO7lOYOvxxHNNXsytPeW1pFm+1W609m740ixD3Cuk6uM0sSdvZaVpARJgrsdtqXhQ7T3JaMc+7G0hMK8fCYdIsEfjrDQ1YjqepS7OAS+eH8VGp4ME5zH6wxobMFbwR+1X16rrNsDSLEPeKFj0f7d1SR0NpxkXz5YiTvPDsh9avKWYFWZpFiLuTtGjvgKyN/+Gf7sG4dwSa7We4sjSLEHcradEKIYSNycUwIYSwMQlaIYSwsRYdtNJtIIRoCVp00AohREsgQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDYmQSuEEDbW6oNWo52B4fFll/899hR973RBDqF8Mnwhi13r30XtGs4njy65UvcUnrkr/5cUhMz5jIMfj8b9TpfSYrQlbMlnbJkTjPJOl9JY3Sbz5a4lPNnRNqdXuoxi9/AZvKCyzfnvFIc7XYCtZRmXEWgED+00dj9wp6sBqgrYn6nH9Gt9O7jxzEND8bn0FaP26cmjnKKq21mgtcrJ0x8m3nQeS53b/Zi9aSUhCdN5ZuWZ21zbXSpoLK/qclg3NqWe16z5KJ2Hs2VQEPsTlvBWsRUH2Hfn4d8/z6Pa7nRwUUPpOU6nr2PjD9+SU1ljv1Pr+Ui/ltemhBAbldy456HS8cVjg0lNXMiCwsacoOW5K9tKrVpVOmuOfcWXZfXtoMa7DWTmG0itKCOvosLmb8rGMm5bzhsrkzHd6UJaBAVh4yLw0McRd9HWj+VIuH8oHrl7WGNNyAI49SLY3cTJYytZvWkmq3/4CZfuc5gx4JFarbES4jceRhk2hqHyVcZqraJFq/EcTlT3UELbucCvORw6u4Wo06lkWX0GT5546Gmmde6CtwKKSnNJOb2BacZz10JO6diLyOChDPPogAtFGC7uYcmxvRy2trVp14v3R0ykPwBFbNk3jznXfZoHEB3xMsPaXLkZ/A8MwQAGorat5ksrH8e9z1jenDUanb8bmAvIT0tkeeTyK29uV558fx0vE0+SMhidjzMq0xli3/4nSxNqFOM8nDXfTSDv7TWYIiYwNMgLpSWb+EVTeGOXhaCZq/l0fOfL+574D+HPbSb/6rHKYOZujuYpzZXbPqs5PgnAQuL8x5m6rfzKYwQyfu5fGK/rikZpISstkY8WL2PjqXKrnqd2TDSfvgqrJs5knfHyc4uIXs1CTRwvTPwQfaM+nRzwcQ3lGd9QXC68y5zcisacpG7KvkTonDmx4nD1B5MyhIXbF6JZ/RQvbCqptb+Ot7a/jurt8fx1V0nts92cYygTOlawI/EoedYeY1rH+7E1bucep6LDw8zw6Y8P33O6xibLkUSSzW8ycpArsZsa0CR1DGfzkBEEXrnZc8AyxgKQzpKd7xJjvrqjAg+P4Xzi349gNeTl7Cby0J4a7zVHHvEdx9/8uuLr5IDZdI4dJz9nSXYBFsDHdwZb/AqI+a8L4Z274K0ow3Dmc6YbUq1/PZpZi2/Rql2fIiakN2bj50zctZCJxw6h8p3IEq2b1efQdHqaqM6w4+BSwncu5aWje0mt+XXJIYjoAU8Tat7LzISFDNu3hRSnoUT36oWHtQ9SdYyXts4g8JvPOVRZ1w6pzIybQeDWd9lSWs7xlP9H4NYZBG61PmRRBvPyggn4G9fwwujxPDNxEcsTcuC6/i4lHiGBsHo6gwc+xQufw8gFb9bR5+ZMvynDYeN8nhj4OKOmrSE+H6AcffTz9Oz9R17YmI259mGWFBYNH0zP3lNYl2Ehbe0UevYeTM/ew6pDlk6Mf2cJL3c8w6q/TeeJifP56GIgs9+Zgc7Kzkvjpn+xNLkTL0eNxV8JmhEzmB2SzYr5MQ0PWQdv/ug7jk8e/Qc7+g0lmHT2m5oxZAGlfzA9lNkY9TWCyWIg+RRogwJv7LPVBuKjPs+JUw0MWRx45IGBBBbuJaawac/BQamE0oLqD9GrLAb0RvAPqaPumynbw+itMwjcuYHjlQWs3zfjyt94zZAF7D0Z5evIlmP/ZuKBRH52G0qk1vPa5uAHpvHuA458r1/F2F1LmXm2jP4hk5nmXH0KlVMgfSu2MTFuNoMP6Gnv9zRTXRr8EjSbFt6idWSYfyjKix8y05h6ufVZtod3zofySacgfIx7ybDiLO2d1fBrKvvzc8kCssy5pNT46/LpHE7/8kNMPJZECgAFLDkZSPgfQnnE7hhf3zV9qB3QuENW8mH0xkIgB+OplBt3O7GdVQcuv+HTNn5B/HMLGRnWgY2f51Tvo1KSt/F/WbTrSv+qPqEB3xBuTtlnNOODzvPRs8uJNV6+z7hoPWHfvcBInYKkBGtatTnELl6G7pPXWTinA6awYIwrprPOyhYxOKBx7c0EXx3DOnrDpVT2nP6EJZmppNri/7OjF+6qQjKuexFLOJF8HnVEVzQkYxr0F94cdIblUbvI9++KNv8M+oa+6KrevPg7B/YkJ1r1t1+v+yfxJx8LKfFbKbhhYzFZ+RaUHb1Qw41B3GRlfH9yC18XVgA7ickZyBIPb5Rnc7HYBTDB141DPy7knewr/W/GDcR4z2NCJ2/eNWRevq/cQMyZc5dbsPmH2G8OJcBNDUV3pqOrhQetJwFqBe07vExK51qbSt3wAKv+2AwXkjD4jOKDx7pwKC+T1LxjbMlMJ6MKwIEA1w6o1FrWPz6w1pGZeKiAevtbbzPLYTbGFfDWrLVsiUhBn2YgcVcccTVbUVjIu5hd/RXKkkNGPui6dUJJTnV/sLmYtCPpNilTre2MRtWd1zbv5rVa25LdlYCVYZmfxKJFiXz53gg0Sf/imU0XGlCFlmmh4xhlb+STQwt5J9fU6L5wpddkDj0cdOWLQxE7vp/HzNrpo1IClhseIyvtDPmTAvF3bgsjwtDpAkleHU9aUCcsaXGkNbCoYN9wgksPMbYp3R6O4Uz60+MoTkbxaXp2nbuYzRZAiU0GB1SaMJRerb8CS1UZKBSoAIvKG582jgSGLMEQcv1hRaXq6vp+NZF37QOzAnMlqOwUtqjWKi08aAHKMeiXMPrsjZ+71rIU7+XZnQYe8QzkEc8AwoOm8Yz3Bsb+kHQtqM05n9P/h0N3+YWfQuKjJjFqU190ffrSL2I0C8cPp9/UKcw7UP0VtPabQ13nF0ALlhv6BZqP2ZTIG4/NJ75JV/oUaEP8Lg8v0wajdd5FlrUXfzDyQfIWzH46RoXOYXChgR3Gvay/eI6sBrZoLbkbGLtn27XbRaV17GQqxmLugLsaqFGjRZ9CBhMI8Q9GpT1DbEIHdCGdoZsbGQlnGvb35hDEC51dOPTjXlIb9hSqOYbz58dn8rusaJYlHqq3DaH2cIbiAhu9H8rhZv8HlQWs37eQBUU3P8vddGGv9ZEAACAASURBVBG5hffR5pJqAl/PrrfsKzVXAnYK6u2mqcrl++y9vHViNWOP6sEjiFA7gApSCwugXcCV23e7crL0SWz8eDl/fTaSdRluhIR1rRGlStTaQK5eq8LZDx+NhaxTF5r9D9MMdUa4yXiefLUfuiArWhh2anwc3fCo47VX93mFheOcifvbG6wzhbFw7qAGjOmtICN/LwsOLaH/d6t4Nxf6dp/GnqHz+KRXOOENaapVmUgtzr32r66gtqQZyMALH22t51xsIDnLDf8R/fC/mMjauDNoBo0mxKeANH3drcn6+HQOp3/VUT642Mj4c3yEPz8+E21ONMvi99wkRDvToyNkZVxoXNBWAjg0rjVsziSjXE2wh/XXYO4GLSI66lfGjtOHyPMYxcrAXvR1dCPAJYAnfMex0rfLdW9y86VMMtt05Qlvb3xU6uveuMGdRvFGpwCCHdVoHLswytsbF3MuZ6+8YTLO72Y/QcwPDeePLm74OHch3Hs40T10BNzW53sLzjpejZrMyD5+aDp2wH/IcPppICut1lhX/wjefC4Ef20gI2dNQIeB2Pic+s7aSNkYL4KmzyBC3NuiVCqu/X9YjmxmXbIzIxe8yaRBgWi1foQMGc3c6NcYWSspNZ0ns2NIJIs9a335ctYxe0EEbFvK0oRkVs2PIWvQKywc0aHBlVrM5/j6dAzPfjePUcl7yVT1ZphrM3/Zu3iYZKMzQbqutTZko08rwD+iH+bkFLKSD2MMCkPHGZLTrO1vBuy68qKfN4bTe6wfCVOT8hHGPh5FD8u3bD1xERf37ni7d8fbvTOOtfftGEyQpoDkhEZ2LVUUkFXuSLB3ABo7B5QNSaGqVGLO5uIbMJnFnboS4OhJsGsvXnhoMm+4371f0O/eyqxkyv+KFw+VEdl9FB/4uUBVEXmXzrH7dMF14WIp2smSk95EBc9mhwJ+zniXwSfSsQBFVY4EPzCOUcEuuFBG5iU9UQe2cfjqwRXHmLkPIoOHEjlgBO0p4+fiTA6ft364SN+H5hHjV+NTeMAyRgHkbaB/YlIzDTspAY++vLxkDO4eSsg7T/LGf7JoU60+2vh4MnQziHnVC0tWChvn/4uN1o7tdB7Omn0zqO4ee4U9R18BCoidOp55B66GQwk7V68hLGoC7333LKrrhnddYN3fImHWXxi/IJrX1GDKOk9a8nZirWoiuRKxYAYR5m1MfTvlcqvq1HrmrdTx6azXGa+/OuSrocpIzd3LnNy9jTn4Fi6wcZuBp8YMIijagP7a/eWkHbkAIzuQnHQBiotJNEKY0oDe6m4Q0HiHE26nZ875Rnah3f8w3V1VOPE0U55+uvr+ioN89NEsjtQYKaONGIR/VjyLjjTgg+A6qbz74zGiH5rMHj8FNw7vurmU06uYVjmKvwVMZr2TI/xawNlCAzGlzTtSpDndF9A99LeGHuTp2Z7c3J9tUY+wqSvjaLPmMayxv+oRjacMYeHm11G+PYk3Eho6bOtmvHnj0Rn0v7CUP53Obcbz1kEZzNzNc/FYMYW/7rpHftbVDFp414EQLYglmeXz15Ol9mreuQ5UavIubmFJho1DFkDjStbGZSyVkG2QFt91IERLkn9kMyuONPNJzamsMTTzOetjTGDtx7fpsVoR6ToQQggbk64DIYSwsRYdtJ6e7e90CUIIcUstOmiFEKIlkKAVQggbk6AVQggbk+FddzEf7Qy2PJDLtD2f8701P6u8/4+M+f3jBHfoiosTlBelc+LYCjaePHnXTDAmxL1IgvZuZdeVCQ94YzgTY13IAo4eofhUnCY+8TP+WwRuD/x/PDnwbRwtz/N+PdPdCSFsT7oOmqrbZLYcWMUkbR3bgv7ClgMrGN+IFUM9OoYzzE5PjNH6366XpS/g7d3RxKd/z+nc7zmYuILvC9V07/qgfKIKcQdJ0DaV0UCayYse3dresMm9W1c0+QZONHgKem8mPNCVvPN72NOk2f6VONhDkSmfu3e6DSFaPwnaprIYSDYq8Q/pihIFIc8t4K1xgShR4B/SCUuaocGz5Ks9w3nCKZ2Ys5lNmvjFzf95fq/6iV0njjfhLEKIppJvlE1WiF6fjXsfP9RKBSOf7EuYORufjcWEaJ3J2GZoYFi68cwDQVj+u5otTVjhwFEzkyn9vDj5f6/y/d29LIQQrZ60aJtBRrIBkyaQHv598c+KI84SiE4biL8mG72+YfODKl3DGeuayfrT6Y1uzTpqpjNjaH/y981irVEugglxp0nQNgOL3oBR7UfYmEDM+u3E6pX0G9cXHy5woiGz5KNm1AO9UV7cw5eNHI/l6DmdqUPDyU+cxfvp5xt3EiFEs5KgbQ7FBk4YvYiIcEafcJ4T8WfQRPTDPSMFfUOapc4DmeBZxI40faPWYnJwn8LU4Y/jmL6aXXnKK0uRdMdb3bLWVxKitZE+2mZxnuS0Yp53N5CYVo6Fw6RZIvDXGxqwRI0jf/QPxSNvOzENWMKkJpfOD+OjUsGDc5j9YI0NmSt4I/aru3wFXyFarxY9H+3dUkezcBzIF+EDyUhczJxCGYwlRGsiLdq7hLpNOSknNxAjIStEqyNBe5cwFSbxlizDJESrJBfDhBDCxiRohRDCxlp00LaaC2FCiFatRQetEEK0BBK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYxK0QghhYw53ugBb02hnsCdYe/lGaSITvvuKw3e0IiHEvabVB22WcRmBRvDQTmP3A3e6GiHEvUi6DoQQwsZaRYtW4zmcqO6hhLZzgV9zOHR2C1GnU8my+gyePPHQ00zr3AVvBRSV5pJyegPTjOewXNlD6diLyOChDPPogAtFGC7uYcmxvRyuss1zEkK0Hi0+aNWuTxETEojh5OdMzMkFdS/+1msiS8qXMtFYYNU5NJ2eJqozxBxcyvpiaO+sJdypxg4OQUQPeBrf3G3MTEgl064LE4KfJrpXEaOPHCPPNk9NCNFKtPCgdWSYfyjKix8y05h6ufVZtod3zofySacgfIx7ybDiLO2d1fBrKvvzc8kCssy5pORXb/fpHE7/8kNMPJZECgAFLDkZSPgfQnnE7hhfS6tWCHETLTxoPQlQK2jf4WVSOtfaVOqGB1gVtIYLSRh8RvHBY104lJdJat4xtmSmk1EF4ECAawdUai3rHx9Y68hMPFRAWTM8FSFEq9XCgxagHIN+CaPPWtdNUBdL8V6e3WngEc9AHvEMIDxoGs94b2DsD0nXgtqc8zn9fziEqXmKFkLcQ1r4qINcUk3g69kVj1vsaa4E7BS41LdDVS7fZ+/lrROrGXtUDx5BhNoBVJBaWADtAq7cFkKIhrH3aO8d1dCD2rZtS0lJqQ3KaagKzpfdz0j/RxloX8D5EgsuTl14xHsIU12L2FN4icpr+3oz7IFAOhSnk2oBh98slP52eUtwp1FMagemcjP2io78ya8/g5UZfHTm8siFX0xlPOg7mGc8qsguukSlwpM+7fszzceD8zkX5GKYEOKmWnzXgSn/K148VEZk91F84OcCVUXkXTrH7tMF14ZmAViKdrLkpDdRwbPZoYCfM95l8Il0LEBRlSPBD4xjVLALLpSReUlP1IFt1b8gqzjGzH0QGTyUyAEjaE8ZPxdncvh8qoSsEOKW7gvoHvpbQw/y9GxPbu7PtqhHCCFaHel1FEIIG5OgFUIIG5OgFUIIG5OgFUIIG2vRQevp2f5OlyCEELfUooNWCCFaAglaIYSwMQnalsh5OGsObGDhHxR3uhIhhBUkaFsiSzaJCYc5kV9ex0YFuqgNHHx/OO63vTAhRF1a/E9w70mWZNZGJt/pKoQQVpIWbXPpqGP2+x+x/8Bujh/4mt2bopn9h7bXNvtPX8XB7QuYG72KHXu/Zv+uj3jvuWDU152kE69+sYMtc4bwZNQKdhzYwfEDX7NlTghKAOUQ3ju6m+NHd3P8aO2uAwVh0V9z/Oi3/GekG6qQGey5su/uqCvHA+CK7rkFfLlrBweP7mD/9mjmDulUY/vNKYMm8+WBDfx7iOu1+7Rjotm/N5ontQ1+1YS4J0iLtlm0JWLW6zypjmfeS/PRm5zRBunwQQmUXNtLpelLj7hInpmZAn3+wpp35zLXOIk3EkpqnEuJJmICQzf+h1ceO0y+R3fCtMWXJ8ix7GJq712X+2i/m1CrhnLiZz5BTxTootbxb00Mw17aTv51+ygImr6Ef48oYd3bkcw7VYy7bgJvLliA6eIUVujr6oq4nkX/IX9f2Z1PZ73OSH0ksarRLHy1Eyfens5GYyNfPiFaOQnaZuGMh7sSk/EwSfoLmIAso4Gk2ruZDvPR2pTLk4cfiWHdkRHMfbIf6oRd108onrGZeSuTLi8uWZxCrLGZylT2ZfyTXiS/PYkVuwov32f8D+uGrGX8kK6s0huum/GsPsbP/8Vy3UpmL3iNIGUYmqRlPLMtp5mKFKL1kaBtFjkkbkth0qw32fKFAX1aOskJu4hNOHNdgJrzsskqvnrLQlZWAfTphAZIo/r+/FNnbDP9ojYQH7Uz/gu+4viC6zeZLrrWfUydctg4fw39vnidpyxx/HVaQq2WsxCiJgnaZmLcFMmwI8GE6UII0fXj5XdGMHLtTP68skYrUaVEpYSazUZVHecymy1WtSwbxZzNV9MmsejIrbsJbkbdLRCtGsCPIB8F8VZ0Owhxr5KLYc3IYkwh7vMPWTRtCn+PLUarC0ZTY7vKvfOVcAJwRqt1w3zxwuUuguamrOPyltFAhsWNHkFeVpzAEY2jGxqHOj6L3XXMnRuBZeMb/DXOmfFRrxDi3OSKhWi1JGibRSdGznyNSUOC8e/YAW1QGEODnDEZz1//lVoVyPNzhhOi7UTImBk8H1RMUmxiMy/4WE6WMRt8+jK0mytKpaI6cy2HWbfxPNopc1k4JgR/bSeC/jCISVFLmD2o1o8fnMP5YMg81nfX1hqR0IGRC2YQZtrEvJXJxL+9lFgiWDhLJ+N2haiHdB00i2JMdGb8qwt4WeOMylxAWlIMb7yddH0fbUYiO03DeeuLGagt50levYh5u0rqPev1FITMWcuaJ2u0Rt/7lpEAJ/5D+HObr4W6cWMM63QzePmTr5itgrzYNxgWlYyFcvQrI/mraQavTXqTmLnOYMrGqD/Muizrvvprx73O7KBsVr0YQ5oFsKSwNGobPd6fwZtJBv569SKbEOKaFr2Uzd1ShzX8p68iZlAKE8b8b40LX0KIe4F0HQghhI1J0AohhI1J14EQQtiYtGiFEMLGGhW0d0sr8m6pQwghbkZatEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMStEIIYWMOd7qAGzlxf8BQfL1/RxtFBSVnN5ByKu9OFyWEEI3WyKBVc7/PALpou9BWBZaic1z86TsyL1maXNB9bjq6etuTm7yWzJJK7Cht8jmFEOJOalzXgUN73O4vJStlA0f2byXT7EWX3gNwaYaClGpXHEpOk3PJRGVFKeUVzXBSIYS4gxrXoq04y9ljZ6/dvJhxDu8+LqgcgOuC0R5nn8fpqm1PG5UKzGc4vf8b8qs8aP9gGN6e7WljZ6YkO4lTPxkwVwF2DtjdP4A+QwcAUHj8PQzZlSjcdPgGBNKurQrKMsnUf3ulBe1Bh16P4e3mShsFVBQkkXLoOGZHX7TddXi4ueBQUUhB+necvpDHb9jjEjAa345utFGpsKssoST3IKf1P1JadaXqdn3we7Anbi5tobyA7GNfYCy4WQ2uuAcNpotXR9rYV2D5OYmfjhynrFEvrhCitWmGPlp7nD29cDAZKLqh9WmPo5sXdnk7OZaWTZW9PVUV9rh0H45fu3OcSvoGk4MvviFhdOv0X/TnTABU/bKPY4f0WIDfqirBsSfdegVSlf4NxzJLaNt1OAG9H6Vk705+qXKhnZsLJalfoM8xY6eowIwHnXsNxa0knpN7z/Hb/Tr8g4fS5ZfPMJrsUbVtj13OTo6lZ4OjL9reA3igSyYpGYXg+BABISFw4TtOHMum0s4JzDev4ZKHDj8PCxlJq8grc6CNI/za9BdWCNFKNHnUgcp7KN06lpKhP4K5nn2qzEWYLaWUl5motOuCV0cVhaf2UVBWSrnpRy5kFtG2Qxfsax5TVXk5ZAFHr0DUZQaM57IprzDxy6lkLjl446a+uncFlWWFlFeUYi6zQLsgPNpmk2kwUGoppSz3IDkmF9p5uFafv+JyTeZLP5J5sRRHNy/sAZVXEGqznrOnzlJaVoq5JA9zxS1qqDBT5eCEum1bqCqlrKSU35r6wgohWo0mtWhV3o8TFKDkYvJWckqsfUQnFAoV7XpPRVfz/iIVSqCqjkMUjk7YuYTQa2hIjXvN5Kns69gb7lM5obT/HX6Pvopfjft/LVIBNxZabjbD/SrsAJWjE5QV3tAivVkNv+Xu4+TJAfgEPsvDgT+Tmx5PRmaehK0QAmhC0Np7DCYwwInc5E0NG21QUUpFuZm8lDWcyqu8YbOqjkPKy8xU/XKcYz/U32qu6bcKMxWV5zgXv5Wfb+jOUN702PIyM7i50gau62O9eQ2VlGbu5qfMfTh6P0Zg96F0KvqM8yYrihVCtHqN7DrwwDugG5XpO7lgquQ+O3vus6u7dXmDqv+Sk1uBW7cw3NupUSidUKm9cLxJ5JflGihR98TXpzOOSicUSlec1er6D/jlR/LKvOn04EO4tHVCoVTj2M4DhRXlleX+SIljEF27+eLk6ISirRdOylvUoPTCRe2EvV0llqJsylHhYM2DCSHuCY1r0Sp/RztnB9SBf+YPgdV3F+pXYci8VevWwi8/beXMg2F0CfkzbRQOVJgvknn0KzIv1XNIyRFSj6jwDXiMYP+22FWaKc39P35KMVFe1/5V2Zw78h10f5huukdR2ldgKTKQ/kM8v9TVN3HdYx0n9YgTvgFh9PBtC+VFFKZuIi2z/hqq7n8I36BuOCkcqCovwpQZT2bBLR5HCHHPuC+ge6h0JQohhA3JXAdCCGFjErRCCGFjErRCCGFjErRCCGFjErRCCGFjErRCCGFjErRCCGFjErRCCGFjErRCCGFjjQxaBWFLNnDw49Foat7dbTJbDnzG3D7yQ38hhLiqkbN3lZMYe5j8dwYRod3MWuPle4NG9EOTlUjskTpnILie83C+CR+Mb50bjUTvXMYaa6bqEkKIu1yjp0m0HNlOYn40Q0f4sXblGVAGMzLMC2PcdvTWnKB0LzPj9XVOiwhlZErICiFaicZP/G0xsDEum5GDhhC08n9JCxpEP/d01m28YN3xVSZSi0z1zg7b9PV0hRDi7tCkFRbStu3COG44I4NiiB/RD3d9DDsvWnmwdB0IIe4RTVuc0RhPrH4C48dMQK1TkrwykXxrj5WuAyHEPaKJq+DmsHNbCq8uGMNQUyJ/jy+0/tArXQdCCNHaNXkcbX58HEkmMCXFEV/cHCUJIUTr0vQfLKjdcFcWEB97WC5gCSFEHRrfdaBsi7t7Z/rNGo1/VjxLrRk7K4QQ96BGB62yzyt8+V4E6qwU1s2PQS/NWSGEqJMsziiEEDYmk8oIIYSNNSpoPT3bN3cdQgjRakmLVgghbEyCVgghbKz1BK1DKJ8MX8hi13p3IFg7mW+GLcXw+DIM/cPxuZ31WU1ByJzPOPjxaNzvaB2dePWLHWyZ7tf4UzgPZ82BDSz8Q+PnJ1YOWcD+AwuIqG/2oXoPDGHhrt0cP3r535ox9f5hNImP7wyOPzqKYJucXbQWrSdoqwrYn6kn5dd6ttsFMbV7V/JOLiP8m0hCE/eScVsLtFY5efrDxB85X88PQPyYvWkHXzYlAK1SjD45kaS0Jvzcz5JNYsJhTuTXPcZaOWIJB/feIkQvppCYkEJWgx87mXlDBtPz928Qm9e0sYdKr8kcHzaBPzbpLOJe1sS5Du4iVemsOZZe/3aVGx52RRzKyySr4vaV1RjGbct5404XQSHx0YuIb8opLMmsjUxuUhUW/WbeiGzSKYS441p+0Nr14v0RE+kPQBFb9s1jTo25bZQuo9gRNhDvK7cDw5cxEaBgG8P277G6VeveZyxvzhqNzt8NzAXkpyWyPHI5cRcB5SD+/d3ruMfHYQnph79aiSkjkeVRy4gzVrfmlEMWsGeBklWR6eimjyDERwl5KSx9KZKNRgVBM1fz6fjOl3c+8R/Cn9tcPRuaMpi5m6N56uraQT6rOT4JwELi/MeZuu3y4yi7DWH2rAkMDfJCTQFpSZtZOn89yVY3TDsw6eN1vNbj8q2MtVMYtfJMjRdUx1vfvYlHbAx5IcPpp3WDrMOsivwn606VX32ivPfD6/QDoIDYqeOZd6D6dVCPiWb/3Oov2//zw27+ByBjE8+M+V/SAOWguex5Jww1gDmRvw+cT1zNhqkykPEL/sKTfTqj8XCGvPMkx61h6cokjM304xm19zQOhXS9dvudx3vzDsCl3YxK2E7qtS2OBPtOZvEDAfgqyjh+5nOmG1LJu7rZzpsJwU8xoaM33nYVZBbq+SBlA18WVwAOPBG6hEj28iVBPOHhhktlLntSPmRmdkHzPBFxx7X8roOqY7y0dQaB33zOocobN1uKtjB46wwCd23DUJnDB3tmELh1BoENCFmUwby8YAL+xjW8MHo8z0xcxPKEHK6f41FJD50bsS+Op/9j01mV35eFSybgX/tcqkAmTfEiPmoS/QeMZ8LiOLLMAOXoo5+nZ+8/8sLGbG6YJdKSwqLhg+nZewrrMiykrZ1Cz96D6dl72LWQxX0Q//PuK4Tkr+fvEyfxxEv/4YRmLG8tGNSA/t4c1j43mJ69J/FRWn2JpSQkzI+kyEn0HzidVfnBvDorovoxLLuY2nswPQcsI7mO6S5Nm2bSs/dgQucfxmxK5O+/H3z5uVwJWQBLwiL69x5M6BuJ1DnHm9IVd3MKaxfPZ8LoSbywOBFVxJv8z7hOVj/TWzFlvkvg1hkEH9RjLj/K37Ze+du5LmRB5RTEM+0MLElcxos/ZuLd7Wmmulzd6saE0GlMdT7HuweWMmzPKj4o7ULkH0bxSI13n4tnIOoz7zJ4RyQTz0J48HDCW/67U1wh/5VW6YDGHbKSD6M35mA8lULcx+uJM16/V8a2GGIvloPlArEfx5Ol7cfIoNrnspC04l9s1BdiKS4kLSGBJGsnS78F7Yix6ExxzJu/naRTFzDqE1i6IhF0EfRzbp7HuCotbj2xxnKwnGFn3BnQBuLf0AtWTVGcxIqoD4lNSCHNeAF9QgwfJRWj1QVfbgXfTlVGPkhJ4vviTL437uH7UjUBbperULoMZIJHDh8c2sLXhblklJ3jy5SdHFIEMcq1+gulOS+R9/JNWKgg5b9HOavwpq/T7X4iwlZaftfB7WA5zMa4At6atZYtESno0wwk7oojTl9z/l0LWcbz1TczLpDPCDQdFaCvcTHIfIETeltMwKPAP8gLlU9XPv1hTK1t6bh7AM02jaUFU1b2tVsmiwVUynomcbeVDuimv8JrEcFoNc7Vj31CiRrqbgXbiLk8l8yqq7fKKKoEld3lkRYu7bzxttcyc+gyZtY67lCbGucwF1B09UZVBWYUqKQZ1GpI0FqlkPioSYza1Bddn770ixjNwvHD6Td1CvMOlFzbS6VUAjVCtM7ksWCx4QQ85uRlDH5pu82DxnyHJxHSjHmdf49rS+z8mbyScIZ8iwJd1Dr+rb0DxVTefI07c/lRZu6IYU+dW+UteC+Qz0yrlZOlT2Ljx8v567ORrMtwIySsa43FJZVo/KtvK/390JBN1sXmb72aoY5FLctJ02eDNpgQK7oJ1A5u+Diqb//X7JrMlrqeiBUUaIM6gX47K3adId8C4IxPzZbtNeWXH0bdxH4NO0Wj3i1FlzLJU2h5xEUC9V4mQWsNZx2vRk1mZB8/NB074D9kOP00kJV2/VhXTcRkXhvkh7abjtemh+GelkisVWuvN0Q2xoug6TOIEPe2KJWKa1ll3LaeJPox963JRAR1QtstmLAxk3kranSti3IOhPeOZMeQiYy6vd/3r3cxmyyVH2GDOqFWKlBanYXlZBmzQduXsI4KQIF2xCuMD6rjBJbz6I0W/CNGo+vWAU1H14Zne2kBmfbeDPZ0Q23n0KDjLUV7iclxZFTIOF5w98bH0Zu+njrm93mKJyR77xkt/r+670PziPFzq75jwDJGAeRtoH9iUvUQmyYpAY++vLxkDO4eystDiTb+k0Wbru+jTY5LQTsrmi81Skwn4lkUGXPtKvotOQ9nzb4ZhFy74xX2HH2FG4dHlbBz9RrCoibw3nfPoqo5vCs/gb9PUzJ71lhmv/8sHhSTl3WG5G1fWP06KIcsYM9b/apbuv5XhpFdHWJ1yzMoCJmzljVPelXf9d63jIQbhqxZ9JtZtTGQ1xasZb+KGsO7OvHqF2t5vsanw9UhYFeHmxk/X85S/9n8/+3df1RUdf7H8edXmeGLMpaACBg6uLqoheMPtAIr+coCWWL+qCzPUpptHlfZWnJ31T2GfSv6rlKu2A/Nstgw8kcquAQsnMEKVMAfyK5AaowaIxA/ihllZyD3+wcoaKgDzgXH3o9zOMeZe+fO+17wNZ/PZ+69n+jkbURbzNR+rWdbTiULva+sp570NW8TFL+IN5Nn4cwxVj8UTVInvoC0NmSx/uQQYsatJF9FB6d3XUsdifnr4a4ZRN39PDEqaDhfTWlVLrsuXP/V4tbQpfvRo8xzcwAAEZtJREFUenoOoLr6OyXqcUyt59Gq17SdzyqEEBfJ0IEQQihMglYIIRQmQwdCCKEwadEKIYTCHDpoZUodIYQjcOigFUIIRyBBK4QQCpOgtbu+hMR9zM7lusuuINI+vY4De35HRxcv3XTUOlbs+JjXJ/ft6UqEuCU4/JVhN52AOUQHVZE0p+iaNxqxi14jeG3KM/gdj+MJgy03iXZDO+o5HrrzbrS3u9OHWr49k82u3A2UmNpVay1i85YqPl0SRUDOO9j9KmJ71OkylxefXnjFvG//YvuHC9E3tm1n+NjlzBk9Bi+1ldqqbLbrEzhq6uE74oifHQlau1IR8mQ4HsWbWmZeUJjPoFCm9iom5rStd+IfjM7Pm4YTm/lb1Vkaew9nUvA8Fj8EryUnUNFuTWNGKoXRi5g7eRN/yunuq91srLP5LIX618isvxicVuoa27ai8Y/lN4FuHNW/yMZ6dyY9EMP8CDNx2zZQ1c17JH7eJGjtST2B8CBXjq4ruMptCtWMfnIVL8+bgJ/azNHUtbzwWl7bdDWdMoRnhw/hm29Wk23zNfNH2P336HaP8ynfPxbdg3cz6vYEKr5vt8hcgL7YlaXhE1Dn5CnfOu9inQ31x6io7ai6wdwzeiyUx/LJiSM0Azu+GoNu1hRCPDeTXC2tWtF9ZIzWjtT+OkarKzFcdkPwNs7uwcz2L2L14hgWbTiJz+xFLBzftam4PbxCmfrfpSSWV99Iyaic1dBcR8P5K5eco6ysEnXAhO6dOeEqOqzTyY17pu9i7bNZxM1Zx5xhw9paDr3vxO92+Nb4Ly7NxVl7hG+bvbnDww0hupMErT15e+HuXE/5VefGLmHzq5+RV1xC3pYd5NW44e/flf/0nkQNH4HpdBZpNzKjb++JTB87ioayrRzsoIFnNNaDuxs+P13UvTqq88fj7N+fwN/Sl7N2dxyZNW7c86s1/Nq39XiqXennZKHxvBnt3R+zeu5LjOxtosEKLhoJWtG9ZOjAnpzVgPWq3WxLbSXGS8OJ5zCZLs7K0Dka91Ae0ZxiU/6pG+jSD2NSxHLGW7exNvcrOspri9UKdG2KGrXXM+TfHdD62gbSvlpJTJfGSK5SpzWfrw63rWXIOoVT/808NPo+XM7spt1QLU3n62horOtwH4XoDhK09mQyY7UMxF3DVefn6mBS2E7SMGN4AJz9iJ1d3thg7o5Yw/Q+2WzcvYGKDmYPBtBo+oK1qkvT4lirtzInO/XS458OTdivzhan+bbGhMrDi35Ao9VMQ7MzLn3UVBRH87/FQO8phKih0STTeIvuJUMHdmQtK6EcL/y0XRt3tYW6XyhRHnXsOl7axXnBBjM+Yg2zNV+ycXcCx6/aJFbh/0svKC/hqrOOX8sFE6Xm6ks/xk7f5NrWOi/ywqu/hqbzlS2THP54nPLv4Q6fsW2tCfcx3OF0lm9rJGhF95IWrT2dLaDQsICQoGGwr0SBN3Bh6vCJaKq3ktilGW29GB+6hl8PrGT3P7I5rxnFIACsNHx/AtNlLcZhBAe4Ysgp4KpDzoq5fp0DRy0jzPkYRVWnaMAdP/95TB9Yy8E92a3DBifYf/QwYffP44lhtWTWu3PPpCn0q9mNXs44EN1MgtauzrA9tYRHZ00mIL7E/if6uwQR5d1I2heHuzhFz52MHuyNytmb2dMT2j1fy/7dj/E3Y7sACphMkE8J27efubGau+T6dTZZ1Qwc+xzz79GgwkJD/TG+/DyW3WfaPoFMZbFs7LOcOfevYWXvlgsWPvjHZjmHVnQ7h74f7c1Sx2XUgbz82R9Qr5nHn3LOXX99mzkxZexK4vukMzU3T+FWZl9C4jaz1PIXHokt7OZzaIW49cgYrb1ZC/nrS8kYNV5dm0n7qtxQm/OJK85Xviuv9kJjTOaVBAlZIexBWrRCCKEwadEKIYTCJGiFEEJhDh20MmwghHAEDh20QgjhCCRohRBCYRK0QgihMAlaIYRQmAStEEIoTIJWCCEUJkErhBAKk6AVQgiFSdAKIYTCJGiFEEJhErRCCKEwCVohhFCYBK0QQihMglYIIRQmQSuEEAqToBVCCIVJ0AohhMIkaIUQQmEStEIIoTAJWiGEUJgErRBCKMyppwtQmo/2ebJ12pYH53OJ+sc2Cnq0IiHEz80tH7RGw1pGGsBDu5is4T1djRDi50iGDoQQQmG3RIvWx/NhYkdNZOJt/eDfVeR/s5PY46UYbd6CJ4/c9RiLBw9hkAoazldTdHwriw2nsLauoXYZyzJdBFM9BtKPBkrOZhN3eC8FF5TZJyHErcPhg1bT/1ESA0dScmwLT1VVg2Ysvx/7FHFNq3nKUGfTNnx8HyN2MCQeWE2yGQa4apnSp90KTgHE3/8YQ6tTickppaLXEKJ0jxE/toGZBw9To8yuCSFuEQ4etC5M9Z+I+uz7xBhKW1qfjdm8cXoiH/kG4GfYS7kNWxngqoF/l/JlbTVGwGippqi2bbnf4Cnc15TPU4fzKAKgjrhjI5ly70Qm9TrMLmnVCiGuwcGD1pMRGhUDBi6kaPAVi8674QE2BW3JmTxK/Gbw3q+GkF9TQWnNYXZWnKD8AoATI/oPxFmjJXn6A1e8sgIPZ6DRDrsihLhlOXjQAjRRUhzHzG9sGyboiNW8lyfSS5jkOZJJniOYErCYxwdtZc7+vEtBbanawn378zHZp2ghxM+Ig591UE2pCYZ6DsPjOmtafgR6qeh3tRUuVPNV5V5eP7qBOYeKwSOAib0Amimtr4PbRrQ+FkKIzuntMWBQbGdf1LdvX86dO69AOZ3VzOnG24n0/x8e6F3H6XNW+vUZwqRBYfy2fwPZ9T/w46V1BzF1+EgGmk9QagWn/1g5/5+WJTrfGcy7DUxNFnqrvHnoF/cRqi7ng5MtZy58b2rkzqGhPO5xgcqGH/hR5cn4Afex2M+D01Vn5MswIcQ1OfzQgal2G8/mN7Js1Aze+0U/uNBAzQ+nyDped+nULABrQzpxxwYRq1tKmgq+K19P6NETWIGGCy7ohj/JDF0/+tFIxQ/FxO5LbbuCrPkwMV/AMl0Ey+6fxgAa+c5cQcHpUglZIcR1/deIURP/09kXeXoOoLr6OyXqEUKIW46MOgohhMIkaIUQQmEStEIIoTAJWiGEUJhDB62n54CeLkEIIa7LoYNWCCEcgQStEEIoTILWEbk+zKZ9W3n5XlVPV3ITUREen8aB+Mmoe7oUIa4gQeuIrJXk5hRwtLapg4UqgmK3cmDjw7h3e2E3Ie+ZJO77mBXjb+xDyW/oYjYOcrFTUeLnRoLWEVkL2bxsNdu/7ulCbpxP/4k80l/T02V0rNcQFtw1hUnObVeqq10C+P1dQYzowbKE45GgtRfvIJZu/IAv92VxZN8usnbEs/TevpcW+y95lwN7VrEi/l3S9u7iy8wPeOtpHZdHjC/Rn6Sxc3kYs2PXkbYvjSP7drFzeWBLd1gdxluHsjhyKIsjh64cOlAREr+LI4c+5+1IN5wDnye7dd2s2MB23en+BD29ik8z0zhwKI0v98SzIszX5u62+6w4DuxdRaRruyfVgby8J41Pl/h28qA5MXLoDBYPcutCd1+FNmwpiXvSOLBvF2nroxjd/mBqn+HTQ1kc+fsiRjt78eh7n7cet3XM9bbxLS5UkHa2mRmBi4kd5MZQbRQfjQugpqKYb2zchM+T6ziQuZSg9jvoGsabe3fx1rT+oJ3Dpz8ZBupLePwu6ZXcQiRo7aIv4S/+gdmaIl75zTymPrGMlR8ew3hFfDj7TGC04R0ef+ARpi4rwOO5FayY3PeKbanxCY8iojaZRb+azn1PvMTmvPqWG+RYM/ntuFDG3L+WQsuVNTShj3mEMeMeZFFKHZbCtUwZF8qYcaGExha23mBHRcCSON580pXcNcuImrmEP24xE7xqFQsDbOta12ZkkMcEwkP6t1U8PoQg9xOkbD/TqaN2Q34Zxf+tCsZZ/xcWPBHD6nIdkYHtjrfhfR4fF8qYh97mqKWSbc8+yJhxoYwZF03SWVvfpBljbS7rj1eg0bgxyFVFQWkqyfWmy25YdC1GfQ5lmglEtBu60AQFE0gJGfp6MOhJKXYlKHJC21+LazCRgVCYqqe2w60KRyNBaxeueLirMRkKyCs+g9FQQl7q+yTtq798NVMBH2wuwgSYDiaSdNCVkNnB/KTjXP4ZKxPyMJibMBmKSMk5aZ8y1ROYO9uLwoRXWJdZRJnhJHlb3iapzIuIsGG2tSrNuaTkmQmcFowPACqCIyegKc4k3eYAu3EB04Lxr83lrwk5FBtOok94n3TbZ+O00SAWTHyBWK9TvHfcQP7xLGoGP0Py+E4MHZzNJaPYlaBwXevx7UtQuA6K9ejNAFWkZxThHBROSGsvwT08hEAKSNGfs/cOiR7i8LdJvDlUkZtaxLwX/8zOT0ooLjtBYU4mKTknL5uRwVJTidF88ZEVo7EOxvviA5TR9nzt1yeVuf2idiR+Glf8V23jyKrLF5nO9u/4NT/RRG5KAbVvhBHivYckUzCRQWoK19ja+nLiwfFxvHFH+xb0CxT9ouVf35W/S+jR0uu0GFX4+HiB8TPKLq5oPUGxwUqkjXthmyrSitazydKI39CJWCynSDy8ljRnDQ2d2IY+o4To50IIVBeSp55ARCAUrsm99LdRm5FBYfTzRIb0JSPVlZBwHSb9ytYgFrcCCVo7MexYxtSDOkKCAgkMCmbhG9OI3BzDrxNK2kLDWY2zGtqniHMH27JYrDZ3TTvNUsm2xfN49WBHZyzYxnpwD7m18USG+5JeG0IgRbxic+urma/+uZY5ZQBO6O5ayIKmnSwpq2gpr6nK5n230P5QKnHEmjFamgGoqT9E9o8tx6zG0rkJjYw5eopfjCJivIpiTQfHy5xLSt4iXg8Pwb1QTWRAHfoNRcr9DYhuJ0MHdmQ1FJGx5X1eXfwcf0wxow3StXavWzi7D0Z7aZzAFa3WDcvZM9i9xwug7mAgwFBCudWN0QFeNmzABR8XN3ycOvgstpawPaMSbfhM5ofrsOhTO9X6MlkqKDK3/FQ0gdVSRUnr49LWYLu2JgzGSnD3RXvpucFovTvYZytA6wfcDTDV5/Fpgy21daA2l/RiV4LCgwkJ10GhnrzLjldrL2F8CPPnheFvzCXlBj4Ixc1HgtYufImM+R3zwnT4ew9EGxBCRIArJsPpy7vTziOZv/xhArW+BM56nvkBZvJScu084WMTRkMl+E0g4pf9UatVbZlrLSBp+2m0z63g5VmB+Gt9Cbh3MvNi41g6+Yovw1yn8F7YSpJHaTscuy1LzaRMO425QWbyUru/9VWWmonBJ5j5YQNRo8InbA4Rfh2saKrEaHIlIESHj6sKtbonLvKoR59RgiZkAQsDoTCj4Ce/85Zego65s4dhyMmkuAeqFMqRoLULMyYGExG9isS/J7Fr4wL8DYn8aU3e5WO05bmkmx7m9U8281a0L+UbXmVlpq1dbhWByz9uOUXpi+cJdHYj8q3WU5Y+nHnZaUCG7Ykklfmy8KNt5O//nLSLp4fRRHHCMl7YUIX/vD+T+NlmNr2+gAiPSsqMnWxBGfRklFnBWEBKcVdbX81kH1xG6D9PdT6ov97BH18qwD16A9mZSWyabebo0Q62Yi3g3YRcrJNfJu2Lz8nfH2/76V12VJujp1DthQ9FpOd18Du3lrA94zRwgvRUO335KW4aDj2Vzc1Shy38l7xL4uQioma90+6LL0c2kqU74gnKi2FGfElPF3MLUBEUm8Sb2mRmPP2ZMsNJosfIl2Gik1Ro3L3wn7aASJ8TfLBDQvZGqV0H4hMwk4Xhagpf00vI3oIkaEXnqEN4fc8fCLaeRp/wKkmGni7I0Q3k0fVJLPU3U5bxNitT66//EuFwZOhACCEUJl+GCSGEwroUtDdLK/JmqUMIIa5FWrRCCKEwCVohhFCYBK0QQihMglYIIRQmQSuEEAqToBVCCIVJ0AohhMIkaIUQQmEStEIIoTAJWiGEUJgErRBCKEyCVgghFCZBK4QQCvt/gMskngpRL4IAAAAASUVORK5CYII=')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('This Function was for the sprite, when the Sprite runs in a certain direction, he comes out from the other side of the screen, I looped this because if I didnt, the Sprite could technically keep running in one direction forever')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Work Cited')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Czaplicki, Evan. “Core Language.” Core Language · An Introduction to Elm, guide.elm-lang.org/core_language.html.')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Medeiros, Pedro. “How to Start Making Pixel Art #6.” Medium, Medium, 4 June 2019, medium.com/pixel-grimoire/how-to-start-making-pixel-art-6-a74f562a4056.')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Ng, Kenneth. “Pixel Art Tutorial, Part 1: Basics & Tools.” Gamasutra, 11 Dec. 2015, www.gamasutra.com/blogs/KennethNg/20151211/261577/Pixel_Art_Tutorial_Part_1_Basics__Tools.php.')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Skaz. “Pixel Art: Animation Smoothness.” Lost Fortress, 24 Aug. 2017, lost-fortress.com/index.php/2017/08/24/pixel-art-animation-smoothness/.')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Winkler, Mary. “What Is Pixel Art?” Design & Illustration Envato Tuts , 13 Jan. 2016, design.tutsplus.com/articles/what-is-pixel-art--cms-21759.')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Griffiths, Devonte. “The History Of Pixel Art.” The Factory Times, The Factory Times, 27 Sept. 2018, www.thefactorytimes.com/factory-times/2018/9/27/the-history-of-pixel-art. Soul')
									]))
							]))
					]));
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('content')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('play')
							]),
						menu),
						about
					]));
	}
};
var $author$project$Menu$main = $elm$browser$Browser$element(
	{bN: $author$project$Menu$init, b3: $author$project$Menu$subscriptions, cd: $author$project$Menu$update, ci: $author$project$Menu$view});
_Platform_export({'Menu':{'init':$author$project$Menu$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));