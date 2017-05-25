/*

import Elm.Kernel.Error exposing (throw)
import Elm.Kernel.Json exposing (equality, run)
import Elm.Kernel.List exposing (Cons, Nil)
import Elm.Kernel.Platform exposing (initialize)
import Elm.Kernel.Utils exposing (Tuple0, Tuple2)
import Json.Decode as Json exposing (map)
import Platform.Cmd as Cmd exposing (none)
import Platform.Sub as Sub exposing (none)

*/


var elm_lang$virtual_dom$VirtualDom_Debug$wrap;
var elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags;


var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


// TEXT

function _VirtualDom_text(string)
{
	return {
		$: __2_TEXT,
		text: string
	};
}


// NODE

var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		var kids = [];
		var descendantsCount = 0;
		while (kidList.$ !== '[]')
		{
			var kid = kidList.a;
			descendantsCount += (kid.descendantsCount || 0);
			kids.push(kid);
			kidList = kidList.b;
		}
		descendantsCount += kids.length;

		return {
			$: __2_NODE,
			tag: tag,
			facts: factList,
			kids: kids,
			namespace: namespace,
			descendantsCount: descendantsCount
		};
	});
});

var _VirtualDom_node = _VirtualDom_nodeNS(undefined);


// KEYED NODE

var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		var kids = [];
		var descendantsCount = 0;
		while (kidList.$ !== '[]')
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.descendantsCount || 0);
			kids.push(kid);
			kidList = kidList.b;
		}
		descendantsCount += kids.length;

		return {
			$: __2_KEYED_NODE,
			tag: tag,
			facts: factList,
			kids: kids,
			namespace: namespace,
			descendantsCount: descendantsCount
		};
	});
});

var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);


// CUSTOM

var _VirtualDom_custom = F3(function(factList, model, impl)
{
	return {
		$: __2_CUSTOM,
		facts: factList,
		model: model,
		impl: impl
	};
});


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: __2_TAGGER,
		tagger: tagger,
		node: node,
		descendantsCount: 1 + (node.descendantsCount || 0)
	};
});


function _VirtualDom_thunk(func, args, thunk)
{
	return {
		$: __2_THUNK,
		func: func,
		args: args,
		thunk: thunk,
		node: undefined
	};
}

var _VirtualDom_lazy = F2(function(fn, arg1)
{
	return _VirtualDom_thunk(fn, [arg1], function() {
		return fn(arg1);
	});
});

var _VirtualDom_lazy2 = F3(function(fn, arg1, arg2)
{
	return _VirtualDom_thunk(fn, [arg1,arg2], function() {
		return A2(fn, arg1, arg2);
	});
});

var _VirtualDom_lazy3 = F4(function(fn, arg1, arg2, arg3)
{
	return _VirtualDom_thunk(fn, [arg1,arg2,arg3], function() {
		return A3(fn, arg1, arg2, arg3);
	});
});



////////////  PROPERTIES AND ATTRIBUTES  ////////////


function _VirtualDom_fact(identifier)
{
	return function(a, b, c, d)
	{
		return {
			$: identifier,
			a: a,
			b: b,
			c: c
		}
	}
}

var _VirtualDom_batch = _VirtualDom_fact(__1_BATCH);
var _VirtualDom_style = F2(_VirtualDom_fact(__1_STYLE));
var _VirtualDom_property = F2(_VirtualDom_fact(__1_PROP));
var _VirtualDom_attribute = F2(_VirtualDom_fact(__1_ATTR));
var _VirtualDom_attributeNS = F3(_VirtualDom_fact(__1_ATTR_NS));
var _VirtualDom_on = F2(_VirtualDom_fact(__1_EVENT));


var _VirtualDom_mapProperty = F2(function(func, fact)
{
	if (fact.key !== __1_EVENT)
	{
		return fact;
	}
	return A2( _VirtualDom_on, fact.a, A2(__Json_map, func, fact.b) );
});


////////////  RENDER  ////////////


function _VirtualDom_render(vNode, eventNode)
{
	switch (vNode.$)
	{
		case __2_THUNK:
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return _VirtualDom_render(vNode.node, eventNode);

		case __2_TAGGER:
			var subNode = vNode.node;
			var tagger = vNode.tagger;

			while (subNode.$ === __2_TAGGER)
			{
				typeof tagger !== 'object'
					? tagger = [tagger, subNode.tagger]
					: tagger.push(subNode.tagger);

				subNode = subNode.node;
			}

			var subEventRoot = { tagger: tagger, parent: eventNode };
			var domNode = _VirtualDom_render(subNode, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;

		case __2_TEXT:
			return _VirtualDom_doc.createTextNode(vNode.text);

		case __2_NODE:
			var domNode = vNode.namespace
				? _VirtualDom_doc.createElementNS(vNode.namespace, vNode.tag)
				: _VirtualDom_doc.createElement(vNode.tag);

			_VirtualDom_applyFacts(domNode, eventNode, vNode.facts);

			var kids = vNode.kids;

			for (var i = 0; i < kids.length; i++)
			{
				domNode.appendChild(_VirtualDom_render(kids[i], eventNode));
			}

			return domNode;

		case __2_KEYED_NODE:
			var domNode = vNode.namespace
				? _VirtualDom_doc.createElementNS(vNode.namespace, vNode.tag)
				: _VirtualDom_doc.createElement(vNode.tag);

			_VirtualDom_applyFacts(domNode, eventNode, vNode.facts);

			var kids = vNode.kids;

			for (var i = 0; i < kids.length; i++)
			{
				domNode.appendChild(_VirtualDom_render(kids[i].b, eventNode));
			}

			return domNode;

		case __2_CUSTOM:
			var domNode = vNode.impl.render(vNode.model);
			_VirtualDom_applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
	}
}



////////////  APPLY FACTS  ////////////


function _VirtualDom_applyFacts(domNode, eventNode, factList)
{
	var classes = _VirtualDom_applyFactsHelp(domNode, eventNode, factList);
	if (classes)
	{
		domNode.setAttribute('class', classes);
	}
}

function _VirtualDom_applyFactsHelp(domNode, eventNode, factList, classes)
{
	while (factList.$ !== '[]')
	{
		classes = _VirtualDom_applyFact(domNode, eventNode, factList.a, classes);
		factList = factList.b;
	}
	return classes;
}

function _VirtualDom_applyFact(domNode, eventNode, fact, classes)
{
	switch (fact.$)
	{
		case __1_ATTR:
			var key = fact.a;
			var value = fact.b;
			if (key === 'class')
			{
				return classes ? value : classes + ' ' + value
			}
			domNode.setAttribute(key, value);
			return classes;

		case __1_EVENT:
			_VirtualDom_applyEvent(domNode, eventNode, fact.a, fact.b);
			return classes;

		case __1_PROP:
			var key = fact.a;
			var value = fact.b;
			if (key === 'value' && domNode.value === value)
			{
				break;
			}
			domNode[key] = value;
			return classes;

		case __1_STYLE:
			domNode.style[fact.a] = fact.b;
			return classes;

		case __1_BATCH:
			return _VirtualDom_applyFactsHelp(domNode, eventNode, fact.a, classes);

		case __1_ATTR_NS:
			domNode.setAttributeNS(fact.a, fact.b, fact.c);
			return classes;
	}
}

function _VirtualDom_deleteFact(domNode, fact)
{
	switch (fact.$)
	{
		case __1_ATTR:
			domNode.removeAttribute(fact.a);
			break;

		case __1_EVENT:
			var handlers = domNode.elm_handlers;
			var eventName = fact.a;
			domNode.removeEventListener(eventName, handlers[eventName]);
			handlers[eventName] = undefined;
			break;

		case __1_PROP:
			domNode[fact.a] = typeof fact.b === 'string' ? '' : null;
			break;

		case __1_STYLE:
			domNode.style[fact.a] = '';
			break;

		case __1_BATCH:
			// should never delete a batch
			// only deletes individual facts
			break;

		case __1_ATTR_NS:
			domNode.removeAttributeNS(fact.a, fact.b);
			break;
	}
}

function _VirtualDom_applyEvent(domNode, eventNode, name, decoder)
{
	var allHandlers = domNode.elm_handlers || (domNode.elm_handlers = {});
	var handler = allHandlers[name];

	if (handler)
	{
		handler.decoder = decoder;
		return;
	}

	var handler = _VirtualDom_makeEventHandler(eventNode, decoder);
	domNode.addEventListener(name, handler);
	allHandlers[name] = handler;
}

function _VirtualDom_makeEventHandler(eventNode, initialDecoder)
{
	function eventHandler(event)
	{
		var decoder = eventHandler.decoder;

		var value = A2(__Json_run, decoder, event);

		if (value.$ === 'Ok')
		{
			var record = value.a;

			if (record.stopPropagation)
			{
				event.stopPropagation();
			}

			if (record.preventDefault)
			{
				event.preventDefault();
			}

			var message = record.message;
			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
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
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.decoder = initialDecoder;

	return eventHandler;
}



////////////  DIFF  ////////////


function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_makePatch(type, index, data)
{
	return {
		$: type,
		index: index,
		data: data,
		domNode: undefined,
		eventNode: undefined
	};
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
		patches.push(_VirtualDom_makePatch(__3_REDRAW, index, y));
		return;
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case __2_THUNK:
			var xArgs = x.args;
			var yArgs = y.args;
			var i = xArgs.length;
			var same = x.func === y.func && i === yArgs.length;
			while (same && i--)
			{
				same = xArgs[i] === yArgs[i];
			}
			if (same)
			{
				y.node = x.node;
				return;
			}
			y.node = y.thunk();
			var subPatches = [];
			_VirtualDom_diffHelp(x.node, y.node, subPatches, 0);
			if (subPatches.length > 0)
			{
				patches.push(_VirtualDom_makePatch(__3_THUNK, index, subPatches));
			}
			return;

		case __2_TAGGER:
			// gather nested taggers
			var xTaggers = x.tagger;
			var yTaggers = y.tagger;
			var nesting = false;

			var xSubNode = x.node;
			while (xSubNode.$ === __2_TAGGER)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.tagger]
					: xTaggers.push(xSubNode.tagger);

				xSubNode = xSubNode.node;
			}

			var ySubNode = y.node;
			while (ySubNode.$ === __2_TAGGER)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.tagger]
					: yTaggers.push(ySubNode.tagger);

				ySubNode = ySubNode.node;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				patches.push(_VirtualDom_makePatch(__3_REDRAW, index, y));
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				patches.push(_VirtualDom_makePatch(__3_TAGGER, index, yTaggers));
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case __2_TEXT:
			if (x.text !== y.text)
			{
				patches.push(_VirtualDom_makePatch(__3_TEXT, index, y.text));
				return;
			}

			return;

		case __2_NODE:
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (x.tag !== y.tag || x.namespace !== y.namespace)
			{
				patches.push(_VirtualDom_makePatch(__3_REDRAW, index, y));
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.facts, y.facts);
			if (factsDiff)
			{
				patches.push(_VirtualDom_makePatch(__3_FACTS, index, factsDiff));
			}

			_VirtualDom_diffKids(x, y, patches, index);
			return;

		case __2_KEYED_NODE:
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (x.tag !== y.tag || x.namespace !== y.namespace)
			{
				patches.push(_VirtualDom_makePatch(__3_REDRAW, index, y));
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.facts, y.facts);
			if (factsDiff)
			{
				patches.push(_VirtualDom_makePatch(__3_FACTS, index, factsDiff));
			}

			_VirtualDom_diffKeyedKids(x, y, patches, index);
			return;

		case __2_CUSTOM:
			if (x.impl !== y.impl)
			{
				patches.push(_VirtualDom_makePatch(__3_REDRAW, index, y));
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.facts, y.facts);
			if (factsDiff)
			{
				patches.push(_VirtualDom_makePatch(__3_FACTS, index, factsDiff));
			}

			var patch = y.impl.diff(x,y);
			if (patch)
			{
				patches.push(_VirtualDom_makePatch(__3_CUSTOM, index, patch));
				return;
			}

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


// DIFF FACTS

function _VirtualDom_diffFacts(oldFactList, newFactList, diff)
{
	while (oldFactList.$ !== '[]' && newFactList.$ !== '[]')
	{
		var oldFact = oldFactList.a;
		var newFact = newFactList.a;
		oldFactList = oldFactList.b;
		newFactList = newFactList.b;

		if (!_VirtualDom_factsEqual(oldFact, newFact))
		{
			diff = _VirtualDom_removeFact(oldFact, diff || {});
			diff = _VirtualDom_insertFact(newFact, diff);
		}
	}
	return _VirtualDom_diffFactsHelp(_VirtualDom_removeFact, oldFactList,
		_VirtualDom_diffFactsHelp(_VirtualDom_insertFact, newFactList, diff)
	);
}

function _VirtualDom_diffFactsHelp(func, list, diff)
{
	while (list.$ !== '[]')
	{
		diff = func(list.a, diff || {});
		list = list.b;
	}
	return diff;
}

function _VirtualDom_factsEqual(x, y)
{
	var category = x.$;
	if (category !== y.$)
	{
		return false;
	}
	if (category === __1_ATTR || category === __1_PROP || category === __1_STYLE)
	{
		return x.a === y.a && x.b === y.b;
	}
	if (category === __1_EVENT)
	{
		return x.a === y.a && __Json_equality(x.b, y.b);
	}
	if (category === __1_ATTR_NS)
	{
		return x.a === y.a && x.b === y.b && x.c === y.c;
	}
}

function _VirtualDom_insertFact(fact, diff)
{
	var key = _VirtualDom_factToKey(fact);
	var entry = diff[key];
	if (!entry)
	{
		diff[key] = { $: __5_INSERT, a: fact };
		return diff;
	}
	if (entry.$ === __5_REMOVE)
	{
		entry.$ = _VirtualDom_factsEqual(entry.a, fact) ? __5_NO_CHANGE : __5_INSERT;
	}
	return diff;
}

function _VirtualDom_removeFact(fact, diff)
{
	var key = _VirtualDom_factToKey(fact);
	var entry = diff[key];
	if (!entry)
	{
		diff[key] = { $: __5_REMOVE, a: fact };
		return diff;
	}
	if (entry.$ === __5_INSERT && _VirtualDom_factsEqual(entry.a, fact))
	{
		entry.$ = __5_NO_CHANGE;
	}
	return diff;
}

function _VirtualDom_factToKey(fact)
{
	var tag = fact.$;
	return (tag === __1_ATTR_NS)
		? tag + fact.a + fact.b
		: tag + fact.a;
}


// DIFF KIDS

function _VirtualDom_diffKids(xParent, yParent, patches, rootIndex)
{
	var xKids = xParent.kids;
	var yKids = yParent.kids;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		patches.push(_VirtualDom_makePatch(__3_REMOVE_LAST, rootIndex, xLen - yLen));
	}
	else if (xLen < yLen)
	{
		patches.push(_VirtualDom_makePatch(__3_APPEND, rootIndex, yKids.slice(xLen)));
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	var index = rootIndex;
	var minLen = xLen < yLen ? xLen : yLen;
	for (var i = 0; i < minLen; i++)
	{
		index++;
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, index);
		index += xKid.descendantsCount || 0;
	}
}



////////////  KEYED DIFF  ////////////


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.kids;
	var yKids = yParent.kids;
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

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.descendantsCount || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xLookAhead = xIndex + 1 < xLen;
		var yLookAhead = yIndex + 1 < yLen;

		if (xLookAhead)
		{
			var xNext = xKids[xIndex + 1];
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			var oldMatch = yKey === xNextKey;
		}

		if (yLookAhead)
		{
			var yNext = yKids[yIndex + 1];
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			var newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (xLookAhead && yLookAhead && newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.descendantsCount || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.descendantsCount || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (yLookAhead && newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.descendantsCount || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (xLookAhead && oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.descendantsCount || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.descendantsCount || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xLookAhead && yLookAhead && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.descendantsCount || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.descendantsCount || 0;

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
		index += xNode.descendantsCount || 0;
		xIndex++;
	}

	var endInserts;
	while (yIndex < yLen)
	{
		endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(_VirtualDom_makePatch(__3_REORDER, rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}



////////////  CHANGES FROM KEYED DIFF  ////////////


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: yIndex,
			data: undefined
		};

		inserts.push({ index: yIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: yIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		_VirtualDom_diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = yIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
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
	if (typeof entry === 'undefined')
	{
		var patch = _VirtualDom_makePatch(__3_REMOVE, index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = _VirtualDom_makePatch(__3_REMOVE, index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



////////////  ADD DOM NODES  ////////////
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === __3_THUNK)
		{
			_VirtualDom_addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else if (patchType === __3_REORDER)
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var subPatches = patch.data.patches;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === __3_REMOVE)
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var data = patch.data;
			if (typeof data !== 'undefined')
			{
				data.entry.data = domNode;
				var subPatches = data.patches;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.$)
	{
		case __2_TAGGER:
			var subNode = vNode.node;

			while (subNode.$ === __2_TAGGER)
			{
				subNode = subNode.node;
			}

			return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case __2_NODE:
			var vKids = vNode.kids;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vKids.length; j++)
			{
				low++;
				var vKid = vKids[j];
				var nextLow = low + (vKid.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case __2_KEYED_NODE:
			var vKids = vNode.kids;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vKids.length; j++)
			{
				low++;
				var vKid = vKids[j].b;
				var nextLow = low + (vKid.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case __2_TEXT:
		case __2_THUNK:
			__Error_throw(13); // 'should never traverse `text` or `thunk` nodes like this'
	}
}



////////////  APPLY PATCHES  ////////////


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
		var localDomNode = patch.domNode
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
		case __3_REDRAW:
			return _VirtualDom_applyPatchRedraw(domNode, patch.data, patch.eventNode);

		case __3_FACTS:
			_VirtualDom_applyFactPatch(domNode, patch.eventNode, patch.data);
			return domNode;

		case __3_TEXT:
			domNode.replaceData(0, domNode.length, patch.data);
			return domNode;

		case __3_THUNK:
			return _VirtualDom_applyPatchesHelp(domNode, patch.data);

		case __3_TAGGER:
			if (typeof domNode.elm_event_node_ref !== 'undefined')
			{
				domNode.elm_event_node_ref.tagger = patch.data;
			}
			else
			{
				domNode.elm_event_node_ref = { tagger: patch.data, parent: patch.eventNode };
			}
			return domNode;

		case __3_REMOVE_LAST:
			var i = patch.data;
			while (i--)
			{
				domNode.removeChild(domNode.lastChild);
			}
			return domNode;

		case __3_APPEND:
			var newNodes = patch.data;
			for (var i = 0; i < newNodes.length; i++)
			{
				domNode.appendChild(_VirtualDom_render(newNodes[i], patch.eventNode));
			}
			return domNode;

		case __3_REMOVE:
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = _VirtualDom_applyPatchesHelp(domNode, data.patches);
			return domNode;

		case __3_REORDER:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case __3_CUSTOM:
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);

		default:
			__Error_throw(13); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (typeof newNode.elm_event_node_ref === 'undefined')
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyFactPatch(domNode, eventNode, diff)
{
	for (var key in diff)
	{
		var entry = diff[key];
		var tag = entry.$;
		if (tag === __5_INSERT)
		{
			_VirtualDom_applyFact(domNode, eventNode, entry.a);
		}
		else if (tag === __5_REMOVE)
		{
			_VirtualDom_deleteFact(domNode, entry.a);
		}
	}
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.data;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.endInserts, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.patches);

	// inserts
	var inserts = data.inserts;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.entry;
		var node = entry.tag === 'move'
			? entry.data
			: _VirtualDom_render(entry.vnode, patch.eventNode);
		domNode.insertBefore(node, domNode.childNodes[insert.index]);
	}

	// add end inserts
	if (typeof frag !== 'undefined')
	{
		domNode.appendChild(frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: _VirtualDom_render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}


// PROGRAMS

var _VirtualDom_program = _VirtualDom_makeProgram(_VirtualDom_checkNoFlags);
var _VirtualDom_programWithFlags = _VirtualDom_makeProgram(_VirtualDom_checkYesFlags);

function _VirtualDom_makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					_VirtualDom_setup(impl, object, moduleName, checker);
				}
				else
				{
					_Degug_setup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function _VirtualDom_staticProgram(vNode)
{
	var nothing = __Utils_Tuple2( __Utils_Tuple0, __Cmd_none );
	return A2(_VirtualDom_program, elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return __Sub_none; }
	})();
}


// FLAG CHECKERS

function _VirtualDom_checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		__Error_throw(0);
	};
}

function _VirtualDom_checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			__Error_throw(1);
		}

		var result = A2(__Json_run, flagDecoder, flags);
		if (result.$ === 'Ok')
		{
			return init(result.a);
		}

		__Error_throw(2);
	};
}


//  NORMAL SETUP

function _VirtualDom_setup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return __Platform_initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			_VirtualDom_renderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return __Platform_initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			_VirtualDom_renderer(document.body, impl.view)
		);
	};
}


// RENDERER

var _VirtualDom_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };

function _VirtualDom_renderer(domNode, view)
{
	return function(tagger, nextModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var currNode = virtualize(domNode);

		var state = __4_NO_REQUEST;

		function updateIfNeeded()
		{
			switch (state)
			{
				case __4_NO_REQUEST:
					__Error_throw(13); // unexpected draw callback

				case __4_PENDING_REQUEST:
					_VirtualDom_requestAnimationFrame(updateIfNeeded);
					state = __4_EXTRA_REQUEST;

					var nextNode = view(nextModel);
					var patches = _VirtualDom_diff(currNode, nextNode);
					domNode = _VirtualDom_applyPatches(domNode, currNode, patches, eventNode);
					currNode = nextNode;

					return;

				case __4_EXTRA_REQUEST:
					state = __4_NO_REQUEST;
					return;
			}
		}

		function stepper(model)
		{
			if (state === __4_NO_REQUEST)
			{
				_VirtualDom_requestAnimationFrame(updateIfNeeded);
			}
			state = __4_PENDING_REQUEST;
			nextModel = model;
		};

		stepper(nextModel);

		return stepper;
	};
}

function virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}
	// else is normal NODE


	// ATTRIBUTES

	var keys;
	var attrList = __List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		name === 'data-k'
			? keys = value.split('|')
			: attrList = __List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = __List_Nil;
	var kids = node.childNodes;

	// KEYED NODES

	if (keys)
	{
		for (var i = kids.length; i--; )
		{
			kidList = __List_Cons(__Utils_Tuple2(keys[i], virtualize(kids[i])), kidList);
		}
		return A3(_VirtualDom_keyedNode, tag, attrList, kidList);
	}

	// NORMAL NODES

	for (var i = kids.length; i--; )
	{
		kidList = __List_Cons(virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}