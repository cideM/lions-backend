(() => {
  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b) {
      return function(a) {
        return f(a)(b);
      };
    };
  };
  var $$const = function(a) {
    return function(v) {
      return a;
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map6 = map(dictApply.Functor0());
    return function(a) {
      return function(b) {
        return apply1(map6($$const(identity2))(a))(b);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var liftA1 = function(dictApplicative) {
    var apply2 = apply(dictApplicative.Apply0());
    var pure1 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply2(pure1(f))(a);
      };
    };
  };

  // output/Control.Bind/index.js
  var bind = function(dict) {
    return dict.bind;
  };

  // output/Data.Array/foreign.js
  var replicateFill = function(count) {
    return function(value12) {
      if (count < 1) {
        return [];
      }
      var result = new Array(count);
      return result.fill(value12);
    };
  };
  var replicatePolyfill = function(count) {
    return function(value12) {
      var result = [];
      var n = 0;
      for (var i = 0; i < count; i++) {
        result[n++] = value12;
      }
      return result;
    };
  };
  var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons(head, tail) {
      this.head = head;
      this.tail = tail;
    }
    var emptyList = {};
    function curryCons(head) {
      return function(tail) {
        return new Cons(head, tail);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr2) {
      return function(xs) {
        return listToArray(foldr2(curryCons)(emptyList)(xs));
      };
    };
  }();
  var length = function(xs) {
    return xs.length;
  };
  var filter = function(f) {
    return function(xs) {
      return xs.filter(f);
    };
  };
  var partition = function(f) {
    return function(xs) {
      var yes = [];
      var no = [];
      for (var i = 0; i < xs.length; i++) {
        var x = xs[i];
        if (f(x))
          yes.push(x);
        else
          no.push(x);
      }
      return { yes, no };
    };
  };
  var sortByImpl = function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from, to) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from + (to - from >> 1);
      if (mid - from > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, from, mid);
      if (to - mid > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
      i = from;
      j = mid;
      k = from;
      while (i < mid && j < to) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare2(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2) {
      return function(fromOrdering) {
        return function(xs) {
          var out;
          if (xs.length < 2)
            return xs;
          out = xs.slice(0);
          mergeFromTo(compare2, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      };
    };
  }();

  // output/Data.Semigroup/foreign.js
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    var bind3 = bind(dictMonad.Bind1());
    var pure3 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind3(f)(function(f$prime) {
          return bind3(a)(function(a$prime) {
            return pure3(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Bounded/foreign.js
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Maybe/index.js
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };

  // output/Data.Monoid/index.js
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/foreign.js
  var pureE = function(a) {
    return function() {
      return a;
    };
  };
  var bindE = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name15, moduleName, init) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2)
        return val;
      if (state2 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init();
      state2 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

  // output/Data.Array.ST/foreign.js
  var sortByImpl2 = function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from, to) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from + (to - from >> 1);
      if (mid - from > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, from, mid);
      if (to - mid > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
      i = from;
      j = mid;
      k = from;
      while (i < mid && j < to) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare2(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2) {
      return function(fromOrdering) {
        return function(xs) {
          return function() {
            if (xs.length < 2)
              return xs;
            mergeFromTo(compare2, fromOrdering, xs, xs.slice(0), 0, xs.length);
            return xs;
          };
        };
      };
    };
  }();

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init) {
      return function(xs) {
        var acc = init;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init) {
      return function(xs) {
        var acc = init;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure3 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr2 = foldr(dictFoldable);
      return function(f) {
        return foldr2(function($454) {
          return applySecond2(f($454));
        })(pure3(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_1 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_1(dictFoldable));
    };
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr2 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append2 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr2(function(x) {
          return function(acc) {
            return append2(f(x))(acc);
          };
        })(mempty2);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a) {
      return [a];
    }
    function array2(a) {
      return function(b) {
        return [a, b];
      };
    }
    function array3(a) {
      return function(b) {
        return function(c) {
          return [a, b, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply2) {
      return function(map6) {
        return function(pure3) {
          return function(f) {
            return function(array) {
              function go2(bot, top2) {
                switch (top2 - bot) {
                  case 0:
                    return pure3([]);
                  case 1:
                    return map6(array1)(f(array[bot]));
                  case 2:
                    return apply2(map6(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply2(apply2(map6(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                    return apply2(map6(concat2)(go2(bot, pivot)))(go2(pivot, top2));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Traversable/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse22 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse22(dictApplicative)(identity3);
    };
  };
  var traversableArray = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
    },
    sequence: function(dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
    },
    Functor0: function() {
      return functorArray;
    },
    Foldable1: function() {
      return foldableArray;
    }
  };

  // output/Data.String.Common/foreign.js
  var joinWith = function(s) {
    return function(xs) {
      return xs.join(s);
    };
  };

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output/Effect.Exception/index.js
  var $$throw = function($4) {
    return throwException(error($4));
  };

  // output/Web.DOM.DOMTokenList/foreign.js
  function add2(list) {
    return function(token) {
      return function() {
        return list.add(token);
      };
    };
  }
  function remove(list) {
    return function(token) {
      return function() {
        return list.remove(token);
      };
    };
  }

  // output/Data.Nullable/foreign.js
  function nullable(a, r, f) {
    return a == null ? r : f(a);
  }

  // output/Data.Nullable/index.js
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Web.DOM.Element/foreign.js
  var getProp = function(name15) {
    return function(doctype) {
      return doctype[name15];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");
  function classList(element) {
    return function() {
      return element.classList;
    };
  }
  function setAttribute(name15) {
    return function(value12) {
      return function(element) {
        return function() {
          element.setAttribute(name15, value12);
        };
      };
    };
  }
  function _getAttribute(name15) {
    return function(element) {
      return function() {
        return element.getAttribute(name15);
      };
    };
  }

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var children = getEffProp("children");
  var _firstElementChild = getEffProp("firstElementChild");
  var _lastElementChild = getEffProp("lastElementChild");
  var childElementCount = getEffProp("childElementCount");
  function _querySelector(selector) {
    return function(node) {
      return function() {
        return node.querySelector(selector);
      };
    };
  }
  function querySelectorAll(selector) {
    return function(node) {
      return function() {
        return node.querySelectorAll(selector);
      };
    };
  }

  // output/Web.DOM.ParentNode/index.js
  var map2 = /* @__PURE__ */ map(functorEffect);
  var QuerySelector = function(x) {
    return x;
  };
  var querySelector = function(qs) {
    var $2 = map2(toMaybe);
    var $3 = _querySelector(qs);
    return function($4) {
      return $2($3($4));
    };
  };

  // output/Web.Internal.FFI/foreign.js
  function _unsafeReadProtoTagged(nothing, just, name15, value12) {
    if (typeof window !== "undefined") {
      var ty = window[name15];
      if (ty != null && value12 instanceof ty) {
        return just(value12);
      }
    }
    var obj = value12;
    while (obj != null) {
      var proto = Object.getPrototypeOf(obj);
      var constructorName = proto.constructor.name;
      if (constructorName === name15) {
        return just(value12);
      } else if (constructorName === "Object") {
        return nothing;
      }
      obj = proto;
    }
    return nothing;
  }

  // output/Web.Internal.FFI/index.js
  var unsafeReadProtoTagged = function(name15) {
    return function(value12) {
      return _unsafeReadProtoTagged(Nothing.value, Just.create, name15, value12);
    };
  };

  // output/Web.DOM.Element/index.js
  var map3 = /* @__PURE__ */ map(functorEffect);
  var toParentNode = unsafeCoerce2;
  var toEventTarget = unsafeCoerce2;
  var getAttribute = function(attr) {
    var $6 = map3(toMaybe);
    var $7 = _getAttribute(attr);
    return function($8) {
      return $6($7($8));
    };
  };
  var fromNode = /* @__PURE__ */ unsafeReadProtoTagged("Element");

  // output/Web.DOM.Node/foreign.js
  var getEffProp2 = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var baseURI = getEffProp2("baseURI");
  var _ownerDocument = getEffProp2("ownerDocument");
  var _parentNode = getEffProp2("parentNode");
  var _parentElement = getEffProp2("parentElement");
  var childNodes = getEffProp2("childNodes");
  var _firstChild = getEffProp2("firstChild");
  var _lastChild = getEffProp2("lastChild");
  var _previousSibling = getEffProp2("previousSibling");
  var _nextSibling = getEffProp2("nextSibling");
  var _nodeValue = getEffProp2("nodeValue");
  var textContent = getEffProp2("textContent");

  // output/Web.DOM.Node/index.js
  var toEventTarget2 = unsafeCoerce2;

  // output/Web.DOM.NodeList/foreign.js
  function toArray(list) {
    return function() {
      return [].slice.call(list);
    };
  }

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target5) {
          return function() {
            return target5.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.HTML.HTMLDocument/foreign.js
  function _body(doc) {
    return doc.body;
  }

  // output/Web.HTML.HTMLDocument/index.js
  var map4 = /* @__PURE__ */ map(functorEffect);
  var body = function(doc) {
    return map4(toMaybe)(function() {
      return _body(doc);
    });
  };

  // output/Web.HTML.HTMLElement/index.js
  var toElement = unsafeCoerce2;

  // output/Web.HTML.HTMLInputElement/foreign.js
  function checked(input) {
    return function() {
      return input.checked;
    };
  }
  function setChecked(checked2) {
    return function(input) {
      return function() {
        input.checked = checked2;
      };
    };
  }

  // output/Web.HTML.HTMLInputElement/index.js
  var fromNode2 = /* @__PURE__ */ unsafeReadProtoTagged("HTMLInputElement");

  // output/Web.HTML.Window/foreign.js
  function document(window2) {
    return function() {
      return window2.document;
    };
  }

  // output/Main/index.js
  var bind2 = /* @__PURE__ */ bind(bindEffect);
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse2 = /* @__PURE__ */ traverse(traversableArray)(applicativeEffect);
  var map5 = /* @__PURE__ */ map(functorArray);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableArray);
  var append1 = /* @__PURE__ */ append(semigroupArray);
  var AllSelected = /* @__PURE__ */ function() {
    function AllSelected2() {
    }
    ;
    AllSelected2.value = new AllSelected2();
    return AllSelected2;
  }();
  var NotAllSelected = /* @__PURE__ */ function() {
    function NotAllSelected2() {
    }
    ;
    NotAllSelected2.value = new NotAllSelected2();
    return NotAllSelected2;
  }();
  var NoneSelected = /* @__PURE__ */ function() {
    function NoneSelected2() {
    }
    ;
    NoneSelected2.value = new NoneSelected2();
    return NoneSelected2;
  }();
  var setMails = function(v) {
    return function(v1) {
      if (v.length === 0) {
        return function __do2() {
          setAttribute("href")("")(v1)();
          var list = classList(v1)();
          return add2(list)("disabled")();
        };
      }
      ;
      return function __do2() {
        setAttribute("href")("mailto:" + joinWith(",")(v))(v1)();
        var list = classList(v1)();
        return remove(list)("disabled")();
      };
    };
  };
  var getCheckboxes = function(root) {
    return bind2(querySelectorAll("[data-email]")(toParentNode(root)))(toArray);
  };
  var getEmailState = function(root) {
    var getState = function(total) {
      return function(checked2) {
        if (checked2 === total) {
          return AllSelected.value;
        }
        ;
        if (checked2 === 0) {
          return NoneSelected.value;
        }
        ;
        if (otherwise) {
          return NotAllSelected.value;
        }
        ;
        throw new Error("Failed pattern match at Main (line 37, column 3 - line 40, column 33): " + [total.constructor.name, checked2.constructor.name]);
      };
    };
    var getCheckboxState = function(checkbox) {
      return function __do2() {
        var checkboxAsEl = maybe($$throw("couldn't convert checkbox to element"))(pure2)(fromNode(checkbox))();
        var checkbox$prime = maybe($$throw("couldn't convert checkbox to input element"))(pure2)(fromNode2(checkbox))();
        var isChecked = checked(checkbox$prime)();
        var email = bind2(getAttribute("data-email")(checkboxAsEl))(maybe($$throw("no data-email attr"))(pure2))();
        return {
          email,
          checked: isChecked
        };
      };
    };
    return function __do2() {
      var itemsState = bind2(getCheckboxes(root))(traverse2(getCheckboxState))();
      var state2 = getState(length(itemsState))(length(filter(function(v2) {
        return v2.checked;
      })(itemsState)));
      var v = partition(function(v1) {
        return v1.checked;
      })(itemsState);
      return {
        state: state2,
        uncheckedEmails: map5(function(v1) {
          return v1.email;
        })(v.no),
        checkedEmails: map5(function(v1) {
          return v1.email;
        })(v.yes)
      };
    };
  };
  var setAllCheckboxes = function(newValue) {
    return function(root) {
      return function __do2() {
        var checkboxes = getCheckboxes(root)();
        return for_2(checkboxes)(function(el) {
          return function __do3() {
            var checkbox = maybe($$throw("couldn't convert to input element"))(pure2)(fromNode2(el))();
            return setChecked(newValue)(checkbox)();
          };
        })();
      };
    };
  };
  var deactivateAll = /* @__PURE__ */ setAllCheckboxes(false);
  var buttonOn = function(el) {
    return function __do2() {
      var list = classList(el)();
      remove(list)("text-primary")();
      return add2(list)("text-muted")();
    };
  };
  var buttonOff = function(el) {
    return function __do2() {
      var list = classList(el)();
      add2(list)("text-primary")();
      return remove(list)("text-muted")();
    };
  };
  var onClickCheckbox = function(root) {
    return function(toggleButton) {
      return function(mailToButton) {
        return $$const(function __do2() {
          var v = getEmailState(root)();
          (function() {
            if (v.state instanceof AllSelected) {
              return buttonOff(toggleButton)();
            }
            ;
            return buttonOn(toggleButton)();
          })();
          return setMails(v.checkedEmails)(mailToButton)();
        });
      };
    };
  };
  var activateAll = /* @__PURE__ */ setAllCheckboxes(true);
  var onClickToggleButton = function(root) {
    return function(toggleButton) {
      return function(mailToButton) {
        return $$const(function __do2() {
          var v = getEmailState(root)();
          if (v.state instanceof AllSelected) {
            deactivateAll(root)();
            buttonOn(toggleButton)();
            return setMails([])(mailToButton)();
          }
          ;
          if (v.state instanceof NotAllSelected) {
            activateAll(root)();
            buttonOff(toggleButton)();
            return setMails(append1(v.checkedEmails)(v.uncheckedEmails))(mailToButton)();
          }
          ;
          if (v.state instanceof NoneSelected) {
            activateAll(root)();
            buttonOff(toggleButton)();
            return setMails(append1(v.checkedEmails)(v.uncheckedEmails))(mailToButton)();
          }
          ;
          throw new Error("Failed pattern match at Main (line 95, column 9 - line 107, column 46): " + [v.state.constructor.name]);
        });
      };
    };
  };
  var initializeUserEmailFeature = function(root) {
    var qs = function($40) {
      return querySelector(QuerySelector($40));
    };
    var qsOrThrow = function(msg) {
      return function(sel) {
        return function(el) {
          return bind2(qs(sel)(el))(maybe($$throw(msg))(pure2));
        };
      };
    };
    return function __do2() {
      var listItems = getCheckboxes(root)();
      var mailToButton = qs("#email-button")(toParentNode(root))();
      if (mailToButton instanceof Nothing) {
        return unit;
      }
      ;
      if (mailToButton instanceof Just) {
        var toggleButton = qsOrThrow("toggle mail to button not found")("#toggle-email-button")(toParentNode(root))();
        var toggleListener = eventListener(onClickToggleButton(root)(toggleButton)(mailToButton.value0))();
        addEventListener("click")(toggleListener)(false)(toEventTarget(toggleButton))();
        var checkboxListener = eventListener(onClickCheckbox(root)(toggleButton)(mailToButton.value0))();
        for_2(listItems)(function() {
          var $41 = addEventListener("click")(checkboxListener)(false);
          return function($42) {
            return $41(toEventTarget2($42));
          };
        }())();
        return unit;
      }
      ;
      throw new Error("Failed pattern match at Main (line 125, column 3 - line 133, column 16): " + [mailToButton.constructor.name]);
    };
  };
  var main = function __do() {
    var htmlDoc = bind2(windowImpl)(document)();
    var body2 = bind2(body(htmlDoc))(maybe($$throw("body not found"))(pure2))();
    return initializeUserEmailFeature(toElement(body2))();
  };

  // <stdin>
  main();
})();
