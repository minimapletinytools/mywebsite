// shuffle the order of the children of `parent` matching `selector`
function randomizeChildren(parent, selector) {
  if (!parent) return;
  var elems = Array.prototype.filter.call(parent.children, function (e) {
    return e.matches(selector);
  });
  for (var i = elems.length - 1; i > 0; i--) {
    var j = Math.floor(Math.random() * (i + 1));
    var t = elems[i]; elems[i] = elems[j]; elems[j] = t;
  }
  elems.forEach(function (e) { parent.appendChild(e); });
}
