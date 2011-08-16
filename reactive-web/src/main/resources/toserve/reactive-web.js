function EventStream() {
  this.listeners = [];
  this.addListener = this.foreach;
  return this;
}
EventStream.prototype = {
  foreach : function(f) {
    this.listeners.push(f);
  },
  removeListener : function(f) {
    for (l in this.listeners) {
      if (this.listeners[l] === f) {
        delete this.listeners[l];
        break;
      }
    }
  },
  fire : function(v) {
    for (l in this.listeners) {
      this.listeners[l](v);
    }
  },
  map : function(f) {
    var mapped = new EventStream();
    this.addListener(function(v) {
      mapped.fire(f(v));
    });
    return mapped;
  },
  flatMap : function(f) {
    var flatMapped = new EventStream();
    var lastES = null;
    this.addListener(function(v) {
      if (lastES)
        lastES.removeListener(flatMapped.fire);
      lastES = f(v);
      lastES.addListener(flatMapped.fire);
    });
    return flatMapped;
  },
  filter : function(f) {
    var filtered = new EventStream();
    this.addListener(function(v) {
      if (f(v))
        filtered.fire(v);
    });
    return filtered;
  }
};
JSON.stringify = JSON.stringify || function(v) {
  var els;
  switch (typeof (v)) {
  case 'function':
    return '';
  case 'string':
    return '"' + v.replace('"', '\\"') + '"';
  case 'number':
    return '' + v;
  case 'undefined':
    return '';
  case 'boolean':
    return '' + v;
  case 'object':
    if (v instanceof Array) {
      els = [];
      for (i in v)
        els.push(jsonify(v[i]));
      return "[" + String(els) + "],";
    } else {
      els = [];
      for (i in v)
        els.push('"' + i + '":' + jsonify(v[i]));
      return "{" + String(els) + "},";
    }
  }
};
window.reactive = {
  queuedAjaxEvents : [],
  eventStreams : {},
  fire : function(es, value) {
    if (!this.eventStreams[es])
      this.eventStreams[es] = new EventStream();
    this.eventStreams[es].fire(value);
  },
  queueAjax : function(es) {
    return function(value) {
      var e = {};
      e[es] = value;
      reactive.queuedAjaxEvents.push(e);
    };
  },
  doAjax : function() {
    var q = this.queuedAjaxEvents;
    this.queuedAjaxEvents = [];
    var s = JSON.stringify(q);
    liftAjax.lift_ajaxHandler(this.funcId + "=" + encodeURIComponent(s), null,
        null, null);
  }
};
