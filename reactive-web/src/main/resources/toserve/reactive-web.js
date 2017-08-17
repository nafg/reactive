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
  },
  throttle : function(period) {
    var throttled = new EventStream();
    var last = undefined;
    var onTimer = function() {
      if(last !== undefined) throttled.fire(last);
      last = undefined
    }
    var to = window.setTimeout(onTimer, period)
    this.addListener(function(v) {
      window.clearTimeout(to)
      last = v
      to = window.setTimeout(onTimer, period)
    })
    return throttled;
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
  error : function(e) {
    if (window.console)
      console.error(e);
  },
  queuedAjaxEvents : [],
  unique: 0,
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
  doAjax : function(pageId) {
    var q = this.queuedAjaxEvents;
    this.queuedAjaxEvents = [];
    var s = JSON.stringify( { unique: this.unique++, events: q } );
    this.sendAjax[pageId](s);
  },
  sendAjax : { },
  createElem : function(label, attributes, innerHtml) {
    var e = document.createElement(label);
    for (k in attributes)
      e.setAttribute(k, attributes[k]);
    e.innerHTML = innerHtml;
    return e;
  },
  insertChild : function(parentId, child, beforeId) {
    try {
      var p = document.getElementById(parentId);
      if (!p)
        this.error("Error in insertChild('" + parentId + "'," + child + ",'"
            + beforeId + "'): no element " + parentId);
      else {
        var b = document.getElementById(beforeId);
        if (!b)
          this.error("Error in insertChild('" + parentId + "'," + child + ",'"
              + beforeId + "'): no element " + beforeId);
        else
          p.insertBefore(child, b);
      }
    } catch (e) {
      this.error(e);
    }
  },
  appendChild : function(parentId, child) {
    try {
      var p = document.getElementById(parentId);
      if (!p)
        this.error("Error in appendChild('" + parentId + "'," + child
            + "): no element " + parentId);
      else
        p.appendChild(child);
    } catch (e) {
      this.error(e);
    }
  },
  removeChild : function(parentId, oldId) {
    try {
      var p = document.getElementById(parentId);
      if (!p)
        this.error("Error in removeChild('" + parentId + "','" + oldId
            + "'): no element " + parentId);
      else {
        var c = document.getElementById(oldId);
        if (!c)
          this.error("Error in removeChild('" + parentId + "','" + oldId
              + "'): no element " + oldId);
        else
          p.removeChild(c);
      }
    } catch (e) {
      this.error(e);
    }
  },
  replaceChild : function(parentId, child, oldId) {
    try {
      var p = document.getElementById(parentId);
      if (!p)
        this.error("Error in replaceChild('" + parentId + "'," + child + ",'"
            + oldId + "'): no element " + parentId);
      else {
        var o = document.getElementById(oldId);
        if (!o)
          this.error("Error in replaceChild('" + parentId + "'," + child + ",'"
              + oldId + "'): no element " + oldId);
        else
          p.replaceChild(child, o);
      }
    } catch (e) {
      this.error(e);
    }
  },
  replaceAll : function(parentId, innerHtml) {
    try {
      var p = document.getElementById(parentId);
      if (!p)
        this.error("Error in replaceAll('" + parentId + "','" + innerHtml
            + "'): no element " + parentId);
      else
        p.innerHTML = innerHtml;
    } catch (e) {
      this.error(e);
    }
  },
  updateProperty : function(parentId, propertyName, value) {
    try {
      var p = document.getElementById(parentId);
      if (!p)
        this.error("Error in updateProperty('" + parentId + "','"
            + propertyName + "'," + value + "): no element " + parentId);
      else
        p[propertyName] = value;
    } catch (e) {
      this.error(e);
    }
  }
};
