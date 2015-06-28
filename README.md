# [orb](http://orbjs.net/)[<img align="right" src="https://nodei.co/npm/orb.png?compact=true"/>](https://www.npmjs.com/package/orb)
Pivot grid javascript library.

**Website:** *[orbjs.net](http://orbjs.net/)*

**Latest version:** 1.0.9 ([Release notes](http://orbjs.net/downloads.html#rl))

**cdnjs:** https://cdnjs.com/libraries/orb

### Demo:

![Demo](http://i.imgur.com/xWw6n4t.gif)

### Features
#### Interactivity
- Drag'n'drop to move fields
- Click to sort
- Visual filters
- Drill down (cell double click)
- Multiple data fields support
- Grand totals &amp; Sub totals
- Sub totals expand/collapse
- Enhanced scrolling (fixed headers)
- Export to Excel <small>*<code>(new in 1.0.9)</code>*</small>
- Fast rendering using [React](http://facebook.github.io/react/index.html)

#### Customization
- Via code and/or toolbar
- Data headers location 
- Grand totals visibility
- Sub totals visibility &amp; collapsed state 
- Data cells aggregate &amp; format functions 
- Theming: built-in & Bootstrap

#### Data query

Query aggregation results with a simple API:

```javascript
var orb = require('orb');
var pgrid = new orb.pgrid(config);

// query
var q = pgrid.query()
             .Manufacturer('Adventure Works')
             .Class('Economy');
```
**1 field**
```javascript
q.Amount()

=> 1185.17 
```

**List of fields**
```javascript
q.val('Amount', 'Q')

=> {
     Amount: 1185.17,
     Q: 44
   }
```

       
**Aggregation func**

**builtin**
```javascript
q.stdev('Amount', 'Q');

=> {
     Amount: 1377.58,
     Q: 3.9
   }
```
**custom**
```javascript
q.val({
    // count
    aggregateFunc: function(datafield, intersection, datasource) {
        return intersection.length;
    },
    fields: ['Amount', 'Q']
});

=> {
     Amount: 7,
     Q: 7
   }
```


## Licence
[MIT](https://github.com/nnajm/orb/blob/master/LICENSE)
