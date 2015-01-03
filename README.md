#[orb](http://nnajm.github.io/orb/)
Pivot grid javascript library.

http://nnajm.github.io/orb

---

> [![NPM](https://nodei.co/npm/orb.png?compact=true)](https://www.npmjs.com/package/orb)

> **cdnjs:** https://cdnjs.com/libraries/orb

###Features
####Interactivity
- Fields drag'n'drop
- Drill down (cell double click)
- Multi data fields support
- Grand totals &amp; Sub totals
- Sub totals expand/collapse
- Sorting
- Fast rendering using [React](http://facebook.github.io/react/index.html)

####Customization
- Data headers location 
- Grand totals visibility
- Sub totals visibility &amp; collapsed state 
- Data cells aggregate &amp; format functions 

####Data query

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


##Licence
[MIT](https://github.com/nnajm/orb/blob/master/LICENSE)
