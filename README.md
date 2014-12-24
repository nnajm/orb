orb
===

Pivot grid javascript library.
<hr/>

##Website:

[nnajm.github.io/orb](http://nnajm.github.io/orb/)

##Features
###Main
- Fields drag'n'drop
- Multi data fields support
- Grand totals &amp; Sub totals
- Sub totals expand/collapse
- Sorting

###Customization
- Data headers location	
- Grand totals visibility	
- Sub totals visibility &amp; collapsed state	
- Data cells aggregate &amp; format functions	

###Data query

Query aggregation results with a simple API:

```javascript
var orb = require('orb');
var pgrid = new orb.pgrid(config);

var amount = pgrid.query()
                  .Manufacturer('Adventure Works')
                  .Class('Economy');
                  .Amount();
```


##Licence
[MIT](https://github.com/nnajm/orb/blob/master/LICENSE)
