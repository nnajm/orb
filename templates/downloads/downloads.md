<% require './templates/header.html' %>

<div class="content">

# Downloads <small>[(Release notes)](#rl)</small>
<br/>
Latest stable production build: 

<a href="static/orb-<%= orb_version %>.zip">
<button class="btn btn-info">
  v <%= orb_version %>&#160;<i class="fa fa-download"></i>  
</button>
</a>

<small style="font-style: normal;" class="text-muted">
(min+gz) <strong>js</strong>:25.2Kb/<strong>css</strong>:8.4Kb
</small>
<br/>

##### Debug files: [orb.js](static/js/orb/orb.js), [orb.css](static/css/orb/orb.css)

### Dependencies

orb.js uses facebook [react](http://facebook.github.io/react/) library v0.12.2. You can download it [here](http://fb.me/react-0.12.2.js).


### cdnjs

orb.js is also available at [cdnjs](https://cdnjs.com/) for a faster experience to users:

[https://cdnjs.cloudflare.com/ajax/libs/orb/<%= orb_version %>/orb.min.css](https://cdnjs.cloudflare.com/ajax/libs/orb/<%= orb_version %>/orb.min.css)  
[https://cdnjs.cloudflare.com/ajax/libs/orb/<%= orb_version %>/orb.min.js](https://cdnjs.cloudflare.com/ajax/libs/orb/<%= orb_version %>/orb.min.js)  
[https://cdnjs.cloudflare.com/ajax/libs/orb/<%= orb_version %>/orb.min.js.map](https://cdnjs.cloudflare.com/ajax/libs/orb/<%= orb_version %>/orb.min.js.map)


[![npmjs](static/images/npm-64-square.png)](https://www.npmjs.com/)

orb.js is available through the npm package manager:

**Package:** [https://www.npmjs.com/package/orb](https://www.npmjs.com/package/orb)

**Intallation:**
```
npm install orb
```

**Usage:**
```
var orb = require('orb');
var pgrid = new orb.pgrid(...)
```

<br/>
# Release notes <a class="anchor" href="#rl" name="rl"></a>
<br/>

<a class="anchor" name="rl1.0.9"></a>
#### <u>Jun. 29 2015 -- `1.0.9` [](#rl1.0.9)</u>

##### **New:**

- Export to Excel
- Ctrl+click on field header to expand/collapse all values

- Configuration options:
    - canMoveFields: whether or not fields can be moved at runtime
    - toolbar { visible: bool }: whether or not to show toolbar
    - rowSettings : global row fields settings
    - columnSettings: global column fields settings

- Toolbar:
    - Expand/collapse all rows/columns
    - Show/hide row/column subtotals
    - Show/hide row/column grandtotal
    - Export to Excel

##### **Fixed bugs:**

- [Issue #12](https://github.com/nnajm/orb/issues/12): Header-col getting larger when click on a field button
- [Issue #13](https://github.com/nnajm/orb/issues/13): aggregateFunc option of Field not working
- [Issue #14](https://github.com/nnajm/orb/issues/14): Use of custom aggregation function displays wrong name
- [Issue #16](https://github.com/nnajm/orb/issues/16): Pivot table displays empty instead of zero by default
- [Issue #19](https://github.com/nnajm/orb/issues/19): Drill down: empty grid when data source is an array of objects bug 
- [Issue #23](https://github.com/nnajm/orb/issues/23): JavaScript error when dataSource is an empty array

<a class="anchor" name="rl1.0.8"></a>
#### <u>Mar. 08 2015 -- `1.0.8` [](#rl1.0.8)</u>

- Enhanced scrolling: fixed row headers when scrolling horizontally, fixed column headers when vertically.
- Added filter clear button

<a class="anchor" name="rl1.0.7"></a>
#### <u>Jan. 23 2015 -- `1.0.7` [](#rl1.0.7)</u>

- Visual filters
- Visual themes (supports bootstrap)

<a class="anchor" name="rl1.0.6b"></a>
#### <u>Jan. 01 2015 -- `1.0.6` [](#rl1.0.6)</u>

- Drill down (cell double click)

<a class="anchor" name="rl1.0.5b"></a>
#### <u>Dec. 30 2014 -- `1.0.5` [](#rl1.0.5)</u>

- Drag'n'drop to move fields
- Click to sort
- Multiple data fields support
- Data headers location
- Grand totals & Sub totals
- Sub totals expand/collapse
- Sub totals visibility & collapsed state
- Grand totals visibility
- Data cells aggregate & format functions

</div>

<% require './templates/footer.html' %>