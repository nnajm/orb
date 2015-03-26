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
(min+gz) <strong>js</strong>:21.5Kb/<strong>css</strong>:5.6Kb
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
<a class="anchor" name="rl1.0.8"></a>
##### Mar. 08 2015 -- `1.0.8` [](#rl1.0.8)

- Enhanced scrolling: fixed row headers when scrolling horizontally, fixed column headers when vertically.
- Added filter clear button

<a class="anchor" name="rl1.0.7"></a>
##### Jan. 23 2015 -- `1.0.7` [](#rl1.0.7)

- Visual filters
- Visual themes (supports bootstrap)

<a class="anchor" name="rl1.0.6b"></a>
##### Jan. 01 2015 -- `1.0.6` [](#rl1.0.6)

- Drill down (cell double click)

<a class="anchor" name="rl1.0.5b"></a>
##### Dec. 30 2015 -- `1.0.5` [](#rl1.0.5)

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