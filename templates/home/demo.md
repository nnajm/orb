<div id="demo-pgrid" class="demo-pgrid" style="padding-top: 17px;"></div>

<div class="demo-source">

<div style="text-align: center;margin: 25px 0">
<h1>Code</h1>
</div>

<br/>
#### Javascript
<br/>
```
  // data source
  var data = [
      ['Contoso Florida', ...],
      ['Contoso New Jersey', ...],
      ...
  ];

  // pivot grid options
  var config = {
      dataSource: data,
      dataHeadersLocation: 'columns',
      theme: 'blue',
      grandTotal: {
          rowsvisible: true,
          columnsvisible: true
      },
      subTotal: {
          visible: true,
          collapsed: false
      },
      fields: [
          {
              name: '6', caption: 'Amount',
              dataSettings: {
                  aggregateFunc: 'avg',
                  formatFunc: function(value) {
                      return Number(value).toFixed(0);
                  }
              }
          }, 
          { name: '0', caption: 'Entity' },
          { name: '1', caption: 'Product' },
          { name: '2', caption: 'Manufacturer', sort: { order: 'asc' } },
          { name: '3', caption: 'Class' },
          { name: '4', caption: 'Category', sort: { order: 'desc' } },
          { name: '5', caption: 'Q' }
      ],
      rows    : [ 'Manufacturer', 'Category' ],
      columns : [ 'Class' ],
      data    : [ 'Q', 'Amount' ];
      preFilters: {
          'Manufacturer': { 'Matches': /n/ },
          'Amount': { '>': 40 },
      },
      width: 1110,
      height: 645,
  }

  // instantiate and show the pivot grid
  new orb.pgridwidget(config).
         .render(document.getElementById('pgrid'));
```

<br/>
#### HTML
<br/>

``` markup
<!DOCTYPE html>
<html>
  <head>
    <title>Orb pivot grid demo</title>

    <link rel="stylesheet" type="text/css" href="orb.min.css" />

    <script type="text/javascript" src="react-0.12.0.min.js"></script>
    <script type="text/javascript" src="orb.min.js"></script>
  </head>

  <body>
    <div id="pgrid"></div>
  </body>
</html>  
```
</div>