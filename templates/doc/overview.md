<% require './templates/header.html' %>

<div id="sidenav" class="menu">
<div class="tc"><i class="fa fa-bars"></i>Table of Content</div>
<% require menuFilepath %>
</div>

<div class="content">

# %L(Overview)

Orb.js is a library that exposes a pivot grid control, a multi-dimensional data analysis tool. It's main features are:

#### Interactivity

- Move fields between axes using drag'n'drop
- Sort
- Filter
- Drill down
- Multiple data fields
- Grand totals & Sub totals
- Sub totals expand/collapse
- Enhanced scrolling (fixed headers)
- Export to Excel

#### Customization

- Via code and/or toolbar
- Data headers location
- Grand totals visibility
- Sub totals visibility & collapsed state
- Data cells aggregate & format functions
- Theming: built-in & Bootstrap


## %L(Building a pivot grid)

<br/>
Create a configuration object (see [orb.pgridwidget/Options](doc-pgridwidget.html#options)):
```
var config = {
    dataSource: mysource,
    fields: [...],
    rows: [...],
    ...
};
```

Instantiate the pivot grid control using the config object:

    var pgridw = new orb.pgridwidget(config);

Render it inside a container element:

    pgridw.render(document.getElementById('pgridContainer'));

## %L(Data refresh)
<br/>
To refresh bound data source, call:

    pgridw.refreshData(newDataSource);

`newDataSource` should conform to the [dataSource](doc-pgridwidget.html#datasource) option. The pivot grid will be rebuilt. Sorting, filtering, sub totals collaped state will be retained.
<br/>
## %L(Query)
<br/>

The pivot grid can be queried for its summarized data. For that, a `query` object should be created:

- Using an already instanciated pgridwidget:

      var query = pgridwidget.pgrid.query();

- Without building a pivot grid:

  First, create a configuration object:

      var config = {
          dataSource: mysource,
          fields: [...],
          rows: [...],
          ...
      };

  Create a pgrid instance then a query instance:

      var pgrid = new orb.pgrid(config);
      var query = pgrid.query();

For these 2 cases, field captions can be used to query data in a readable manner.

For example, to query about the sales amount of 'Adventures Works' for 'Economic' goods:

    var amount = query
         .Manufacturer('Adventure Works') // - slice Manufacturer dimension on 'Adventure Works' values
         .Class('Economy');               // - slice class dimension on 'Economy' values
         .Amount();                       // - get the amount (using the aggregate function defined in the options)
                                          //   of the sliced data.


A third possibility, is to query jsut an array of data:

    var query = new orb.query(data);

    var amount = query
         .slice(2, 'Adventure Works') // - slice 3rd (0 based indexing) field dimension on 'Adventure Works' values
         .slice(3, 'Economy');        // - slice 4th field dimension on 'Economy' values
         .sum(6);                     // - get 7th field sum of the sliced data.

</div>

<% require './templates/footer.html' %>