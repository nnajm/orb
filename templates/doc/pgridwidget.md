<% require './templates/header.html' %>

<div id="sidenav" class="menu">
<div class="tc"><i class="fa fa-bars"></i>Table of Contents</div>
<% require menuFilepath %>
</div>

<div class="content">

# %L(orb.pgridwidget)

## %L(Options)

<br/>
#### %l(dataSource)

Pivot grid data source array. It can contain arrays or objects. When it is an array of objects, field names are object properties. When it is an array of arrays, field names are array indexes.

**%s(Type:)** `Array`  
**%s(Default:)** `null`

--- 

#### %l(dataHeadersLocation)

Data headers location: `'rows'` or `'columns'`.

**%s(Type:)** `String`  
**%s(Default:)** `'rows'`

---

#### %l(canMoveFields)

Whether or not user can move fileds at runtime.

**%s(Type:)** `Boolean`  
**%s(Default:)** `true`

---

#### %l(grandTotal)

Grand total settings. It contains the following sub options:

- rowsvisible: grand total row visibility
- columnsvisible: grand total column visibility

**%s(Type:)** `Object`  
**%s(Default:)** `{ rowsvisible: true, columnsvisible: true }`

---

#### %l[g-](subTotal)

Global subtotals settings for row and column fiels. It contains the following sub options:

- visible: sub total rows visibility
- collapsible : sub total rows can be collapsed or not
- collapsed: sub total rows initial collapsed state

**%s(Type:)** `Object`  
**%s(Default:)** `{ visible: true, collapsible: true, collapsed: false }`

---

#### %l[g-](rowSettings)

Global subtotals settings for row fields. It contains the following sub options:

- subTotal (see Field [subTotal](#subtotal) option)
- sort (see Field [sort](#sort) option)

**%s(Type:)** `Object`  
**%s(Default:)** `{ subTotal: { visible: true, collapsible: true, collapsed: false }, sort: { order: null, customfunc: null } }`

---

#### %l[g-](columnSettings)

Global subtotals settings for column fields. It contains the following sub options:

- subTotal (see Field [subTotal](#subtotal) option)
- sort (see Field [sort](#sort) option)

**%s(Type:)** `Object`  
**%s(Default:)** `{ subTotal: { visible: true, collapsible: true, collapsed: false }, sort: { order: null, customfunc: null } }`

---

#### %l(fields)

Pivot grid fields mapping to data source fields. See [Field options](#Field_options)

**%s(Type:)** `Array<Pivot grid field>`  
**%s(Default:)** `null`

---

#### %l(rows)

Array containing initial _rows axe_ fields. It can contain:

- Pivot grid field objects
- Field names
- Field captions

Fields in this array should be defined first in the [fields](#fields) array.

**%s(Type:)** `Array<Pivot grid field>`  
**%s(Default:)** `null`

---

#### %l(columns)

Array containing initial _columns axe_ fields. It can contain:

- Pivot grid field objects
- Field names
- Field captions

Fields in this array should be defined first in the [fields](#fields) array.

**%s(Type:)** `Array<Pivot grid field>`  
**%s(Default:)** `null`

---

#### %l(data)

Array containing initial _data axe_ fields. It can contain:

- Pivot grid field objects
- Field names
- Field captions

Fields in this array should be defined first in the [fields](#fields) array.

**%s(Type:)** `Array<Pivot grid field>`  
**%s(Default:)** `null`

---

#### %l(preFilters)

Defines how to filter data source before first building the pivot grid.
Each property should map to either the caption or the name of one of the datasource fields.
Property value should be an object with a single property having its name being one of the following operators:

- Matches
- Does Not Match
- =
- <>
- &gt;
- &gt;=
- <
- <=

And its value being either:

- An array, in which case field values not belonging to this array will be filtered out.
- A regular expression. Only valid for _Matches_ and _Does Not Match_ operators.
- A value. Field values will be compared to this value using the operator in the property name.

String comparison and regular expressions searches are always case-insensitive.

*Example:*

    preFilters : {
        'Manufacturer': { 'Matches': /^c/i },      // Keep only Manufacturers starting with 'c'
        'Category'    : { 'Does Not Match': 'D' }, // Keep Categories that does not contain 'd'
        'Amount'      : { '>':  40 },              // Keep only Amounts > 40
        'Q'           : [4, 8, 12]                 // Keep Quantites in [4, 8, 12]
    }

**%s(Type:)** `Object`  
**%s(Default:)** `null`

---

#### %l(toolbar)

Toolbar configuration object:
- visible: Whether or not to show the toolbar (default `false`).

The toolbar contains the following buttons:

![Demo](http://i.imgur.com/pXClXwT.png)

- Collapse/Expand all rows/columns
- Show/hide rows/columns sub-totals
- Show/hide rows/columns grand-total
- Export to Excel

**%s(Type:)** `Object`  
**%s(Default:)** `{ visible: false }`

---

#### %l(theme)

Defines Pivot grid theme. Should be one of the following values:

<table class="prop-theme-table"><tbody>
    <tr><td style="background-color: #C72C48"></td><td>red</td></tr>
    <tr><td style="background-color: #268BD2"></td><td>blue</td></tr>
    <tr><td style="background-color: #3A9D23"></td><td>green</td></tr>
    <tr><td style="background-color: #f7840d"></td><td>orange</td></tr>
    <tr><td style="background-color: #A74AC7"></td><td>flower</td></tr>
    <tr><td style="background-color: #808080"></td><td>gray</td></tr>
    <tr><td style="background-color: #FFFFFF"></td><td>white</td></tr>
    <tr><td style="background-color: #000000"></td><td>black</td></tr>
    <tr><td style="background-color: #FFFFFF; border-style: dotted;"></td><td>bootstrap</td></tr>
</tbody></table>

<br/>
***N.B.:*** bootstrap theme needs bootstrap css files to render properly.

**%s(Type:)** `String`  
**%s(Default:)** `'blue'`

---

#### %l(width)

Pivot grid width in pixel. A horizontal scroll bar appears when data to display needs more space.
By default, the pivot grid extends horizontally as much as it needs.

**%s(Type:)** `Number`  
**%s(Default:)** `null`

---

#### %l(height)

Pivot grid height in pixel. A vertical scroll bar appears when data to display needs more space.
By default, the pivot grid extends vertically as much as it needs.

**%s(Type:)** `Number`  
**%s(Default:)** `null`

<br/>
## %L(Field options)
<br/>
A pivot grid field object is what describes the properties of a field when belonging to the different axes.
It can be used in the [fields](#fields), [rows](#rows), [columns](#columns) and [data](#data) options of the pivot grid.

Field options defined for ___rows___, ___columns___ and ___data___ are only used when the pivot grid is first rendered and while the fields are not moved to other axes. These settings override the ones in ___fields___.

### %L(Properties)
<br/>
#### %l(name)

Data source property name that this field will map.

**%s(Type:)** `String`  
**%s(Default:)** `null`

---

#### %l(caption)

Field caption (displayed on buttons, ...).

**%s(Type:)** `String`  
**%s(Default:)** `field.name`

---

#### %l(subTotal)

Same as pivot grid subTotal option but only applicable to current field. Overrides the global subtotal option.

**%s(Type:)** `Object`  
**%s(Default:)** `{ visible: true, collapsible: true, collapsed: false }`

---

#### %l(sort)

Defines sort settings:

- order: field sort order: `'asc'`/`'desc'`
- customfunc: custom sort function

**%s(Type:)** `Object`  
**%s(Default:)** `{ order: null, customfunc: null }`

---

#### %l(aggregateFunc)

Aggregate function when the field is dropped into the ___data___ axe.
It can be one of the following predefined constants:

- `'sum'`: sum of the values
- `'count'`: count of the values
- `'min'`: min of the values
- `'max'`: max of the values
- `'avg'`: average of the values
- `'prod'`: product of the values
- `'var'`: estimate of the variance of the values, where they are considered as a subset of the entire population
- `'varp'`: estimate of the variance of the values, where they are considered as the entire population
- `'stdev'`: estimate of the standard deviation of the values, where they are considered as a subset of the entire population
- `'stdevp'`: estimate of the standard deviation of the values, where they are considered as the entire population


It can also be a custom aggregation function having the following signature:

    function(datafield, intersection, datasource, rowIndexes, colIndexes): number

  Where:

  - `datafield`: the name of the datafield for which the aggregation is calculated
  - `intersection`: indexes in the data source array of all rows that will participate in this aggregation = drill down result of current cell.
     If the entire data source is included, this parameter will be equal to 'all'.
  - `datasource`: data source array
  - `rowIndexes`: cell rows axe indexes in the data source array
  - `colIndexes`: cell columns axe indexes in the data source array

  Returns:

  aggregation result

**%s(Type:)** `Object`  
**%s(Default:)** `'sum'`

---

#### %l(formatFunc)

Data cell formatting function when the field is dropped into the ___data___ axe.

_Signature:_ `function(value)`, `value`: raw numeric value

_Returns:_ formatted value to display in pivot grid data cells

**%s(Type:)** `Function`  
**%s(Default:)** `.toString()`

---

#### %l(rowSettings)

Settings to use when this field is dropped in the _rows axe_.These settings override pivot grid [fields](#fields) settings.

**%s(Type:)** `Pivot grid field`  
**%s(Default:)** `null`

---

#### %l(columnSettings)

Settings to use when this field is dropped in the _columns axe_.These settings override pivot grid [fields](#fields) settings.

**%s(Type:)** `Pivot grid field`  
**%s(Default:)** `null`

---

#### %l(dataSettings)

Settings to use when this field is dropped in the _data axe_.These settings override pivot grid [fields](#fields) settings.

**%s(Type:)** `Pivot grid field`  
**%s(Default:)** `null`

## %L(Methods)
<br/>
#### %l(changeTheme)

Changes the theme of the pivot grid. See [theme](#theme) option for possible values of _themeName_.
If _themeName_ is not valid, `'blue'` theme will be applied.

**%s(Signature:)** `changeTheme(themeName)`  
**%s(Returns:)** `undefined`

---

#### %l(refreshData)

Changes the datasource of the pivot grid. The pivot grid will be rebuilt, but sorting, filtering, sub totals collaped state will be retained.

**%s(Signature:)** `refreshData(dataArray)`  
**%s(Returns:)** `undefined`

</div>

<% require './templates/footer.html' %>