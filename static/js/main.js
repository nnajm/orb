(function() {

var config = {
    width: 886,
    height: 645,
	dataSource: orb.demo.data,
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
            name: '6',
            caption: 'Amount',
            dataSettings: {
                aggregateFunc: 'avg',
                formatFunc: function(value) {
                    return Number(value).toFixed(0);
                }
            }
        },
        {
            name: '0',
            caption: 'Entity'
        },
        {
            name: '1',
            caption: 'Product',
        },
        {
            name: '2',
            caption: 'Manufacturer',
            sort: {
                order: 'asc'
            }
        },
        {
            name: '3',
            caption: 'Class'
        },
        {
            name: '4',
            caption: 'Category',
            sort: {
                order: 'desc'
            }
        },
        {
            name: '5',
            caption: 'Quantity'
        }
    ],
    rows    : [ 'Manufacturer', 'Category' ],
    columns : [ 'Class' ],
    data    : [ 'Quantity', 'Amount' ],
    preFilters : {
        'Manufacturer': { 'Matches': /n/ },
        'Amount'      : { '>':  40 }
    }
};

window.onload = function() {
    new orb.pgridwidget(config).render(document.getElementById('demo-pgrid'));
};

}());