(function() {

var config = {
    width: 721,
	dataSource: orb.demo.data,
	dataHeadersLocation: 'columns',
    bootstrap: false,
	grandTotal: {
		rowsvisible: true,
		columnsvisible: true
	},
	subTotal: {
		visible: true,
        collapsed: true
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
            caption: 'Q'
        }
    ],
    rows    : [ 'Manufacturer', 'Category' ],
    columns : [ 'Class' ],
    data    : [ 'Q', 'Amount' ]
};

window.onload = function() {

	var tabs = {
        'pgrid': {
            button: document.getElementById('demo-pgrid-button'),
            source: document.getElementById('demo-pgrid'),
            clickHandler: showSource('pgrid')
        },
		'html': {
			button: document.getElementById('demo-source-html-button'),
			source: document.getElementById('demo-source-html'),
			clickHandler: showSource('html')
		}
	}

	function showSource(source) {

		return function() {

			var tabToShow = tabs[source];
            for(var tabname in tabs) {
    			var tabToHide = tabs[tabname];
                tabToHide.source.style.display = 'none';
                tabToHide.button.style.color = "#333333";
                //tabToHide.button.style.fontWeight = "normal";
                tabToHide.button.style.backgroundColor = "#f9f9f9";
                tabToHide.button.style.borderBottom = "none";
                tabToHide.button.addEventListener('click', tabToHide.clickHandler);
            }

			tabToShow.source.style.display = 'block';
			tabToShow.button.style.color = "#333333";
            //tabToShow.button.style.fontWeight = "bold";
			tabToShow.button.style.backgroundColor = "white";
            tabToShow.button.style.borderBottom = "1px solid white";
			tabToShow.button.removeEventListener('click', tabToShow.clickHandler);
		}
	}

	tabs['pgrid'].clickHandler();

	new orb.pgridwidget(config).render(document.getElementById('demo-pgrid'));
}

}());