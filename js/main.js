(function() {

var config = {
    width: 841,
	dataSource: orb.demo.data,
	dataHeadersLocation: 'columns',
	grandTotal: {
		rowsvisible: true,
		columnsvisible: true
	},
	subTotal: {
		visible: true,
	},
    fields: [
        {
            name: '6',
            caption: 'Amount',
            dataSettings: {
                aggregateFunc: 'avg',
                formatFunc: function(value) {
                    return Number(value).toFixed(0) + ' $';
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
    rows: [
        { name: '2' }, 
        { name: '4' }
    ],
    columns: [
        { name: '3' }
    ],
    data: [
        { name: '5' }, 
        { name: '6' }
    ]
};

window.onload = function() {

	var tabs = {
		'html': {
			button: document.getElementById('demo-source-html-button'),
			source: document.getElementById('demo-source-html'),
			clickHandler: showSource('html')
		},
		'js': {
			button: document.getElementById('demo-source-js-button'),
			source: document.getElementById('demo-source-js'),
			clickHandler: showSource('js')
		}
	}

	function showSource(source) {

		return function() {

			var tabToShow = tabs[source];
			var tabToHide = tabs[source === 'html' ? 'js' : 'html'];

			tabToHide.source.style.display = 'none';
			tabToHide.button.style.color = "#5bc0de";
			tabToHide.button.style.backgroundColor = "#f9f9f9";
			tabToHide.button.addEventListener('click', tabToHide.clickHandler);

			tabToShow.source.style.display = 'block';
			tabToShow.button.style.color = "#f9f9f9";
			tabToShow.button.style.backgroundColor = "#5bc0de";
			tabToShow.button.removeEventListener('click', tabToShow.clickHandler);
		}
	}

	tabs['html'].clickHandler();

	new orb.pgridwidget(config).render(document.getElementById('rr'));
}

}());