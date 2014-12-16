(function() {

var formatfunc = function(value) {
	return Number(value).toLocaleString();
};

var config = {
	width: 841,
	dataSource: orb.demo.data,
	dataHeadersLocation: 'columns',
	grandTotal: {
		rowsvisible: false,
		columnsvisible: false
	},
	subTotal: {
		visible: true,
	},
	fields: [
		{ name: 'region', caption: 'Region' },
		{ 
			name: 'population',
			caption: 'Population (M)', 
			dataSettings: { formatFunc: formatfunc } 
		},
		{ 
			name: 'percent_agricultural_land', 
			caption: '% Agricultural Land', 
			dataSettings: { 
				formatFunc: function(value) {
					return value ? value + '%' : '';
				}
			}
		},
		{ 
			name: 'co2_emission', 
			caption: 'Co2 (T.PC)', 
			dataSettings: { formatFunc: formatfunc } 
		},
		{ 
			name: 'gdp', 
			caption: 'GDP (B$)', 
			dataSettings: { formatFunc: formatfunc } 
		},
		{ name: 'year', caption: 'Year' },
	],
	rows: [
		{ name: 'region' },
	],
	columns: [
		{ name: 'year' }
	],
	data: [
		{ name: 'population' }
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

	new orb.ui.pgridwidget(config).render(document.getElementById('rr'));
}

}());