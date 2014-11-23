(function() {

var formatfunc = function(value) {
	return Number(value).toLocaleString();
};

var config = {
	width: 701,
	datasource: orb.demo.data,
	dataheaderslocation: 'columns',
	grandtotal: {
		rowsvisible: false,
		columnsvisible: false
	},
	subtotal: {
		visible: true,
	},
	fields: [
		{ name: 'region', caption: 'Region' },
		{ 
			name: 'population',
			caption: 'Population (M)', 
			datasettings: { formatfunc: formatfunc } 
		},
		{ 
			name: 'percent_agricultural_land', 
			caption: '% Agricultural Land', 
			datasettings: { 
				formatfunc: function(value) {
					return value ? value + '%' : '';
				}
			}
		},
		{ 
			name: 'co2_emission', 
			caption: 'Co2 (T.PC)', 
			datasettings: { formatfunc: formatfunc } 
		},
		{ 
			name: 'gdp', 
			caption: 'GDP (B$)', 
			datasettings: { formatfunc: formatfunc } 
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
			tabToHide.button.style.backgroundColor = "white";
			tabToHide.button.addEventListener('click', tabToHide.clickHandler);

			tabToShow.source.style.display = 'block';
			tabToShow.button.style.backgroundColor = "#dfdfdf";
			tabToShow.button.removeEventListener('click', tabToShow.clickHandler);
		}
	}

	tabs['html'].clickHandler();

	new orb.ui.pgridwidget(config).render(document.getElementById('rr'));
}

}());