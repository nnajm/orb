(function() {

    var formatfunc = function(value) {
    	return Number(value).toLocaleString();
    };

	var config = {
		width: 601,
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
			{ name: 'population', caption: 'Population (M)', datasettings: { formatfunc: formatfunc } },
			{ 
				name: 'percent_agricultural_land', 
				caption: '% Agricultural Land', 
				datasettings: { 
					formatfunc: function(value) {
						return value ? value + '%' : '';
					}
				}
			},
			{ name: 'co2_emission', caption: 'Co2 (T.PC)', datasettings: { formatfunc: formatfunc } },
			{ name: 'gdp', caption: 'GDP (B$)', datasettings: { formatfunc: formatfunc } },
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
		new orb.ui.pgridwidget(config).render(document.getElementById('rr'));
	}

}());