
/**
 * @fileOverview Pivot Grid rows viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global orb */
/*jshint eqnull: true*/

// Ensure orb.ui namespace is created
orb.utils.ns('orb.ui');

(function(){

/**
 * Creates a new instance of rows ui properties.
 * @class
 * @memberOf orb.ui
 * @param  {orb.axe} rowsAxe - axe containing all rows dimensions.
 */
orb.ui.rows = function(rowsAxe) {

	var self = this;

	/**
	 * Row dimensions axe
	 * @type {orb.axe}
	 */
	this.axe = rowsAxe;	

	/**
	 * Rows render properties
	 * @type {Array}
	 */
	this.uiInfos = [];

	var _multidatafields;
	var _datafieldscount;

	this.build = function () {

		_datafieldscount = self.axe.pgrid.config.dataheaderslocation === 'rows' ? (self.axe.pgrid.config.datafieldscount || 1) : 1;
		_multidatafields = self.axe.pgrid.config.dataheaderslocation === 'rows' &&  _datafieldscount > 1;

		var uiInfos = [[]];
		if(self.axe != null) {
			// Fill Rows layout infos
			getUiInfo(uiInfos, self.axe.root);

			if(self.axe.pgrid.config.grandtotal.rowsvisible) {
				var lastrow = uiInfos[uiInfos.length - 1];
				var grandtotalHeader = new orb.ui.header(orb.axe.Type.ROWS, orb.ui.HeaderType.GRAND_TOTAL, self.axe.root, null, _datafieldscount);
				if(lastrow.length === 0) {
					lastrow.push(grandtotalHeader);	
				} else {
					uiInfos.push([grandtotalHeader]);
				}

				// add grand-total data headers if more than 1 data field and they will be the leaf headers
				addDataHeaders(uiInfos, grandtotalHeader);
			}

			if(uiInfos[0].length === 0) {
				uiInfos[0].push(new orb.ui.header(orb.axe.Type.ROWS, orb.ui.HeaderType.INNER, self.axe.root, null, _datafieldscount));
			}
			
		}
		self.uiInfos = uiInfos;
	}

	this.build();

	function addDataHeaders(infos, parent) {
		if(_multidatafields) {
			var lastInfosArray = infos[infos.length - 1];
			for(var datafieldindex = 0; datafieldindex < _datafieldscount; datafieldindex++) {
				lastInfosArray.push(new orb.ui.dataHeader(self.axe.pgrid.config.datafields[datafieldindex], parent));
				if(datafieldindex < _datafieldscount - 1) {
					infos.push((lastInfosArray = []));
				}
			}
		}
	}

	/**
	 * Fills the infos array given in argument with the dimension layout infos as row.
	 * @param  {orb.dimension}  dimension - the dimension to get ui info for
	 * @param  {object}  infos - array to fill with ui dimension info
	 */
	function getUiInfo(infos, dimension, totalheader) {
		if(dimension.values.length > 0) {

			var infosMaxIndex = infos.length - 1;
			var lastInfosArray = infos[infosMaxIndex];
			var parent = lastInfosArray.length > 0 ? lastInfosArray[lastInfosArray.length - 1] : null;

			for(var valIndex = 0; valIndex < dimension.values.length; valIndex++) {
				var subvalue = dimension.values[valIndex];
				var subdim = dimension.subdimvals[subvalue];

				var subTotalHeader;
				if(!subdim.isLeaf && subdim.field.subtotal.visible) {
					subTotalHeader = new orb.ui.header(orb.axe.Type.ROWS, orb.ui.HeaderType.SUB_TOTAL, subdim, parent, _datafieldscount);
				} else {
					subTotalHeader = null;
				}
				
				var newHeader = new orb.ui.header(orb.axe.Type.ROWS, null, subdim, parent, _datafieldscount, subTotalHeader);

				if(valIndex > 0) {
					infos.push((lastInfosArray = []));
				}

				lastInfosArray.push(newHeader);

				if(!subdim.isLeaf) {
					getUiInfo(infos, subdim, subTotalHeader);
					if(subdim.field.subtotal.visible) {
						infos.push([subTotalHeader]);

						// add sub-total data headers if more than 1 data field and they will be the leaf headers
						addDataHeaders(infos, subTotalHeader);
					}
				} else {
					// add data headers if more than 1 data field and they will be the leaf headers
					addDataHeaders(infos, newHeader);
				}
			}
		}
	}
};

}());