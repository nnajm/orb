
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

	this.build = function () {

		var uiInfos = [[]];
		if(self.axe != null) {
			// Fill Rows layout infos
			getUiInfo(uiInfos, self.axe.root);
			var lastrow = uiInfos[uiInfos.length - 1];
			var grandtotalHeader = new orb.ui.header(orb.axe.Type.ROWS, orb.ui.HeaderType.GRAND_TOTAL, self.axe.root);
			if(lastrow.length === 0) {
				lastrow.push(grandtotalHeader);	
			} else {
				uiInfos.push([grandtotalHeader]);
			}
			
		}
		self.uiInfos = uiInfos;
	}

	this.build();

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
					subTotalHeader = new orb.ui.header(orb.axe.Type.ROWS, orb.ui.HeaderType.SUB_TOTAL, subdim, parent);
				} else {
					subTotalHeader = null;
				}
				
				var newHeader = new orb.ui.header(orb.axe.Type.ROWS, null, subdim, parent, subTotalHeader);

				if(valIndex === 0) {
					lastInfosArray.push(newHeader);
				} else {
					infos.push([newHeader]);
				}
				if(!subdim.isLeaf) {
					getUiInfo(infos, subdim, subTotalHeader);
					if(subdim.field.subtotal.visible) {
						infos.push([subTotalHeader]);
					}
				}
			}
		}
	}
};

}());