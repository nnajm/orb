
/**
 * @fileOverview Pivot Grid columns viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global orb */
/*jshint eqnull: true*/

// Ensure orb.ui namespace is created
orb.utils.ns('orb.ui');

(function(){

/**
 * Creates a new instance of columns ui properties.
 * @class
 * @memberOf orb.ui
 * @param  {orb.axe} columnsAxe - axe containing all columns dimensions.
 */
orb.ui.cols = function(columnsAxe) {

	var self = this;

	/**
	 * Column dimensions axe
	 * @type {orb.axe}
	 */
	this.axe = columnsAxe;	

	/**
	 * Columns render properties
	 * @type {Array}
	 */
	this.uiInfos = null;

	this.build = function () {

		var uiInfos = [];

		if(self.axe != null) {
			// Fill columns layout infos
			for(var depth = self.axe.root.depth; depth > 1; depth--) {
				uiInfos.push([]);
				getUiInfo(depth, uiInfos);
			}
			(uiInfos[0] = uiInfos[0] || []).push(new orb.ui.header(orb.axe.Type.COLUMNS, orb.ui.HeaderType.GRAND_TOTAL, self.axe.root));
		}
		self.uiInfos = uiInfos;
		console.log('fff');
	}

	this.getAllHeaders = function() {
		var infos = self.uiInfos[self.uiInfos.length - 1];
		var arr = [];
		var cparent = infos[0].parent;

		function pushsubtotal(pheader) {
			if(pheader.dim.field.subtotal.visible) {
				arr.push(pheader.subtotalHeader);
			}
		}

		for(var i = 0; i < infos.length; i++) {
			var header = infos[i];
			if(header.parent != cparent) {
				pushsubtotal(cparent);
				var pparent = header.parent.parent;
				var pcparent = cparent.parent;
				while(pparent != pcparent && pcparent != null) {
					pushsubtotal(pcparent);
					pparent = pparent.parent;
					pcparent = pcparent.parent;
				}
				cparent = header.parent;
			}
			arr.push(header);
			if(cparent != null && i === infos.length - 1) {
				pushsubtotal(cparent);
				pcparent = cparent.parent;
				while(pcparent != null) {
					pushsubtotal(pcparent);
					pcparent = pcparent.parent;
				}
			}
		}
		if(infos[0].parent != null) {
			arr.push(self.uiInfos[0][self.uiInfos[0].length - 1]);
		}
		return arr;
	}

	this.build();

	/**
	 * Calculates the width of a given column header.<br/>
	 * Column's width represents the number of cells it should span to wrap all sub-dimensions to the deepest.
	 * @param  {orb.dimension} dimension - the column header dimension object
	 * @return {Number}
	 */
	function calcWidth(dimension) {
		var width = 0;

		if(!dimension.isLeaf) {
			// subdimvals 'own' properties are the set of values for this dimension
			for(var i = 0; i < dimension.values.length; i++) {
				var subdim = dimension.subdimvals[dimension.values[i]];
				// if its not an array
				if(!subdim.isLeaf) {
					// call its extractValues (recursive)
					width += calcWidth(subdim) + 1;
				} else {
					width += 1;
				}
			}
			return width;
		} else {
			return 1;
		}
	}

	/**
	 * Fills the infos array given in argument with the dimension layout infos as column.
 	 * @param  {orb.dimension}  dimension - the dimension to get ui info for
	 * @param  {int}  depth - the depth of the dimension that it's subdimensions will be returned
	 * @param  {object}  infos - array to fill with ui dimension info
	 */
	function getUiInfo(depth, uiInfos){

		var infos = uiInfos[uiInfos.length - 1];
		var parents = self.axe.root.depth === depth
			? [null]
			: uiInfos[self.axe.root.depth - depth - 1].filter(function(p) {
				return p.type !== orb.ui.HeaderType.SUB_TOTAL;
			});

		for(var pi = 0; pi < parents.length; pi++) {
			
			var parent = parents[pi];
			var parentDim = parent == null ? self.axe.root : parent.dim;

			for(var di = 0; di < parentDim.values.length; di++) {
			
				var subvalue = parentDim.values[di];
				var subdim = parentDim.subdimvals[subvalue];

				var subtotalHeader;
				if(!subdim.isLeaf && subdim.field.subtotal.visible) {
					subtotalHeader = new orb.ui.header(orb.axe.Type.COLUMNS, orb.ui.HeaderType.SUB_TOTAL, subdim, parent);
				} else {
					subtotalHeader = null;
				}

				var header = new orb.ui.header(orb.axe.Type.COLUMNS, null, subdim, parent, subtotalHeader);
				infos.push(header);

				if(!subdim.isLeaf && subdim.field.subtotal.visible) {
					infos.push(subtotalHeader);
				}
			}
		}
	}
};

}());