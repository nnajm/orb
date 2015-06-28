/** @jsx React.DOM */

/* global module, require, react */
/*jshint eqnull: true*/

'use strict';

var pbid = 0;

module.exports.PivotButton = react.createClass({
	displayName: 'PivotButton',
	getInitialState: function () {
		this.pbid = ++pbid;

		// initial state, all zero.
		return {
			pos: { x: 0, y: 0 },
			startpos: { x: 0, y: 0 },
			mousedown: false,
			dragging: false
		};
	},
	onFilterMouseDown: function(e) {
		// left mouse button only
		if (e.button !== 0) return;

		var filterButton = this.refs.filterButton.getDOMNode();
		var filterButtonPos = reactUtils.getOffset(filterButton);
		var filterContainer = document.createElement('div');

        var filterPanelFactory = React.createFactory(comps.FilterPanel);
        var filterPanel = filterPanelFactory({
            field: this.props.field.name,
            pivotTableComp: this.props.pivotTableComp
        });

        filterContainer.className = this.props.pivotTableComp.pgrid.config.theme.getFilterClasses().container;
        filterContainer.style.top = filterButtonPos.y + 'px';
        filterContainer.style.left = filterButtonPos.x + 'px';
        document.body.appendChild(filterContainer);

        React.render(filterPanel, filterContainer);

		// prevent event bubbling (to prevent text selection while dragging for example)
		e.stopPropagation();
		e.preventDefault();
	},
	componentDidUpdate: function () {
		if (this.props.pivotTableComp.pgrid.config.canMoveFields) {
			if (!this.state.mousedown) {
				// mouse not down, don't care about mouse up/move events.
				dragManager.setDragElement(null);
				document.removeEventListener('mousemove', this.onMouseMove);
			} else if (this.state.mousedown) {
				// mouse down, interested by mouse up/move events.
				dragManager.setDragElement(this);
				document.addEventListener('mousemove', this.onMouseMove);
			}
		}
	},
	componentDidMount: function() {
		this.props.pivotTableComp.registerThemeChanged(this.updateClasses);
	},
	componentWillUnmount : function() {
		this.props.pivotTableComp.unregisterThemeChanged(this.updateClasses);
		document.removeEventListener('mousemove', this.onMouseMove);
	},
	onMouseDown: function(e) {
		// drag/sort with left mouse button
		if (e.button !== 0) return;

		if(e.ctrlKey) {
			this.props.pivotTableComp.toggleFieldExpansion(this.props.axetype, this.props.field);
		} else {

			var thispos = reactUtils.getOffset(this.getDOMNode());
			
			// inform mousedown, save start pos
			this.setState({
				mousedown: true,
				mouseoffset: {
					x: thispos.x - e.pageX,
					y: thispos.y - e.pageY,
				},
				startpos: {
					x: e.pageX,
					y: e.pageY
				}
			});
		}
		
		// prevent event bubbling (to prevent text selection while dragging for example)
		e.stopPropagation();
		e.preventDefault();
	},
	onMouseUp: function(e) {
		
		var isdragged = this.state.dragging;

		this.setState({
			mousedown: false,
			dragging: false,
			size: null,
			pos: {
				x: 0,
				y: 0
			}
		});
		
		if(!e.ctrlKey && !isdragged) {
			// if button was not dragged, proceed as a click
			this.props.pivotTableComp.sort(this.props.axetype, this.props.field);
		}
	},
	onMouseMove: function (e) {
		// if the mouse is not down while moving, return (no drag)
		if (!this.props.pivotTableComp.pgrid.config.canMoveFields || !this.state.mousedown) return;

		var size = null;
		if(!this.state.dragging) {
			size = reactUtils.getSize(this.getDOMNode());
		} else {
			size = this.state.size;
		}

		var newpos = {
			x: e.pageX + this.state.mouseoffset.x,
			y: e.pageY + this.state.mouseoffset.y
		};

		this.setState({
			dragging: true,
			size: size,
			pos: newpos
		});

		dragManager.elementMoved();

		e.stopPropagation();
		e.preventDefault();
	},
	updateClasses: function() {
		this.getDOMNode().className = this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().pivotButton;
	},
	render: function() {
		var self = this;
		var divstyle = {
			left: self.state.pos.x + 'px',
			top: self.state.pos.y + 'px',
			position: self.state.dragging ? 'fixed' : '',
			zIndex: 101
		};

		if(self.state.size) {
			divstyle.width = self.state.size.width + 'px';
		}

		var sortDirectionClass = self.props.field.sort.order === 'asc' ? 
			'sort-asc' :
			//' \u2191' :
			(self.props.field.sort.order === 'desc' ?
				'sort-desc' :
				//' \u2193' :
				'' );
		var filterClass = (self.state.dragging ? '' : 'fltr-btn') + (this.props.pivotTableComp.pgrid.isFieldFiltered(this.props.field.name) ? ' fltr-btn-active' : '');
		var fieldAggFunc = '';
		if(self.props.axetype === axe.Type.DATA) {
			fieldAggFunc = <small>{' (' + self.props.field.aggregateFuncName + ')' }</small>;
		}

		return <div key={self.props.field.name} 
		            className={this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().pivotButton}
		            onMouseDown={this.onMouseDown}
		            onMouseUp={this.onMouseUp}
		            style={divstyle}>
		            <table>
		            	<tbody>
		            		<tr>
		            			<td className="caption">{self.props.field.caption}{fieldAggFunc}</td>
		            			<td><div className={'sort-indicator ' + sortDirectionClass}></div></td>
		            			<td className="filter">
		            				<div ref="filterButton" className={filterClass} onMouseDown={self.state.dragging ? null : this.onFilterMouseDown}></div>
		            			</td>
		            		</tr>
		            	</tbody>
		            </table>
		        </div>;
	}
});