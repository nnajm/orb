/** @jsx React.DOM */

/* global module, require, React, react, reactUtils, document */

'use strict';

module.exports.HorizontalScrollBar = react.createClass({
  getInitialState: function () {
    // initial state, all zero.
    return {
      mousedown: false,
      x: 0
    };
  },
  componentDidUpdate: function () {
    if (!this.state.mousedown) {
      // mouse not down, don't care about mouse up/move events.
      document.removeEventListener('mousemove', this.onMouseMove);
      document.removeEventListener('mouseup', this.onMouseUp);
    } else if (this.state.mousedown) {
      // mouse down, interested by mouse up/move events.
      document.addEventListener('mousemove', this.onMouseMove);
      document.addEventListener('mouseup', this.onMouseUp);
    }
  },
  componentWillUnmount : function() {
    document.removeEventListener('mousemove', this.onMouseMove);
    document.removeEventListener('mouseup', this.onMouseUp);
  },
  onMouseDown: function(e) {
    // drag/sort with left mouse button
    if (e.button !== 0) return;

    var thumbElem  = this.refs.scrollThumb.getDOMNode();
    var thumbposAbs = reactUtils.getParentOffset(thumbElem);
    var thumbposInParent = reactUtils.getParentOffset(thumbElem);
    
    // inform mousedown, save start pos
    this.setState({
      mousedown: true,
      mouseoffsetX: e.pageX,
      x0: thumbposInParent.x,
      x: thumbposInParent.x
    });

    // prevent event bubbling (to prevent text selection while dragging for example)
    e.stopPropagation();
    e.preventDefault();
  },
  onMouseUp: function() {

    this.setState({
      mousedown: false
    });

    return true;
  },
  onMouseMove: function (e) {
    // if the mouse is not down while moving, return (no drag)
    if (!this.state.mousedown) return;

    this.setState({
      x: this.state.x0 + (e.pageX - this.state.mouseoffsetX)
    });

    e.stopPropagation();
    e.preventDefault();
  },
  render: function() {
    var self = this;

    return  <div style={{ position: 'relative', height: 16 }}>
        <div style={{ position: 'absolute', width:16, height: 16, border: '1px solid #ccc', backgroundColor: '#ddd', top: 0, left: this.state.x }}
             ref="scrollThumb"
             onMouseDown={this.onMouseDown}>
        </div>
      </div>;
  }
});