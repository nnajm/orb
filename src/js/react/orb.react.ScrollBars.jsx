/** @jsx React.DOM */

/* global module, require, React, react, reactUtils, document */
/*jshint eqnull: true*/

'use strict';

var scrollBarMixin = {
  scrollEvent: null,
  scrollClient: null,
  getInitialState: function () {
    // initial state, all zero.
    return {
      size: 16,
      mousedown: false,
      thumbOffset: 0
    };
  },
  componentDidMount: function () {
    this.scrollEvent = new ScrollEvent(this);
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
    // drag with left mouse button
    if (e.button !== 0) return;

    var thumbElem  = this.refs.scrollThumb.getDOMNode();
    var thumbposInParent = reactUtils.getParentOffset(thumbElem);

    reactUtils.addClass(thumbElem, 'orb-scrollthumb-hover');

    // inform mousedown, save start pos
    this.setState({
      mousedown: true,
      mouseoffset: e[this.mousePosProp],
      thumbOffset: thumbposInParent[this.posProp]
    });

    // prevent event bubbling (to prevent text selection while dragging for example)
    e.stopPropagation();
    e.preventDefault();
  },
  onMouseUp: function() {

    if(this.state.mousedown) {
      var thumbElem  = this.refs.scrollThumb.getDOMNode();
      reactUtils.removeClass(thumbElem, 'orb-scrollthumb-hover');
    }

    this.setState({
      mousedown: false
    });
  },
  onMouseMove: function (e) {

    // if the mouse is not down while moving, return (no drag)
    if (!this.state.mousedown) return;

    e.stopPropagation();
    e.preventDefault();

    var amount = e[this.mousePosProp] - this.state.mouseoffset;
    this.state.mouseoffset = e[this.mousePosProp];

    this.scroll(amount);
  },
  getScrollSize: function() {
    if(this.scrollClient != null) {
      return reactUtils.getSize(this.scrollClient)[this.sizeProp];
    } else {
      return reactUtils.getSize(this.getDOMNode())[this.sizeProp];
    }
  },
  setScrollClient: function(scrollClient, scrollCallback) {
    this.scrollClient = scrollClient;
    this.scrollEvent.callback = scrollCallback;
  },
  getScrollPercent: function() {
    var maxOffset = this.getScrollSize() - this.state.size;    
    return maxOffset <= 0 ? 0 : this.state.thumbOffset/maxOffset;
  },
  refresh: function() {
    if(this.scrollClient) {
      var scrolledElement = this.scrollClient.children[0];

      var clientSize = reactUtils.getSize(this.scrollClient);
      var elementSize = reactUtils.getSize(scrolledElement);

      var scrollBarContainerSize = this.getScrollSize();
      var newSize = clientSize[this.sizeProp] >= elementSize[this.sizeProp] ? 0 : (clientSize[this.sizeProp]/elementSize[this.sizeProp]) * scrollBarContainerSize;
      
      this.setState(
        {
          containerSize: scrollBarContainerSize,
          size: newSize,
          thumbOffset: Math.min(this.state.thumbOffset, scrollBarContainerSize - newSize)
        },
        this.scrollEvent.raise
      );

    }
  },
  scroll: function(amount, mode) {
    if(this.state.size > 0) {
      if(mode == 1) amount *= 8;

      var maxOffset = this.getScrollSize() - this.state.size;
      var newOffset = this.state.thumbOffset + amount;
      if(newOffset < 0) newOffset = 0;
      if(newOffset > maxOffset) newOffset = maxOffset;

      this.setState(
        { thumbOffset: newOffset },
        this.scrollEvent.raise
      );
      return true;
    }
    return false;
  },
  onWheel: function(e) {
    this.scroll(e.deltaY, e.deltaMode);
    e.stopPropagation();
    e.preventDefault();
  },
  render: function() {
    var self = this;
    
    var thumbStyle = {padding: 0};
    thumbStyle[this.sizeProp] = this.state.size;
    thumbStyle[this.offsetCssProp] = this.state.thumbOffset;

    var thisStyle = {};
    thisStyle[this.sizeProp] = this.state.containerSize;

    var thumbClass = "orb-scrollthumb " + this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().scrollBar;

    var scrollThumb = this.state.size <= 0 ?
      null : 
      <div className={thumbClass} style={thumbStyle}
           ref="scrollThumb"
           onMouseDown={this.onMouseDown}>
      </div>;

    return  <div className={this.cssClass} style={thisStyle} onWheel={this.onWheel}>
        { scrollThumb }
      </div>;
  }
};

function ScrollEvent(scrollBarComp) {
  var self = this;
  this.scrollBarComp = scrollBarComp;
  this.callback = null;
  this.raise = function() {
    if(self.callback) {
      self.callback(self.scrollBarComp.getScrollPercent());
    }
  };
}

module.exports.HorizontalScrollBar = react.createClass({
  mixins: [scrollBarMixin],
  posProp: 'x',
  mousePosProp: 'pageX',
  sizeProp: 'width',
  offsetCssProp: 'left',
  cssClass: 'orb-h-scrollbar'
});

module.exports.VerticalScrollBar = react.createClass({
  mixins: [scrollBarMixin],
  posProp: 'y',
  mousePosProp: 'pageY',
  sizeProp: 'height',
  offsetCssProp: 'top',
  cssClass: 'orb-v-scrollbar'
});