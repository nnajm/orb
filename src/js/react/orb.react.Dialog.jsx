/** @jsx React.DOM */

/* global module, require, React */

'use strict';

function createOverlay() {
  var overlayElement = document.createElement('div');
  overlayElement.className = 'orb-overlay orb-overlay-hidden';
  document.body.appendChild(overlayElement);  
  return overlayElement;
}

var Dialog = module.exports.Dialog = react.createClass({
  statics: {
    create: function() {
        var dialogFactory = React.createFactory(Dialog);
        var overlay = createOverlay();

        return {
          show: function(props) {
            React.render(dialogFactory(props), overlay);
          }
        };
    }
  },
  overlayElement: null,
  setOverlayClass: function(visible) {
    this.overlayElement.className = this.props.theme.getDialogClasses(visible).overlay;
  },
  componentDidMount: function() {
    this.overlayElement = this.getDOMNode().parentNode;
    this.setOverlayClass(true);
    this.overlayElement.addEventListener('click', this.close);

    var dialogElement = this.overlayElement.children[0];
    var dialogBodyElement = dialogElement.children[0].children[1];

    var screenWidth = Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
    var screenHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
    var maxHeight = 2*screenHeight/3;
    maxHeight = maxHeight < 301 ? 301 : maxHeight;
    var dWidth = dialogElement.offsetWidth + (dialogElement.offsetHeight > maxHeight ?  11 : 0);
    var dHeight = dialogElement.offsetHeight > maxHeight ? maxHeight : dialogElement.offsetHeight;

    dialogElement.style.top = (screenHeight > dHeight ? (screenHeight - dHeight) / 2 : 0) + 'px';
    dialogElement.style.left = (screenWidth > dWidth ? (screenWidth - dWidth) / 2 : 0) + 'px';
    dialogElement.style.height = dHeight + 'px';
    dialogBodyElement.style.width = dWidth + 'px';
    dialogBodyElement.style.height = (dHeight - 45) + 'px';
  },
  close: function(e) {
    if(e.target == this.overlayElement || e.target.className === 'button-close') {
      this.overlayElement.removeEventListener('click', this.close);
      React.unmountComponentAtNode(this.overlayElement);
      this.setOverlayClass(false);
    }
  },
  render: function() {
    if(this.props.comp) {
      var comp = React.createElement(this.props.comp.type, this.props.comp.props);
      var classes = this.props.theme.getDialogClasses();

      return <div className={classes.dialog} style={ this.props.style || {} }> 
      <div className={classes.content}>
          <div className={classes.header}><div className="button-close" onClick={ this.close }></div><div className={classes.title}>{ this.props.title }</div></div>
          <div className={classes.body}>
          { comp }
          </div>
          </div>
        </div>;
    }
  }
});