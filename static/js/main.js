(function() {

var config = function() {
    return {
        width: 1110,
        height: 645,
    	dataSource: orb.demo.data,
    	dataHeadersLocation: 'columns',
        theme: 'blue',
        toolbar: {
            visible: true
        },
    	grandTotal: {
    		rowsvisible: true,
    		columnsvisible: true
    	},
    	subTotal: {
    		visible: true,
            collapsed: true
    	},
        fields: [
            {
                name: '6',
                caption: 'Amount',
                dataSettings: {
                    aggregateFunc: 'avg',
                    formatFunc: function(value) {
                        return Number(value).toFixed(0);
                    }
                }
            },
            {
                name: '0',
                caption: 'Entity'
            },
            {
                name: '1',
                caption: 'Product',
            },
            {
                name: '2',
                caption: 'Manufacturer',
                sort: {
                    order: 'asc'
                }
            },
            {
                name: '3',
                caption: 'Class'
            },
            {
                name: '4',
                caption: 'Category',
                sort: {
                    order: 'desc'
                }
            },
            {
                name: '5',
                caption: 'Quantity'
            }
        ],
        rows    : [ 'Manufacturer', 'Category' ],
        columns : [ 'Class' ],
        data    : [ 'Quantity', 'Amount' ],
        preFilters : {
            'Manufacturer': { 'Matches': /n/ },
            'Amount'      : { '>':  40 }
        }
    };
};

window.onload = function() {
    var pgridElem = document.getElementById('demo-pgrid');
    var sideMenuElement = document.getElementById('sidenav');
    var topMenuButton = document.getElementById('linkstoggle');
    var topMenuElement = document.getElementById('headerlinks');

    if(pgridElem) {
        new orb.pgridwidget(config()).render(pgridElem);
    }

    if(sideMenuElement) {
        new toggler({
            menu: sideMenuElement,
            onOpen: function(elem, compactMode) {
                elem.style.overflow = 'auto';
                elem.style.height = 'auto';

                if(compactMode) {
                    var menuHeight = elem.offsetHeight;
                    elem.style.height = Math.min((getWindowSize().height - 74 - 24), menuHeight) + 'px';
                }
            },
            onClose: function(elem) {
                elem.style.overflow = 'hidden';
                elem.style.height = '30px';
            },
            isCompactMode: function() {
                return getStyle(sideMenuElement, 'cursor') === 'pointer';
            }
        });
    }

    if(topMenuElement) {
        new toggler({
            button: topMenuButton,
            menu: topMenuElement,
            onOpen: function(elem) {
                topMenuElement.style.height = 'auto';
                topMenuButton.style.borderRadius = '3px 3px 0 0';
            },
            onClose: function(elem) {
                topMenuElement.style.height = '27px';
                topMenuButton.style.borderRadius = '3px';
            },
            isCompactMode: function() {
                return getStyle(topMenuButton.parentNode, 'display') === 'block';
            }
        });
    }
};

var togglers = [];

function toggler(options) {

    var self = this;

    this.options = options;
    
    this.openMenu = function(force) {
        if(force || self.collapsed) {

            // close all open menus except current one
            for(var i = 0; i < togglers.length; i++) {
                if(togglers[i] != self) {
                    togglers[i].closeMenu();
                }
            }

            self.collapsed = false;
            self.options.onOpen(self.options.menu, self.options.isCompactMode());
        }
        self.options.menu.scrollTop = 0;
    };

    this.closeMenu = function() {
        if(!self.collapsed && self.options.isCompactMode()) {
            self.collapsed = true;
            self.options.onClose(self.options.menu);
        }
        self.options.menu.scrollTop = 0;
    }

    this.ensureMenu = function() {
        if(!self.options.isCompactMode()) {
            self.openMenu(true);
        } else {
            self.closeMenu();
        }
    }

    function init() {

        togglers.push(self);

        addEventListener(window, 'resize', self.ensureMenu);
        addEventListener(document, 'click', self.closeMenu);

        self.options.button = self.options.button || self.options.menu;

        addEventListener(self.options.button, 'click', function(e) {
            if(self.collapsed) {
                self.openMenu();

                if(e.stopPropagation) {
                    e.stopPropagation();
                } else {
                    e.cancelBubble = true;
                }

                if(e.preventDefault) {
                    e.preventDefault();
                } else {
                    e.returnValue = false;
                }
            }
        });

        self.collapsed = self.options.isCompactMode();
    }

    init();
}

function addEventListener(element, eventName, callback) {
    if (element.addEventListener) {
        element.addEventListener(eventName, callback);
    }
    else {
        element.attachEvent('on' + eventName, callback);
    }
}

function getWindowSize() {
    var win = window,
        d = document,
        e = d.documentElement,
        g = d.getElementsByTagName('body')[0],
        w = win.innerWidth || e.clientWidth || g.clientWidth,
        h = win.innerHeight|| e.clientHeight|| g.clientHeight;
    return { width: w, height: h};
}

function getStyle(element, styleProp) {
    if(element && styleProp) {
        if (element.currentStyle) {
            return element.currentStyle[styleProp];
        } else if (window.getComputedStyle) {
            return window.getComputedStyle(element, null).getPropertyValue(styleProp);
        }
    }
    return null;
};

}());