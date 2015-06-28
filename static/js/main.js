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
        toggler({
            button: sideMenuElement,
            menu: sideMenuElement,
            onOpen: function(elem, windowSize) {
                elem.style.height = 'auto';
                elem.style.overflow = 'auto';
                elem.style.bottom = 'auto';

                var menuHeight = elem.offsetHeight;
                elem.style.height = Math.min((windowSize.h - 74 - 12), menuHeight) + 'px';
            },
            onClose: function(elem) {
                elem.style.height = '30px';
                elem.style.overflow = 'hidden';
                elem.style.bottom = '0';
            },
            activationWidth: 1600,
            activationHeight: 40
        });
    }

    if(topMenuElement) {
        toggler({
            button: topMenuButton,
            menu: topMenuElement,
            onOpen: function(elem, windowSize) {
                topMenuElement.style.height = 'auto';
            },
            onClose: function(elem) {
                topMenuElement.style.height = '27px';
            },
            activationWidth: 1000,
            activationHeight: 30
        });
    }
};

var togglers = [];

function toggler(options) {

    togglers.push(options);

    function getWindowSize() {
        var W = window,
            d = document,
            e = d.documentElement,
            g = d.getElementsByTagName('body')[0],
            w = W.innerWidth || e.clientWidth || g.clientWidth,
            h = W.innerHeight|| e.clientHeight|| g.clientHeight;
        return { w: w, h: h};
    }
    
    function openMenu(menuOptions) {
        menuOptions = menuOptions || options;
        // close all open menus
        for(var i = 0; i < togglers.length; i++) {
            if(togglers[i] != menuOptions) {
                closeMenu(togglers[i]);
            }
        }

        menuOptions.onOpen(menuOptions.menu, getWindowSize());
    }

    function closeMenu(menuOptions) {
        menuOptions = menuOptions || options;
        if(getWindowSize().w < menuOptions.activationWidth) {
            menuOptions.onClose(menuOptions.menu);
        }
    }

    function ensureMenu() {
        if(getWindowSize().w > options.activationWidth) {
            openMenu();
        } else {
            closeMenu();
        }
    }

    window.addEventListener('resize', function() {
        ensureMenu();
    });

    document.addEventListener('click', function() {
        closeMenu();
    });

    options.button.addEventListener('click', function(e) {
        if(options.menu.offsetHeight <= options.activationHeight) {
            openMenu();

            e.stopPropagation();
            e.preventDefault();
        }
    });
}

}());