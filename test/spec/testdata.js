module.exports.pgridConfig = config();
module.exports.dataSource = data();
module.exports.expected = {
    amount: {
        sum: {
            gt: 265795.89,
            sliced: 8296.22
        },
        avg: {
            gt: 3497.3149,
            sliced: 1185.1736
        },
        stdev: {
            gt: 4345.37,
            sliced: 1377.58
        }
    },
    quantity: {
        sum: {
            gt: 861,
            sliced: 44
        },
        avg: {
            gt: 11.33,
            sliced: 6.29
        },
        stdev: {
            gt: 12.78,
            sliced: 3.9
        }
    }
};

module.exports.params = {
    queryAvgWithNames1: {
        aggregateFunc: 'avg',
        fields: [6]
    },
    queryAvgWithNames2: {
        aggregateFunc: 'avg',
        fields: [5, 6]
    },
    queryStdevWithCaptions1: {
        aggregateFunc: 'stdev',
        fields: ['Amount']
    },
    queryStdevWithCaptions2: {
        aggregateFunc: 'stdev',
        fields: ['Amount', 'Q']
    },
    queryFieldsConfig: {
        '2': {
            caption: 'Manufacturer'
        },
        '3': {
            caption: 'Class'
        },
        '5': {
            toAggregate: true,
            caption: 'Q'
        },
        '6': {
            toAggregate: true,
            caption: 'Amount',
            aggregateFunc: 'avg'
        }
    }
};

function config() {
    return {
        width: 841,
        dataSource: data(),
        dataHeadersLocation: 'columns',
        grandTotal: {
            rowsvisible: true,
            columnsvisible: true
        },
        subTotal: {
            visible: true,
        },
        fields: [{
            name: '6',
            caption: 'Amount',
            dataSettings: {
                aggregateFunc: 'avg',
                formatFunc: function(value) {
                    return Number(value).toFixed(0) + ' $';
                }
            }
        }, {
            name: '0',
            caption: 'Entity'
        }, {
            name: '1',
            caption: 'Product',
        }, {
            name: '2',
            caption: 'Manufacturer',
            sort: {
                order: 'asc'
            }
        }, {
            name: '3',
            caption: 'Class'
        }, {
            name: '4',
            caption: 'Category',
            sort: {
                order: 'desc'
            }
        }, {
            name: '5',
            caption: 'Q'
        }],
        rows: ['Manufacturer', 'Category'],
        columns: ['Class'],
        data: ['Q', 'Amount']
    };
}

function data() {
    return [
        ['Contoso Florida', 'Proseware LCD17W E202 Black', 'Proseware, Inc.', 'Economy', 'Monitors', 4, 509.55],
        ['Contoso New Jersey', 'Adventure Works CRT15 E101 Black', 'Adventure Works', 'Economy', 'Monitors', 4, 351],
        ['Contoso Japan', 'WWI Projector 480p LCD12 Silver', 'Wide World Importers', 'Regular', 'Projectors & Screens', 9, 2038.1],
        ['Contoso Quebec', 'Proseware Screen 80in E1010 Black', 'Proseware, Inc.', 'Economy', 'Projectors & Screens', 4, 419.65],
        ['Contoso North America Online Store', 'Adventure Works LCD15 E100 White', 'Adventure Works', 'Economy', 'Monitors', 4, 381.15],
        ['Contoso Massachusetts', 'WWI CRT17 E106 Black', 'Wide World Importers', 'Economy', 'Monitors', 4, 391.05],
        ['Contoso Iran', 'WWI Projector 480p LCD12 Silver', 'Wide World Importers', 'Regular', 'Projectors & Screens', 9, 2038.1],
        ['Contoso Denmark', 'WWI Screen 85in E1010 White', 'Wide World Importers', 'Economy', 'Projectors & Screens', 4, 549.05],
        ['Contoso South Carolina', 'Proseware Screen 80in E1010 Silver', 'Proseware, Inc.', 'Economy', 'Projectors & Screens', 4, 425.1],
        ['Contoso Colorado', 'Contoso Projector 480p M481 White', 'Contoso, Ltd', 'Regular', 'Projectors & Screens', 9, 4416.15],
        ['Contoso Slovenia', 'Contoso Screen 85in E085 White', 'Contoso, Ltd', 'Economy', 'Projectors & Screens', 4, 542.1],
        ['Contoso Thailand', 'Proseware Projector 1080p LCD86 Silver', 'Proseware, Inc.', 'Deluxe', 'Projectors & Screens', 9, 20655],
        ['Contoso Shanghai', 'Contoso Screen 80in E080 White', 'Contoso, Ltd', 'Economy', 'Projectors & Screens', 4, 419.65],
        ['Contoso Alberta', 'Proseware Projector 480p LCD12 White', 'Proseware, Inc.', 'Regular', 'Projectors & Screens', 9, 2061],
        ['Contoso Australia', 'Proseware Screen 85in E1010 Silver', 'Proseware, Inc.', 'Economy', 'Projectors & Screens', 4, 542.1],
        ['Contoso Scotland', 'Proseware Scan Jet Digital Flat Bed Scanner M300 Grey', 'Proseware, Inc.', 'Regular', 'Printers, Scanners & Fax', 9, 1076.9],
        ['Contoso Virginia', 'Proseware Mobile Receipt and Document Scanner M200 Grey', 'Proseware, Inc.', 'Regular', 'Printers, Scanners & Fax', 9, 1026.6],
        ['Contoso Beijing', 'Proseware Laser Jet All in one X300 White', 'Proseware, Inc.', 'Regular', 'Printers, Scanners & Fax', 9, 1927.2],
        ['Contoso Alberta', 'Proseware Projector 480p LCD12 Black', 'Proseware, Inc.', 'Regular', 'Projectors & Screens', 9, 2026.65],
        ['Contoso Beijing', 'Proseware Ink Jet Instant PDF Sheet-Fed Scanner M300 Grey', 'Proseware, Inc.', 'Regular', 'Printers, Scanners & Fax', 9, 1440],
        ['Contoso Poland', 'Proseware Scan Jet Digital Flat Bed Scanner M300 Grey', 'Proseware, Inc.', 'Regular', 'Printers, Scanners & Fax', 9, 1082.95],
        ['Contoso Canada', 'Proseware Projector 1080p DLP86 Black', 'Proseware, Inc.', 'Deluxe', 'Projectors & Screens', 9, 21991.2],
        ['Contoso Syria', 'Proseware Wireless Photo All-in-One Printer M390 Grey', 'Proseware, Inc.', 'Regular', 'Printers, Scanners & Fax', 9, 1654.4],
        ['Contoso Poland', 'Proseware 23ppm Laser Printer with Wireless and Wired Network Interfaces M680 Black', 'Proseware, Inc.', 'Regular', 'Printers, Scanners & Fax', 9, 1734.6],
        ['Contoso the Netherlands', 'WWI Desktop PC1.80 E1800 White', 'Wide World Importers', 'Economy', 'Desktops', 12, 2724.315],
        ['Contoso Quebec', 'Adventure Works Laptop15 M1500 Black', 'Adventure Works', 'Regular', 'Laptops', 9, 6151.2],
        ['Contoso the Netherlands', 'Adventure Works Desktop PC1.80 ED182 Silver', 'Adventure Works', 'Regular', 'Desktops', 27, 13472.305],
        ['Contoso Malta', 'Adventure Works Desktop PC1.60 ED160 Brown', 'Adventure Works', 'Economy', 'Desktops', 12, 3212.405],
        ['Contoso Iran', 'WWI Desktop PC1.80 E1802 White', 'Wide World Importers', 'Economy', 'Desktops', 12, 3598.8],
        ['Contoso South Carolina', 'Adventure Works Desktop PC1.80 ED182 Silver', 'Adventure Works', 'Regular', 'Desktops', 27, 13472.305],
        ['Contoso Maryland', 'Fabrikam Laptop19W M9800 Black', 'Fabrikam, Inc.', 'Regular', 'Laptops', 9, 10671.1],
        ['Contoso Europe', 'Fabrikam Laptop13.3 M3000 Red', 'Fabrikam, Inc.', 'Regular', 'Laptops', 9, 3382],
        ['Contoso Connecticut', 'WWI Laptop16 M0160 Black', 'Wide World Importers', 'Regular', 'Laptops', 9, 5331.1],
        ['Contoso Maryland', 'Fabrikam Laptop16W M6080 Black', 'Fabrikam, Inc.', 'Regular', 'Laptops', 9, 8654.65],
        ['Contoso Switzerland', 'Fabrikam Laptop15 M5000 White', 'Fabrikam, Inc.', 'Regular', 'Laptops', 9, 6256.05],
        ['Contoso South Korea', 'Fabrikam Laptop19 M9000 Black', 'Fabrikam, Inc.', 'Regular', 'Laptops', 9, 9836.05],
        ['Contoso Virginia', 'Adventure Works Desktop PC1.80 ED180 Black', 'Adventure Works', 'Regular', 'Desktops', 27, 9944.55],
        ['Contoso Quebec', 'Proseware LCD17 E200 White', 'Proseware, Inc.', 'Economy', 'Monitors', 4, 386.1],
        ['Contoso Alaska', 'WWI Desktop PC1.60 E1600 Black', 'Wide World Importers', 'Economy', 'Desktops', 12, 2595.41],
        ['Contoso Canada', 'Adventure Works Desktop PC1.80 ED180 White', 'Adventure Works', 'Regular', 'Desktops', 27, 9963],
        ['Contoso North America Online Store', 'WWI Desktop PC1.60 E1600 Black', 'Wide World Importers', 'Economy', 'Desktops', 12, 2606.4075],
        ['Contoso Guangdong', 'Adventure Works Desktop PC1.60 ED160 White', 'Adventure Works', 'Economy', 'Desktops', 12, 3185.41],
        ['Contoso North America Online Store', 'Adventure Works LCD17W E203 Black', 'Adventure Works', 'Economy', 'Monitors', 4, 516],
        ['Contoso Alaska', 'Adventure Works LCD15 E100 Black', 'Adventure Works', 'Economy', 'Monitors', 4, 381.15],
        ['Contoso Thailand', 'Adventure Works Desktop PC1.80 ED180 White', 'Adventure Works', 'Regular', 'Desktops', 27, 9963],
        ['Contoso New Jersey', 'Adventure Works CRT19 E10 Black', 'Adventure Works', 'Economy', 'Monitors', 4, 269.1],
        ['Contoso Malta', 'Proseware LCD17 E200 Black', 'Proseware, Inc.', 'Economy', 'Monitors', 4, 381.15],
        ['Contoso New York', 'Proseware CRT17 E104 Black', 'Proseware, Inc.', 'Economy', 'Monitors', 4, 224.2],
        ['Contoso Florida', 'Adventure Works Desktop PC1.80 ED180 White', 'Adventure Works', 'Regular', 'Desktops', 27, 9889.2],
        ['Contoso Scotland', 'The Phone Company Touch Screen Phones - CRT M11 Gold', 'The Phone Company', 'Economy', 'Touch Screen Phones ', 4, 718.2],
        ['Contoso Maine', 'Contoso Touch Screen Phones Capacitive M908 Black', 'Contoso, Ltd', 'Regular', 'Touch Screen Phones ', 9, 2756.6],
        ['Contoso Virginia', 'The Phone Company Touch Screen Phones Capacitive M908 Gold', 'The Phone Company', 'Regular', 'Touch Screen Phones ', 9, 2725.8],
        ['Contoso Malta', 'The Phone Company Sharp Touch Screen Phones M910 Grey', 'The Phone Company', 'Regular', 'Touch Screen Phones ', 9, 2678.9],
        ['Contoso the Netherlands', 'Contoso Private Automatic Branch Exchange M65 Black', 'Contoso, Ltd', 'Regular', 'Home & Office Phones', 9, 343.112],
        ['Contoso Guangdong', 'Contoso Sharp Touch Screen Phones M910 Black', 'Contoso, Ltd', 'Regular', 'Touch Screen Phones ', 9, 2663.85],
        ['Contoso the Netherlands', 'The Phone Company Touch Screen Phones Capacitive M908 Grey', 'The Phone Company', 'Regular', 'Touch Screen Phones ', 9, 2710.4],
        ['Contoso Alaska', 'The Phone Company Touch Screen Phones - LCD M12 Gold', 'The Phone Company', 'Economy', 'Touch Screen Phones ', 4, 800],
        ['Contoso Slovenia', 'The Phone Company Touch Screen Phones SAW/On-wall M806 Grey', 'The Phone Company', 'Regular', 'Touch Screen Phones ', 9, 2637],
        ['Contoso Connecticut', 'Contoso Private Automatic Branch Exchange M65 Black', 'Contoso, Ltd', 'Regular', 'Home & Office Phones', 9, 343.112],
        ['Contoso Malta', 'The Phone Company Touch Screen Phones Infrared M901 Gold', 'The Phone Company', 'Regular', 'Touch Screen Phones ', 18, 5340],
        ['Contoso Massachusetts', 'Contoso Private Automatic Branch Exchange M65 Black', 'Contoso, Ltd', 'Regular', 'Home & Office Phones', 9, 345.0615],
        ['Contoso Connecticut', 'The Phone Company PDA Phone 3.7 inches M340 Silver', 'The Phone Company', 'Deluxe', 'Smart phones & PDAs ', 9, 2953.5],
        ['Contoso New Jersey', 'The Phone Company PDA Palm 4.7 inch L850 Black', 'The Phone Company', 'Deluxe', 'Smart phones & PDAs ', 9, 3562.1],
        ['Contoso Catalog Store', 'The Phone Company PDA Wifi 4.7-inch L290 Black', 'The Phone Company', 'Deluxe', 'Smart phones & PDAs ', 9, 3382],
        ['Contoso the Netherlands', 'The Phone Company PDA Phone 3.5 inches M320 White', 'The Phone Company', 'Regular', 'Smart phones & PDAs ', 9, 2661.1],
        ['Contoso Catalog Store', 'The Phone Company Sharp Touch Screen Phones M910 Black', 'The Phone Company', 'Regular', 'Touch Screen Phones ', 9, 2693.95],
        ['Contoso Connecticut', 'The Phone Company PDA Phone Unlocked 3.5 inches M530 Black', 'The Phone Company', 'Regular', 'Smart phones & PDAs ', 9, 2412],
        ['Contoso Scotland', 'The Phone Company PDA Phone Unlocked 3.5 inches M530 Silver', 'The Phone Company', 'Regular', 'Smart phones & PDAs ', 9, 2398.6],
        ['Contoso New Jersey', 'The Phone Company PDA Wifi 4.7-inch L290 White', 'The Phone Company', 'Deluxe', 'Smart phones & PDAs ', 9, 3344],
        ['Contoso Denmark', 'The Phone Company PDA Phone Unlocked 3.5 inches M530 Black', 'The Phone Company', 'Regular', 'Smart phones & PDAs ', 9, 2412],
        ['Contoso Quebec', 'The Phone Company PDA Phone Unlocked 3.7 inches M510 White', 'The Phone Company', 'Regular', 'Smart phones & PDAs ', 9, 2094.4],
        ['Contoso the Netherlands', 'The Phone Company Smart phones 6-LINE SCREEN M21 Gold', 'The Phone Company', 'Regular', 'Smart phones & PDAs ', 9, 2070],
        ['Contoso Guangdong', 'The Phone Company Smart phones without camera E100 White', 'The Phone Company', 'Economy', 'Smart phones & PDAs ', 4, 496.65],
        ['Contoso Poland', 'Cigarette Lighter Adapter for Contoso Phones E110 Black', 'Contoso, Ltd', 'Economy', 'Cell phones Accessories', 80, 1997.9505],
        ['Contoso Shanghai', 'The Phone Company Smart phones without camera E100 Gold', 'The Phone Company', 'Economy', 'Smart phones & PDAs ', 4, 490.2],
        ['Contoso Massachusetts', 'Cigarette Lighter Adapter for Contoso Phones E110 Red', 'Contoso, Ltd', 'Economy', 'Cell phones Accessories', 80, 1999.2]
    ];
}
