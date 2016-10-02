"use strict";

var currency = function(x) {
    return x.toLocaleString('en-US', {style: 'currency', currency: 'USD'});
}

exports["currency"] = currency;
