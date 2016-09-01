"use strict";

// module Susurrant.Math.MultivariateGaussian

var MultivariateNormal = require("multivariate-normal").default;

exports.toSampler_ = function(gaussian) {
  return MultivariateNormal(gaussian['mean'], gaussian['covariance']['unMatrix']);
};

exports.sample_ = function(dist) {
  return function () {
    return dist.sample();
  };
};

exports.sampleN_ = function (n) {
  return function(dist) {
    return function() {
      var samples = new Array(n);
      for (var i = 0; i < n; i++) {
        samples[i] = dist.sample();
      }
      return samples;
    };
  };
};
