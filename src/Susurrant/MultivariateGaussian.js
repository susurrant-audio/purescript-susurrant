"use strict";

// module Susurrant.MultivariateGaussian

var MultivariateNormal = require("multivariate-normal").default;

exports.toSampler_ = function(gaussian) {
  return MultivariateNormal(gaussian['mean'], gaussian['covariance']['unMatrix']);
};

exports.sample_ = function(dist) {
  return function () {
    return dist.sample();
  };
};
