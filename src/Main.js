// module Example.Intro

exports.attachHandler = function(selector) {
  return function(callback) {
    var el = document.querySelector(selector);
    el.addEventListener("click", callback);
  };
};
