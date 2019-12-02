exports.setSize = function(pct) {
  return function() {
    document.getElementById('bar').style.width = pct + '%';
  };
};
