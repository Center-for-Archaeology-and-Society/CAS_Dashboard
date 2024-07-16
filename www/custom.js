// custom.js
shinyjs.showLoading = function() {
  var loadingDiv = document.createElement('div');
  loadingDiv.id = 'loading-popup';
  loadingDiv.style.position = 'fixed';
  loadingDiv.style.top = '50%';
  loadingDiv.style.left = '50%';
  loadingDiv.style.transform = 'translate(-50%, -50%)';
  loadingDiv.style.padding = '20px';
  loadingDiv.style.backgroundColor = 'white';
  loadingDiv.style.border = '1px solid black';
  loadingDiv.style.zIndex = '9999';
  loadingDiv.innerHTML = 'Loading...';
  document.body.appendChild(loadingDiv);
};

shinyjs.hideLoading = function() {
  var loadingDiv = document.getElementById('loading-popup');
  if (loadingDiv) {
    document.body.removeChild(loadingDiv);
  }
};
