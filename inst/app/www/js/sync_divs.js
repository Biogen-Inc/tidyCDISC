// rather than have the column block div have its own y-axis scroll
// and the stats block have their own y-axis scroll
// this code is used to "combine" the two divs and have them
// react together to a single scroller

var isSyncingLeftScroll = false;
var isSyncingRightScroll = false;
var leftDiv = document.getElementById('d_blocks');
var rightDiv = document.getElementById('d_agg');

leftDiv.onscroll = function() {
  if (!isSyncingLeftScroll) {
    isSyncingRightScroll = true;
    rightDiv.scrollTop = this.scrollTop;
  }
  isSyncingLeftScroll = false;
}

rightDiv.onscroll = function() {
  if (!isSyncingRightScroll) {
    isSyncingLeftScroll = true;
    leftDiv.scrollTop = this.scrollTop;
  }
  isSyncingRightScroll = false;
}