

#' my_cols
#'
#' Grab a quick color pallette
#'
#' @importFrom RColorBrewer brewer.pal
#' 
#' @noRd
my_cols <- RColorBrewer::brewer.pal(7,"Pastel2")

css <- paste0("
              .nav li a.disabled {
              background-color: #aaa !important;
              color: #333 !important;
              cursor: not-allowed !important;
              border-color: #aaa !important;
              }
              
              .vis-item.DS { background-color: ",my_cols[1],"; }
              .vis-item.CM { background-color: ",my_cols[2],"; }
              .vis-item.AE { background-color: ",my_cols[3],"; }
              .vis-item.LB { background-color: ",my_cols[4],"; }
              .vis-item.MH_MH { background-color: ",my_cols[5],"; }
              .vis-item.MH_FDH { background-color: ",my_cols[6],"; }
              .vis-item.MH_DH { background-color: ",my_cols[7],"; }
              ")
# feel free to add more css strings above



jscode <- "
shinyjs.disableTab = function() {
var tabs = $('.nav').find('li:not(.active) a');
tabs.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tabs.addClass('disabled');
}
shinyjs.enableTab = function(param) {
var tab = $('.nav').find('li:not(.active) a');
tab.unbind('click.tab');
tab.removeClass('disabled');
}
"

htmljs <- "
// execute the code after the shiny session has started
$(document).on('shiny:sessioninitialized', function(event) {
// browser detection from https://stackoverflow.com/a/5918791/8099834
navigator.sayswho= (function(){
var ua= navigator.userAgent, tem, 
M= ua.match(/(opera|chrome|safari|firefox|msie|trident(?=\\/))\\/?\\s*(\\d+)/i) || [];
if(/trident/i.test(M[1])){
tem=  /\\brv[ :]+(\\d+)/g.exec(ua) || [];
return 'IE '+(tem[1] || '');
}
if(M[1]=== 'Chrome'){
tem= ua.match(/\\b(OPR|Edge)\\/(\\d+)/);
if(tem!= null) return tem.slice(1).join(' ').replace('OPR', 'Opera');
}
M= M[2]? [M[1], M[2]]: [navigator.appName, navigator.appVersion, '-?'];
if((tem= ua.match(/version\\/(\\d+)/i))!= null) M.splice(1, 1, tem[1]);
return M.join(' ');
})(); 
// pass browser info from JS to R
Shiny.onInputChange('myBrowser', navigator.sayswho); 
});"
