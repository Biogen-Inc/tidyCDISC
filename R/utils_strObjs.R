

#' my_cols
#'
#' Grab a quick color pallette
#' 
#' @noRd
my_cols <- c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9",
             "#FFF2AE", "#F1E2CC", "#CCCCCC")
# don't want to add another import to package, so doing this manually
# my_cols <- RColorBrewer::brewer.pal(8,"Pastel2")

css <- paste0("
              .nav li a.disabled {
              background-color: #aaa !important;
              color: #333 !important;
              cursor: not-allowed !important;
              border-color: #aaa !important;
              }
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
