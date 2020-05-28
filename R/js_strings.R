accordion_js <- "
shinyjs.accordion_function = function() {
$(document).on('click', '.accordion', function(){
$('.accordion-panel').hide();//Hide accordions 
/* Slide accordion panel down onclick */
$('.accordion').click(function(){
/* Select only the accordion clicked on */
$(this).parent().find('.fa-chevron-down').toggleClass('up'); 
$(this).parent().find('.accordion-panel').slideToggle('slow');
panelCheck();
});
/* Checks if all panels are closed */
function panelCheck(){
/* Panels are all hidden */
if ($('.fa-chevron-down').hasClass('up') == false) {
$('.control').text('Collapse All');
}else{
$('.control').text('Hide All');
}
}
});
}
"


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