// code copied from Eric Millman to use motomo

var _paq = window._paq || [];
/* tracker methods like "setCustomDimension" should be called before "trackPageView" */
  _paq.push(["setDocumentTitle", document.domain + "/" + document.title]);
_paq.push(['trackPageView']);
_paq.push(['enableLinkTracking']);
(function() {
  var u="https://adspiwik2.abv.amazon.biogen.com/matomo/"; 
  _paq.push(['setTrackerUrl', u+'matomo.php']);
  _paq.push(['setSiteId', '7']);
  var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
  g.type='text/javascript'; g.async=true; g.defer=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
})();

