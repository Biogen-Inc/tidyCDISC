// this code adds a class "up" to the row palletes
// and we use that class to turn the arrow up and down
// and also collapse the block divs

$( document ).ready(function() {
  $(document).on('click', '.accordion', function(){
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
