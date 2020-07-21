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
