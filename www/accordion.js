console.log("test")

$(document).ready(function(){
  $(".accordion-panel").hide();//Hide accordions 

  /* Show all/Hide all accordions onclick */
  $(".control").click(function(){
    $(".accordion-panel").stop();//Stops user from spamming button
    /* Toggle text in button and panel */
    if ($(".control").text() == "Collapse All") {
      $(".control").text("Hide All");
      $(".accordion-panel").slideDown("slow");//Show all panels
      $(".fa-chevron-down").addClass("up"); 
    }else {
      $(".control").text("Collapse All");
      $(".accordion-panel").slideUp("slow");//Hide all panels
      $(".fa-chevron-down").removeClass("up"); 

    }
  });

  /* Slide accordion panel down onclick */
  $(".accordion").click(function(){
    /* Select only the accordion clicked on */
    $(this).parent().find(".fa-chevron-down").toggleClass("up"); 
    $(this).parent().find(".accordion-panel").slideToggle("slow");
    panelCheck();
  });

  /* Checks if all panels are closed */
  function panelCheck(){
    /* Panels are all hidden */
    if ($('.fa-chevron-down').hasClass("up") == false) {
      $(".control").text("Collapse All");
    }else{
      $(".control").text("Hide All");
    }
  }

});