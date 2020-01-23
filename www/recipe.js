/* Create custom block recipes to automatically populate when selected */
  $("#RECIPE").bind("change", function(event, ui) {
    var publisher = $("#RECIPE").val();
    if (publisher === "MEAN") {
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($("<div><div><div class='form-group drop_area'><label class='control-label' for='mean_recipe'>MEAN</label><button class='delete'>Delete</button></div></div></div><div><div><div class='form-group drop_area'><label class='control-label' for='freq_recipe'>FREQ</label><button class='delete'>Delete</button></div></div></div>"));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($("<div><div><div class='form-group drop_area'><label class='control-label' for='A'>A</label><button class='delete'>Delete</button></div></div></div><div><div><div class='form-group drop_area'><label class='control-label' for='B'>B</label><button class='delete'>Delete</button></div></div></div>"));
    } else {
      document.getElementById("droppable_agg").innerHTML = "";
      document.getElementById("droppable_blocks").innerHTML = "";
    }
  });