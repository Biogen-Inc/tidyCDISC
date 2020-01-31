/* Function to create list of row blocks */
function recipeRowBlock(text) {
  return (`
  <div><div><div class='form-group drop_area'>
  <label class='control-label' for='demography_recipe'>${text}</label>
  <button class='delete'>Delete</button>
  </div></div></div>`)
}

function combineRows(arr) {
  let t = []
  arr.forEach(function (e) {
    t.push(recipeRowBlock(e))
  });
  t= t.join("")
  return(t)
}

demography_rows = ["AGE", "AGEGRN", "SEX", "RACE", "HEIGHTBL", "WEIGHTBL", "BMIBL", "COUNTRY"]
demography_agg = ["Mean", "FREQ", "FREQ", "FREQ", "Mean", "Mean", "Mean", "FREQ"]

/* Create custom block recipes to automatically populate when selected */
  $("#RECIPE").bind("change", function(event, ui) {
    var publisher = $("#RECIPE").val();
    if (publisher === "DEMOGRAPHY") {
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(combineRows(demography_agg)));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(demography_rows)));
    } else {
      document.getElementById("droppable_agg").innerHTML = "";
      document.getElementById("droppable_blocks").innerHTML = "";
    }
  });