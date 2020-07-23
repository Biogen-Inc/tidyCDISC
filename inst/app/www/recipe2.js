$(document).ready(function(){
  $(document).on('click', '#RECIPE', function(){
/* Function to create list of row blocks */
function recipeRowBlock(newid) {
  return `
  <div><div><div class="form-group drop_area">
  <label class="control-label ADSL" for="${newid}">${newid}</label>
  <button class="delete">X</button>
  </div></div></div>`
}

function combineRows(arr) {
  let t = []
  arr.forEach(function (e) {
    t.push(recipeRowBlock(e))
  });
  t= t.join("")
  return(t)
}

demography_rows = ["AGE", "SEX", "RACE", "HEIGHTBL", "WEIGHTBL", "BMIBL", "ETHNIC"]
demography_agg = ["MEAN", "FREQ", "FREQ", "MEAN", "MEAN", "MEAN", "FREQ"]

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
  
$('select#RECIPE').change(function() {
  var selectedDropdown = $(this).children('option:selected').val()
  Shiny.setInputValue('tableGen_ui_1-recipe', selectedDropdown)
})

});
});