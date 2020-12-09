$(document).ready(function(){
  $(document).on('click', '#RECIPE', function(){
/* Function to create list of row blocks. If we need a block with a text String
or a dropdown, make a new function here */
function simpleRecipeRowBlock(newid, df) {
  return `
  <div><div><div class="form-group drop_area">
  <label class="control-label ${df}" for="${newid}">${newid}</label>
  <button class="delete">X</button>
  </div></div></div>`
}


// this is scanning through the array, and appends another thing (the html above) 
// and join() concatenates it all together, joining on nothing. strg remains constant
function combineRows(arr, strg) {
  let t = []
  arr.forEach(function (e) {
    t.push(simpleRecipeRowBlock(e, strg))
  });
  t= t.join("")
  return(t)
}

// These are called arrays
demography_rows = ["AGEGR1", "AGE",  "SEX",  "ETHNIC", "RACE", "HEIGHTBL", "WEIGHTBL"]
demography_agg =  ["FREQ",   "MEAN", "FREQ", "FREQ",   "FREQ", "MEAN",     "MEAN"]

ae18_rows = ["AOCCFL", "AESEV", "AESER","DTHDT"]
ae18_agg =  ["Y_FREQ", "MAX_FREQ", "Y_FREQ", "NON_MISSING"]

ae18_rows = ["AEBODSYS"]
ae18_agg =  ["NESTED_FREQ"]

/* Create custom block recipes to automatically populate when selected */
  $("#RECIPE").bind("change", function(event, ui) {
    var publisher = $("#RECIPE").val();
    if (publisher === "Table 5: Demography") {
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(combineRows(demography_agg, "ADSL")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(demography_rows, "ADSL")));
      
    } else if (publisher === "Table 18: Overall summary of adverse events") {
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(combineRows(ae18_agg, "ADAE")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(ae18_rows, "ADAE")));
      
    } else if (publisher === "Table 19: Adverse events by system organ class and preferred term") {
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(combineRows(ae19_agg, "ADAE")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(ae19_rows, "ADAE")));
      
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