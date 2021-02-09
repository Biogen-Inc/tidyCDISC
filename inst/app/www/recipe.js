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
function selectRecipeBlock(newid, df, values) { 
    return `<div class="form-group drop_area">
      <label class="control-label ${df}" for="${newid}">${newid}</label>
        <select id="${newid}" class="dropdown">
          <option value="${values}">${values}</option>
          </select>
            <button class="delete">X</button>
              </div>`
  }

// this is scanning through the array, and appends to the html above 
// and join() concatenates it all together, joining on nothing ("")
// the strg (df name) remains constant
function combineRows(arr, strg) {
  let t = []
  arr.forEach(function (e) {
    t.push(simpleRecipeRowBlock(e, strg))
  });
  t= t.join("")
  return(t)
}

// this is scanning through the
function oneAgg_AllOptions(agg_stat, df, select_input) {
  let t = []
  select_input.forEach(function (sel_input) {
    t.push(selectRecipeBlock(agg_stat, df, sel_input))
  });
  t= t.join("")
  return(t)
}


// These are called arrays
demography_rows = ["AGEGR1", "AGE",  "SEX",  "ETHNIC", "RACE", "HEIGHTBL", "WEIGHTBL"]
demography_agg =  ["FREQ",   "MEAN", "FREQ", "FREQ",   "FREQ", "MEAN",     "MEAN"]

ae18_rows = ["AOCCFL", "AESEV", "AESER","DTHDT"]
ae18_agg =  ["Y_FREQ", "MAX_FREQ", "Y_FREQ", "NON_MISSING"]

soc_pt_rows = ["AOCCFL", "AEBODSYS"]
soc_pt_agg =  ["Y_FREQ", "NESTED_FREQ_DSC"]
soc_pt_sel = ["NONE", "AEDECOD"]

weeks = Shiny.addCustomMessageHandler('my_weeks', function(x) {
  Object.values(x) // the dataframe column is imported as an array
}
adlbc_params = Shiny.addCustomMessageHandler('adlbc_params', function(x) {
  Object.values(x) // the dataframe column is imported as an array
}

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
      
      
    }  else if (publisher === "Table 20: Adverse events by system organ class and preferred term sorted by alphabetical order") {
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(simpleRecipeRowBlock("NON_MISSING", "ADAE")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_ABC", "ADAE", "AEDECOD")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(simpleRecipeRowBlock("USUBJID", "ADAE")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("AEBODSYS", "ADAE")));
      
      
    }
    else if (["Table 19: Adverse events by system organ class and preferred term sorted by decreasing frequency",
                "Table 25: Severe adverse events by system organ class and preferred term",
                "Table 29: Related adverse events by system organ class and preferred term",
                "Table 30: Serious adverse events by system organ class and preferred term",
                "Table 33: Related serious adverse events by system organ class and preferred term",
                "Table 34: Adverse events that led to discontinuation of study treatment by system organ class and preferred term",
                "Table 36: Adverse events that led to withdrawl from study by system organ class and preferred term",
                "Table 38: Adverse events that led to drug interrupted, dose reduced, or dose increased by system organ class and preferred term"].includes(publisher)) {
                  
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(simpleRecipeRowBlock("NON_MISSING", "ADAE")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_DSC", "ADAE", "AEDECOD")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(simpleRecipeRowBlock("USUBJID", "ADAE")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("AEBODSYS", "ADAE")));
      
      
      
    } else if (publisher === "Table 21: Adverse events by system organ class") {
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(simpleRecipeRowBlock("NON_MISSING", "ADAE")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_DSC", "ADAE", "NONE")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(simpleRecipeRowBlock("USUBJID", "ADAE")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("AEBODSYS", "ADAE")));
      
      
      
      
    } else if (["Table 23: Adverse events by preferred term",
                "Table 26: Severe adverse events by preferred term",
                "Table 31: Serious adverse events by preferred term"].includes(publisher)) {
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(simpleRecipeRowBlock("NON_MISSING", "ADAE")));
      $("#droppable_agg").append($(selectRecipeBlock("NESTED_FREQ_DSC", "ADAE", "NONE")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(simpleRecipeRowBlock("USUBJID", "ADAE")));
      $("#droppable_blocks").append($(simpleRecipeRowBlock("AEDECOD", "ADAE")));
      
     } else if (["Table 41: Blood Chemistry actual values by visit"].includes(publisher)) {
      document.getElementById("droppable_agg").innerHTML = "";
      $("#droppable_agg").append($(combineRows(demography_agg, "ADSL")));
      document.getElementById("droppable_blocks").innerHTML = "";
      $("#droppable_blocks").append($(combineRows(demography_rows, "ADSL")));
      
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